<?php declare(strict_types=1);

namespace IiifSearch\Job;

use DateTime;
use DOMDocument;
use Exception;
use Omeka\Api\Representation\AbstractResourceEntityRepresentation;
use Omeka\Api\Representation\ItemRepresentation;
use Omeka\Api\Representation\MediaRepresentation;
use Omeka\File\TempFile;
use Omeka\Job\AbstractJob;
use Omeka\Stdlib\Message;
use SimpleXMLElement;
use XSLTProcessor;

class ExtractOcr extends AbstractJob
{
    const FORMAT_ALTO = 'application/alto+xml';
    const FORMAT_PDF2XML = 'application/vnd.pdf2xml+xml';
    const FORMAT_TSV = 'text/tab-separated-values';
    const FORMAT_TSV_BY_WORD = 'text/tab-separated-values;by-word';

    /**
     * @var \Omeka\Api\Manager
     */
    protected $api;

    /**
     * @var \Omeka\Stdlib\Cli
     */
    protected $cli;

    /**
     * @var \Doctrine\ORM\EntityManager
     */
    protected $entityManager;

    /**
     * @var \IiifSearch\View\Helper\FixUtf8|null
     */
    protected $fixUtf8;

    /**
     * @var \Laminas\Log\Logger
     */
    protected $logger;

    /**
     * @var \Omeka\File\TempFileFactory
     */
    protected $tempFileFactory;

    /**
     * @var string
     */
    protected $basePath;

    /**
     * @var string
     */
    protected $baseUri;

    /**
     * @var bool
     */
    protected $createEmptyFile;

    /**
     * @var bool
     */
    protected $contentOnly = false;

    /**
     * @var bool
     */
    protected $createMedia;

    /**
     * @var string
     */
    protected $language;

    /**
     * @var int|null
     */
    protected $propertyId;

    /**
     * @var string|null
     */
    protected $propertyTerm;

    /**
     * @var string
     */
    protected $targetDirPath;

    /**
     * @var string
     */
    protected $targetExtension;

    /**
     * @var string
     */
    protected $targetFormat;

    /**
     * @var string
     */
    protected $targetMediaType;

    /**
     * @var array
     */
    protected $dataPdf;

    /**
     * @var array
     */
    protected $store = [
        'item' => false,
        'media_pdf' => false,
        'media_extracted' => false,
    ];

    /**
     * @var array
     */
    protected $stats = [];

    /**
     * @var \Transliterator|false|null
     */
    protected $transliterator;

    /**
     * @brief Attach attracted ocr data from pdf with item
     */
    public function perform(): void
    {
        $services = $this->getServiceLocator();
        $helpers = $services->get('ViewHelperManager');
        $this->api = $services->get('Omeka\ApiManager');
        $this->fixUtf8 = $helpers->has('FixUtf8') ? $helpers->get('FixUtf8') : null;
        $this->logger = $services->get('Omeka\Logger');
        $referenceIdProcessor = new \Laminas\Log\Processor\ReferenceId();
        $referenceIdProcessor->setReferenceId('extract-ocr/extract-ocr/job_' . $this->job->getId());
        $this->logger->addProcessor($referenceIdProcessor);
        $this->tempFileFactory = $services->get('Omeka\File\TempFileFactory');
        $this->cli = $services->get('Omeka\Cli');
        $this->entityManager = $services->get('Omeka\EntityManager');
        $this->baseUri = $this->getArg('base_uri');
        $this->basePath = $services->get('Config')['file_store']['local']['base_path'] ?: (OMEKA_PATH . '/files');

        if ((int) shell_exec('hash pdftotext 2>&- || echo 1')) {
            $this->job->setStatus(\Omeka\Entity\Job::STATUS_ERROR);
            $this->logger->err(
                'The command-line utility pdftotext is not available. Install the package poppler-utils.' //@translate
            );
            return;
        }

        if ((int) shell_exec('hash pdftohtml 2>&- || echo 1')) {
            $this->job->setStatus(\Omeka\Entity\Job::STATUS_ERROR);
            $this->logger->err(
                'The command-line utility pdftohtml is not available. Install the package poppler-utils.' //@translate
            );
            return;
        }

        if (!$this->checkDestinationDir($this->basePath . '/temp')) {
            $this->job->setStatus(\Omeka\Entity\Job::STATUS_ERROR);
            $this->logger->err(
                'The temporary directory "files/temp" is not writeable. Fix rights or create it manually.' // @translate
            );
            return;
        }

        if (!$this->baseUri) {
            $this->job->setStatus(\Omeka\Entity\Job::STATUS_ERROR);
            $this->logger->err(
                'The base uri is unknown.' // @translate
            );
            return;
        }

        // Clean and reorder media types to extract tsv, then pdf2xml then alto
        // to simplify storage of text as value.
        // Indeed, the text is not extracted directly for format alto.
        $formats = [
            self::FORMAT_TSV => 'tsv',
            self::FORMAT_TSV_BY_WORD => 'tsv by word',
            self::FORMAT_PDF2XML => 'pdf2xml',
            self::FORMAT_ALTO => 'alto',
        ];
        $dirPaths = [
            self::FORMAT_ALTO => 'alto',
            self::FORMAT_PDF2XML => 'pdf2xml',
            self::FORMAT_TSV => 'iiif-search',
            self::FORMAT_TSV_BY_WORD => 'iiif-search',
        ];
        $extensions = [
            self::FORMAT_ALTO => 'alto.xml',
            self::FORMAT_PDF2XML => 'pdf2xml.xml',
            self::FORMAT_TSV => 'full.tsv',
            self::FORMAT_TSV_BY_WORD => 'by-word.tsv',
        ];
        $mediaTypes = [
            self::FORMAT_ALTO => 'application/alto+xml',
            self::FORMAT_PDF2XML => 'application/vnd.pdf2xml+xml',
            self::FORMAT_TSV => 'text/tab-separated-values',
            self::FORMAT_TSV_BY_WORD => 'text/tab-separated-values',
        ];

        $settings = $services->get('Omeka\Settings');

        // Prepare and reorder types.
        $targetTypesFiles = $settings->get('iiifsearch_extract_types_files') ?: [];
        $targetTypesFiles = array_values(array_intersect(array_keys($formats), $targetTypesFiles));
        $targetTypesMedia = $settings->get('iiifsearch_extract_types_media') ?: [];
        $targetTypesMedia = array_values(array_intersect(array_keys($formats), $targetTypesMedia));
        $targetContentStore = $settings->get('iiifsearch_extract_content_store') ?: [];
        $targetContentStore = array_intersect($targetContentStore, ['item', 'media_pdf', 'media_extracted']);
        if (!count($targetTypesFiles) && !count($targetTypesMedia) && !count($targetContentStore)) {
            $this->logger->warn(
                'No extract format to process.' // @translate
            );
            return;
        }

        if ((in_array(self::FORMAT_ALTO, $targetTypesFiles) || in_array(self::FORMAT_ALTO, $targetTypesMedia))
            && !class_exists('XSLTProcessor', false)
        ) {
            $this->job->setStatus(\Omeka\Entity\Job::STATUS_ERROR);
            $this->logger->err(
                'The php extension "xml" or "xsl" is required to extract text as xml alto.' // @translate
            );
            return;
        }

        foreach ($targetTypesFiles as $format) {
            if (!$this->checkDestinationDir($this->basePath . '/' . $dirPaths[$format])) {
               $this->job->setStatus(\Omeka\Entity\Job::STATUS_ERROR);
            }
        }
        if ($this->job->getStatus() === \Omeka\Entity\Job::STATUS_ERROR) {
            $this->logger->err('The directory is not writeable.'); // @translate
            return;
        }

        $mode = $this->getArg('mode') ?: 'all';
        $itemId = (int) $this->getArg('item_id');
        $itemIds = (string) $this->getArg('item_ids');
        if ($itemId) {
            $itemIds = trim($itemId . ' ' . $itemIds);
        }

        // TODO Manage the case where there are multiple pdf by item (rare).

        $contentStore = &$targetContentStore;
        if ($contentStore) {
            $prop = $settings->get('iiifsearch_extract_content_property');
            if ($prop) {
                /** @var \Common\Stdlib\EasyMeta $easyMeta */
                $easyMeta = $services->get('Common\EasyMeta');
                $this->propertyId = $easyMeta->propertyId($prop);
                if ($this->propertyId) {
                    $this->propertyTerm = $prop;
                    $this->language = $settings->get('iiifsearch_extract_content_language');
                    $this->store['item'] = in_array('item', $contentStore) && !$this->getArg('manual');
                    $this->store['media_pdf'] = in_array('media_pdf', $contentStore);
                    $this->store['media_extracted'] = in_array('media_extracted', $contentStore);
                }
            }
            if (!$this->propertyId) {
                $this->logger->warn(
                    'The option to store text is set, but no property is defined.' // @translate
                );
            }
        }

        $this->createEmptyFile = (bool) $settings->get('iiifsearch_extract_create_empty_file');

        // It's not possible to search multiple item ids, so use the connection.
        // SInce the job can be sent only by an admin, there is no rights issue.

        /*
        // TODO The media type can be non-standard for pdf (text/pdf…) on very old servers.
        $query = [
            'media_type' => 'application/pdf',
            'extension' => 'pdf',
        ];
        if ($itemId) {
            $query['item_id'] = $itemId;
        }
        $response = $this->api->search('media', $query, ['returnScalar' => 'id']);
        $pdfMediaIds = $response->getContent();
        $totalToProcess = count($pdfMediaIds);
        */

        /** @var \Doctrine\ORM\EntityManager $entityManager */
        /*
        $entityManager = $services->get('Omeka\EntityManager');
        $mediaRepository = $entityManager->getRepository(\Omeka\Entity\Media::class);
        $criteria = Criteria::create();
        $expr = $criteria->expr();
        $criteria
            ->andWhere($expr->in('media_type', ['application/pdf', 'text/pdf']))
            ->andWhere($expr->eq('extension', 'pdf'))
            ->orderBy(['id' => 'ASC']);
        if ($itemIds) {
            $range = $this->exprRange('item', $itemIds);
            if ($range) {
                $criteria->andWhere($expr->orX(...$range));
            }
        }
        $collection = $mediaRepository->matching($criteria);
        $totalToProcess = $collection->count();
        */

        /** @var \Doctrine\DBAL\Connection $connection */
        $connection = $services->get('Omeka\Connection');
        /*
        $sql = <<<'SQL'
            SELECT id
            FROM `media`
            WHERE `media_type`IN (:media_type)
                AND `extension`= :extension
            GROUP BY item_id
            SQL;
        */
        // Process only the first pdf of each item.
        $sql = <<<'SQL'
            SELECT `id`
            FROM `media`
            WHERE `position` = (
                SELECT MIN(`position`)
                FROM `media` AS sub
                WHERE `sub`.`item_id` = `media`.`item_id`
                    AND `media_type`IN (:media_type)
                    AND `extension`= :extension
            )
            SQL;
        $bind = [
            'media_type' => ['application/pdf', 'text/pdf'],
            'extension' => 'pdf',
        ];
        $types = [
            'media_type' => \Doctrine\DBAL\Connection::PARAM_STR_ARRAY,
            'extension' => \Doctrine\DBAL\ParameterType::STRING,
        ];
        if ($itemIds) {
            $range = $this->exprRange('item_id', $itemIds);
            if ($range) {
                $sql .= ' AND ((' . implode(') OR (', $range) . '))';
            }
        }
        $sql .= <<<'SQL'
            GROUP BY `item_id`, `position`, `id`
            ORDER BY `item_id` ASC;
            SQL;
        $pdfMediaIds = $connection->executeQuery($sql, $bind, $types)->fetchFirstColumn();
        $totalToProcess = count($pdfMediaIds);

        if (empty($totalToProcess)) {
            $message = new Message('No item with a pdf to process.'); // @translate
            $this->logger->notice($message);
            return;
        }

        if (count($targetTypesMedia)) {
            $this->logger->info(new Message(
                'Formats of xml files to create: %s.', // @translate,
                implode(', ', array_intersect_key($formats, array_flip($targetTypesMedia)))
           ));
        }

        if ($mode === 'existing') {
            $this->logger->info(new Message(
                'Creating Extract OCR files for %d PDF only if they already exist.', // @translate
                $totalToProcess
            ));
        } elseif ($mode === 'missing') {
            $this->logger->info(new Message(
                'Creating Extract OCR files for %d PDF, only if they do not exist yet.', // @translate
                $totalToProcess
            ));
        } elseif ($mode === 'all') {
            $this->logger->info(new Message(
                'Creating Extract OCR files for %d PDF, xml files will be overridden or created.', // @translate
                $totalToProcess
            ));
        } else {
            $this->job->setStatus(\Omeka\Entity\Job::STATUS_ERROR);
            $this->logger->err(new Message(
                'Mode of extraction "%s" is not managed.', // @translate
                $mode
            ));
            return;
        }

        // TODO Currently, the process create the files via a loop by media type. Restructure it to do it one time by item.

        // Create a single table to process a single loop.
        // Most of the time, there are one or two formats.
        $create = [];
        foreach ($targetTypesFiles as $targetType) {
            $create[] = ['format' => $targetType, 'create_media' => false];
        }
        foreach ($targetTypesMedia as $targetType) {
            $create[] = ['format' => $targetType, 'create_media' => true];
        }
        // Allow to store extracted text without creating files or media.
        if (empty($create) && $contentStore) {
            $create[] = ['format' => self::FORMAT_PDF2XML, 'create_media' => false, 'content_only' => true];
        }
        foreach ($create as $key => $targetData) {
            $this->createMedia = $targetData['create_media'];
            $this->contentOnly = !empty($targetData['content_only']);
            $this->targetFormat = $targetData['format'];
            $this->targetDirPath = $dirPaths[$this->targetFormat];
            $this->targetExtension = $extensions[$this->targetFormat];
            $this->targetMediaType = $mediaTypes[$this->targetFormat];
            if (count($targetTypesMedia) > 1) {
                $this->logger->notice(new Message(
                    'Processing format %1$d/%2$d: %3$s (%4$s).', // @translate
                    $key + 1, count($targetTypesMedia), $formats[$this->targetFormat], $this->targetMediaType
                ));
            }
            if ($key > 0) {
                $this->store = [
                    'item' => false,
                    'media_pdf' => false,
                    'media_extracted' => false,
                ];
                $this->propertyId = null;
                $this->propertyTerm = null;
            }
            $this->process($pdfMediaIds, $mode, $totalToProcess);
            if ($this->shouldStop()) {
                // The message is already displayed.
                return;
            }
        }
        if (count($targetTypesMedia) > 1) {
            $this->logger->notice(
                'End of processing formats.', // @translate
            );
        }
    }

    protected function process($pdfMediaIds, $mode, $totalToProcess)
    {
        $countPdf = 0;
        $countSkipped = 0;
        $countFailed = 0;
        $countProcessed = 0;
        $this->stats = [
            'no_pdf' => [],
            'no_text_layer' => [],
            'issue' => [],
        ];

        // Create one xml by item, so don't manage multiple pdf by item (rare anyway).
        $processedItems = [];

        $suffixFilenames = [
            self::FORMAT_ALTO=> '.alto',
            self::FORMAT_PDF2XML => '.pdf2xml',
            self::FORMAT_TSV => '.full',
            self::FORMAT_TSV_BY_WORD => '.by-word',
        ];
        $shortExtensions = [
            self::FORMAT_ALTO => 'xml',
            self::FORMAT_PDF2XML => 'xml',
            self::FORMAT_TSV => 'tsv',
            self::FORMAT_TSV_BY_WORD => 'tsv',
        ];

        foreach ($pdfMediaIds as $pdfMediaId) {
            if ($this->shouldStop()) {
                if ($mode === 'all') {
                    $this->logger->warn(new Message(
                        'The job "Extract OCR" was stopped: %1$d/%2$d resources processed, %3$d failed (%4$d without file, %5$d without text layer, %6$d with issue).', // @translate
                        $countProcessed, $totalToProcess, $countFailed, count($this->stats['no_pdf']), count($this->stats['no_text_layer']), count($this->stats['issue'])
                    ));
                } else {
                    $this->logger->warn(new Message(
                        'The job "Extract OCR" was stopped: %1$d/%2$d resources processed, %3$d skipped, %4$d failed (%5$d without file, %6$d without text layer, %7$d with issue).', // @translate
                        $countProcessed, $totalToProcess, $countSkipped, $countFailed, count($this->stats['no_pdf']), count($this->stats['no_text_layer']), count($this->stats['issue'])
                    ));
                }
                return;
            }

            // Step 1: Check the presence of the file/media according to mode.
            // Remove existing file/media if needed.
            // Only the file/media with the same format is removed.

            $pdfMedia = $this->api->read('media', ['id' => $pdfMediaId])->getContent();
            $item = $pdfMedia->item();
            $itemId = $item->id();
            if (isset($processedItems[$itemId])) {
                $this->logger->warn(new Message(
                    'Item #%d: only the first pdf is processed.', // @translate
                    $itemId
                ));
                continue;
            }
            $processedItems[$itemId] = true;

            // TODO Improve search of an existing file, that can be imported separatly, or that can be another xml format with the same name.
            // Search if this item has already an xml file, managing double
            // extension.
            // For security and to avoid to remove native xml, in particular
            // alto, append the item id for the base of the derivative file.
            $targetFilenameNoExtension = basename($pdfMedia->source(), '.pdf') . '.' . $item->id();
            $shortExtension = $shortExtensions[$this->targetFormat];
            $targetFilename = $targetFilenameNoExtension . $suffixFilenames[$this->targetFormat] . '.' . $shortExtension;
            $searchExistingOcrMedia = $this->getMediaFromFilename($item->id(), $targetFilename, $shortExtension, $this->targetMediaType);

            $localSearchFilepath = $this->basePath . '/' . $this->targetDirPath . '/' . $item->id() . '.' . $this->targetExtension;
            $searchExistingOcrFile = file_exists($localSearchFilepath);

            ++$countPdf;
            $this->logger->info(new Message(
                'Index #%1$d/%2$d: Extracting OCR for item #%3$d, media #%4$d "%5$s".', // @translate
                $countPdf, $totalToProcess, $item->id(), $pdfMedia->id(), $pdfMedia->source())
            );

            if ($this->contentOnly) {
                // No file or media to check, skip to text extraction.
            } elseif ($mode === 'all' || $mode === 'existing') {
                if ($searchExistingOcrFile) {
                    @unlink($localSearchFilepath);
                    $this->logger->info(new Message(
                        'The existing %1$s was removed for item #%2$d.', // @translate
                        $this->targetExtension, $item->id()
                    ));
                }
                if ($searchExistingOcrMedia) {
                    try {
                        $this->api->delete('media', $searchExistingOcrMedia->id());
                    } catch (Exception $e) {
                        // There may be a doctrine issue with module Access, but media is removed.
                    }
                    $this->logger->info(new Message(
                        'The existing %1$s was removed for item #%2$d.', // @translate
                        $this->targetExtension, $item->id()
                    ));
                }
                if ($mode === 'existing'
                    && (
                        (!$this->createMedia && !$searchExistingOcrFile)
                        || ($this->createMedia && !$searchExistingOcrMedia)
                    )
                ) {
                    ++$countSkipped;
                    continue;
                }
            }
            // Here, mode is "missing".
            elseif (!$this->createMedia && $searchExistingOcrFile) {
                $this->logger->info(new Message(
                    'A file %1$s already exists, so item #%2$d is skipped.',  // @translate
                    $this->targetExtension, $item->id()
                ));
                ++$countSkipped;
                continue;
            } elseif ($this->createMedia && $searchExistingOcrMedia) {
                $this->logger->info(new Message(
                    'A file %1$s (media #%2$d) already exists, so item #%3$d is skipped.',  // @translate
                    $this->targetExtension, $searchExistingOcrMedia->id(), $item->id()
                ));
                ++$countSkipped;
                continue;
            }

            // Step 2: Create new file/media, and store text content if needed.

            $hasOcrFile = null;
            $ocrMedia = null;
            $tempFile = $this->extractOcrFromPdfMediaToTempFile($pdfMedia);
            if ($tempFile) {
                $textContent = $this->extractTextContent($pdfMedia, $tempFile);
                if ($this->contentOnly) {
                    // No file or media to create, text stored below.
                } elseif ($this->createMedia) {
                    // Do not create is only for media.
                    $doNotCreate = !in_array($this->targetFormat, [self::FORMAT_TSV, self::FORMAT_TSV_BY_WORD])
                        && !$this->createEmptyFile
                        && !strlen($textContent);
                    if ($doNotCreate) {
                        $this->stats['no_text_layer'][] = $pdfMedia->id();
                        $this->logger->notice(new Message(
                            'The output %1$s for pdf #%2$d has no text content and is not created.', // @translate
                            $this->targetExtension, $pdfMedia->id()
                        ));
                    } else {
                        $ocrMedia = $this->storeFileInMedia($tempFile, $textContent, $pdfMedia);
                        if ($ocrMedia) {
                            $this->logger->info(new Message(
                                'Media #%1$d (item #%2$d) created for %3$s file.', // @translate
                                $ocrMedia->id(), $item->id(), $this->targetExtension
                            ));
                        }
                    }
                } else {
                    $hasOcrFile = $this->storeFileLocally($tempFile, $localSearchFilepath);
                    if ($hasOcrFile) {
                        $this->logger->info(new Message(
                            'IIIF Search file for item #%1$d created for format %2$s.', // @translate
                            $item->id(), $this->targetExtension
                        ));
                    } else {
                        $this->logger->err(new Message(
                            'Unable to store the IIIF Search file for item #%1$d for format %2$s.', // @translate
                            $item->id(), $this->targetExtension
                        ));
                    }
                }
                $tempFile->delete();

                if ($hasOcrFile || $ocrMedia || $this->contentOnly) {
                    // Text content is already stored in media ocr.
                    if ($this->store['media_pdf']) {
                        $this->storeContentInProperty($pdfMedia, $textContent);
                    }
                    if ($this->store['item']) {
                        $this->storeContentInProperty($item, $textContent);
                    }
                    ++$countProcessed;
                } else {
                    ++$countFailed;
                }
            } else {
                ++$countFailed;
            }

            // Avoid memory issue.
            unset($pdfMedia);
            unset($ocrMedia);
            unset($item);

            // Clear the Doctrine identity map to prevent memory growth
            // on large batches. Property scalars are cached above.
            $this->entityManager->clear();
        }

        if ($this->stats['no_pdf']) {
            $this->logger->notice(new Message(
                'These medias have no pdf file: #%s', // @translate
                implode(', #', $this->stats['no_pdf'])
            ));
        }

        if ($this->stats['no_text_layer']) {
            $this->logger->notice(new Message(
                'These pdf files have no text layer: #%s', // @translate
                implode(', #', $this->stats['no_text_layer'])
            ));
        }

        if ($this->stats['issue']) {
            $this->logger->notice(new Message(
                'These pdf files have issues when extracting content: #%s', // @translate
                implode(', #', $this->stats['issue'])
            ));
        }

        if ($mode === 'all') {
            $this->logger->notice(new Message(
                'Processed %1$d/%2$d pdf files, %3$d files %4$s created, %5$d failed (%6$d without file, %7$d without text layer, %8$d with issue).', // @translate
                $countPdf, $totalToProcess, $countProcessed, $this->targetExtension, $countFailed, count($this->stats['no_pdf']), count($this->stats['no_text_layer']), count($this->stats['issue'])
            ));
        } else {
            $this->logger->notice(new Message(
                'Processed %1$d/%2$d pdf files, %3$d skipped, %4$d files %5$s, created, %6$d failed (%7$d without file, %8$d without text layer, %9$d with issue).', // @translate
                $countPdf, $totalToProcess, $countSkipped, $countProcessed, $this->targetExtension, $countFailed, count($this->stats['no_pdf']), count($this->stats['no_text_layer']), count($this->stats['issue'])
            ));
        }
    }

    /**
     * Get the first media from item id, source name, extension and media type.
     *
     * @todo Improve search of ocr pdf2xml files.
     *
     * Copy:
     * @see \IiifSearch\Module::getMediaFromFilename()
     * @see \IiifSearch\Job\ExtractOcr::getMediaFromFilename()
     *
     * @param int $itemId
     * @param string $filename
     * @param string $extension
     * @param string $mediaType
     * @return \Omeka\Api\Representation\MediaRepresentation|null
     */
    protected function getMediaFromFilename(
        int $itemId,
        string $filename,
        string $extension,
        string $mediaType
    ): ?MediaRepresentation {
        // The api search() doesn't allow to search a source, so we use read().
        try {
            return $this->api->read('media', [
                'item' => $itemId,
                'source' => $filename,
                'extension' => $extension,
                'mediaType' => $mediaType,
            ])->getContent();
        } catch (\Omeka\Api\Exception\NotFoundException $e) {
            return null;
        }
    }

    protected function extractOcrFromPdfMediaToTempFile(MediaRepresentation $pdfMedia, bool $forceXml = false): ?TempFile
    {
        $pdfFilepath = $this->basePath . '/original/' . $pdfMedia->filename();
        if (!file_exists($pdfFilepath)) {
            $this->stats['no_pdf'][] = $pdfMedia->id();
            $this->logger->err(new Message(
                'Missing pdf file (media #%1$d).', // @translate
                $pdfMedia->id()
            ));
            return null;
        }

        $this->dataPdf = [
            'source_pdf_file_url' => $pdfMedia->originalUrl(),
            'source_pdf_file_name' => $pdfMedia->filename(),
            'source_pdf_file_identifier' => (string) $pdfMedia->value('dcterms:identifier') ?: '',
            'source_pdf_document_url' => $pdfMedia->item()->apiUrl(),
            'source_pdf_document_identifier' => (string) $pdfMedia->item()->value('dcterms:identifier') ?: '',
        ];

        // Do the conversion of the pdf to xml.
        $forceXmlForTsv = $forceXml
            && in_array($this->targetFormat, [self::FORMAT_TSV, self::FORMAT_TSV_BY_WORD]);
        $tempFile = $forceXmlForTsv
            // The temp file is a pdf2xml file, with extension ".pdf2xml.xml".
            ? $this->extractPdfToTempFile($pdfFilepath, $pdfMedia->item(), 'pdf2xml.xml', self::FORMAT_PDF2XML)
            : $this->extractPdfToTempFile($pdfFilepath, $pdfMedia->item(), $this->targetExtension, $this->targetFormat);

        if (empty($tempFile)) {
            $this->stats['issue'][] = $pdfMedia->id();
            $this->logger->err(new Message(
                'File %1$s was not created for media #%2$s.', // @translate
                $this->targetExtension, $pdfMedia->id()
            ));
            return null;
        }

        $tempPath = $tempFile->getTempPath();

        // A check is done when option "create empty file" is not used.
        if (!$tempPath || !file_exists($tempPath)) {
            return null;
        }

        return $tempFile;
    }

    /**
     * Extract the text content of the pdf, reusing temp file when possible.
     */
    protected function extractTextContent(MediaRepresentation $pdfMedia, ?TempFile $tempFile = null): ?string
    {
        // For tsv, text content is only needed for property storage.
        // Skip the expensive re-extraction when no property is configured.
        $isTsv = in_array($this->targetFormat, [self::FORMAT_TSV, self::FORMAT_TSV_BY_WORD]);
        if ($isTsv && !$this->propertyId) {
            return null;
        }

        $localTempFile = null;
        if ($isTsv || !$tempFile) {
            $localTempFile = $this->extractOcrFromPdfMediaToTempFile($pdfMedia, true);
            $tempFile = $localTempFile;
        }

        if (!$tempFile) {
            return null;
        }

        $tempPath = $tempFile->getTempPath();
        $xmlContent = (string) file_get_contents($tempPath);

        // Clean up the locally created temp file.
        if ($localTempFile) {
            $localTempFile->delete();
        }

        // The content can be reextracted through pdftotext, that may return a
        // different layout with options -layout or -raw.
        // Here, the text is extracted from the extracted pdf2xml.
        if ($this->targetFormat === self::FORMAT_ALTO) {
            $textContent = $this->extractTextFromAlto($xmlContent);
        } else {
            // Add a space between words.
            $textContent = trim(str_replace('  ', ' ', strip_tags( str_replace('<', ' <', $xmlContent))));
        }

        return $textContent;
    }

    /**
     * Extract text from alto.
     */
    protected function extractTextFromAlto(string $content): string
    {
        $simpleXml = simplexml_load_string($content, null, LIBXML_NONET);
        if ($simpleXml === false) {
            return '';
        }

        $modulePath = dirname(__DIR__, 2);
        $xsltPath = $modulePath . '/data/xsl/alto_to_text.xsl';
        $dom = $this->processXslt($simpleXml, $xsltPath);
        if (!$dom) {
            return '';
        }

        $dom->formatOutput = false;
        $dom->strictErrorChecking = false;
        $dom->validateOnParse = false;
        $dom->recover = true;
        $result = (string) $dom->saveHTML();
        return html_entity_decode($result, ENT_QUOTES | ENT_SUBSTITUTE | ENT_HTML5);
    }

    protected function storeFileLocally(TempFile $tempFile, string $filepath): bool
    {
        $tempFilepath = $tempFile->getTempPath();
        return copy($tempFilepath, $filepath);
    }

    protected function storeFileInMedia(
        TempFile $tempFile,
        ?string $textContent,
        MediaRepresentation $pdfMedia
    ): ?MediaRepresentation {
        // It's not possible to save a local file via the "upload" ingester. So
        // the ingester "url" can be used, but it requires the file to be in the
        // omeka files directory. Else, use module FileSideload or inject sql.
        $storeFile = $this->makeTempFileDownloadable($tempFile, '/extractocr');
        if (!$storeFile) {
            return null;
        }

        $item = $pdfMedia->item();
        $currentPosition = count($item->media());

        // This data is important to get the matching pdf and xml.
        $source = basename($pdfMedia->source(), '.pdf') . '.' . $item->id() . '.' . $this->targetExtension;

        $data = [
            'o:item' => [
                'o:id' => $item->id(),
            ],
            'o:ingester' => 'url',
            'ingest_url' => $storeFile['url'],
            'o:source' => $source,
            'o:lang' => $this->language,
            'o:media_type' => $this->targetMediaType,
            'position' => $currentPosition,
            'values_json' => '{}',
        ];

        if ($this->propertyId && strlen((string) $textContent) && $this->store['media_extracted']) {
            $data[$this->propertyTerm][] = [
                'type' => 'literal',
                'property_id' => $this->propertyId,
                '@value' => $textContent ,
                '@language' => $this->language,
            ];
            $data['dcterms:isFormatOf'][] = [
                'type' => 'resource:media',
                // dcterms:isFormatOf.
                'property_id' => 37,
                'value_resource_id' => $pdfMedia->id(),
            ];
        }

        try {
            $media = $this->api->create('media', $data)->getContent();
        } catch (\Omeka\Api\Exception\ExceptionInterface $e) {
            // Generally a bad or missing pdf file.
            $this->logger->err($e->getMessage() ?: $e);
            return null;
        } catch (Exception $e) {
            $this->logger->err($e);
            return null;
        } finally {
            @unlink($storeFile['filepath']);
        }

        if (!$media) {
            return null;
        }

        // Move the xml file as the last media to avoid thumbnails issues.
        $this->reorderMediasAndSetType($media);
        return $media;
    }

    /**
     * Extract and store OCR Data from pdf in .xml or .tsv file.
     */
    protected function extractPdfToTempFile(
        string $pdfFilepath,
        ItemRepresentation $item,
        string $extension,
        string $format
    ): ?TempFile {
        $tempFile = $this->tempFileFactory->build();

        $tempFilepath = $tempFile->getTempPath() . '.' . $extension;
        @unlink($tempFile->getTempPath());
        $tempFile->setTempPath($tempFilepath);
        $tempPath = $tempFile->getTempPath();

        if (in_array($format, [self::FORMAT_TSV, self::FORMAT_TSV_BY_WORD])) {
            $result = $this->extractTextToTsv($pdfFilepath, $tempFilepath, $item, $format);
            if (!$result) {
                if ($tempPath && file_exists($tempPath)) {
                    $tempFile->delete();
                }
                return null;
            }
            return $tempFile;
        }

        $command = sprintf('pdftohtml -i -c -hidden -nodrm -enc "UTF-8" -xml %1$s %2$s',
            escapeshellarg($pdfFilepath), escapeshellarg($tempFilepath));

        $result = $this->cli->execute($command);
        if ($result === false || !file_exists($tempFilepath) || !filesize($tempFilepath)) {
            if ($tempPath && file_exists($tempPath)) {
                $tempFile->delete();
            }
            return null;
        }

        // Remove control characters from bad ocr.
        /** @see https://stackoverflow.com/questions/1497885/remove-control-characters-from-php-string */
        $xmlContent = file_get_contents($tempFilepath);
        $xmlContent = preg_replace('/[^\PCc^\PCn^\PCs]/u', '', $xmlContent);

        if ($this->fixUtf8) {
            $xmlContent = $this->fixUtf8->__invoke($xmlContent);
        }

        $xmlContent = $this->fixXmlPdf2Xml($xmlContent);
        if (!$xmlContent) {
            if ($tempPath && file_exists($tempPath)) {
                $tempFile->delete();
            }
            return null;
        }

        $simpleXml = $this->fixXmlDom($xmlContent);
        if (!$simpleXml) {
            if ($tempPath && file_exists($tempPath)) {
                $tempFile->delete();
            }
            return null;
        }

        $simpleXml->saveXML($tempFilepath);

        if ($format === self::FORMAT_ALTO) {
            /** @see https://gitlab.freedesktop.org/poppler/poppler/-/raw/master/utils/pdf2xml.dtd pdf2xml */
            $modulePath = dirname(__DIR__, 2);
            $xsltPath = $modulePath . '/data/xsl/pdf2xml_to_alto.xsl';
            $args = $this->dataPdf;
            $args['datetime'] = (new DateTime('now'))->format('Y-m-d\TH:i:s');
            $dom = $this->processXslt($simpleXml, $xsltPath, $args);
            if (!$dom) {
                $tempFile->delete();
                return null;
            }
            $dom->formatOutput = true;
            $dom->strictErrorChecking = false;
            $dom->validateOnParse = false;
            $dom->recover = true;
            $dom->preserveWhiteSpace = false;
            $result = $dom->save($tempFilepath);
            if (!$result) {
                if ($tempPath && file_exists($tempPath)) {
                    $tempFile->delete();
                }
                return null;
            }
        }

        return $tempFile;
    }

    protected function extractTextToTsv($pdfFilepath, $tsvFilepath, ItemRepresentation $item, $format) : bool
    {
        $listMediaImages = $this->listMediaImagesData($item);

        // Create temp file that will be removed at the end of the method.
        $tempFile = $this->tempFileFactory->build();
        $xmlFilepath = $tempFile->getTempPath() . 'pdf2xml.xml';
        @unlink($tempFile->getTempPath());
        $tempFile->setTempPath($xmlFilepath);
        $tempPath = $tempFile->getTempPath();

        $command = sprintf('pdftotext -bbox -layout %1$s %2$s',
            escapeshellarg($pdfFilepath), escapeshellarg($xmlFilepath));

        $result = $this->cli->execute($command);
        if ($result === false) {
            if ($tempPath && file_exists($tempPath)) {
                $tempFile->delete();
            }
            return false;
        }

        // Remove control characters from bad ocr.
        /** @see https://stackoverflow.com/questions/1497885/remove-control-characters-from-php-string */
        $content = file_get_contents($xmlFilepath);
        $content = preg_replace('/[^\PCc^\PCn^\PCs]/u', '', $content);
        $xml = simplexml_load_string($content, null,
            LIBXML_BIGLINES
            | LIBXML_COMPACT
            | LIBXML_NOBLANKS
            | LIBXML_PARSEHUGE
            // Avoid issue and security when network is unavailable.
            | LIBXML_NONET
            // | LIBXML_NOCDATA
            // | LIBXML_NOENT
        );

        if ($xml === false) {
            $tempFile->delete();
            return false;
        }

        $hasRow = false;
        $resultTsv = [];
        $indexXmlPage = 0;

        $isFullTsv = $format === self::FORMAT_TSV;

        // For full tsv, save each row one by one.
        if ($isFullTsv) {
            $fp = fopen($tsvFilepath, 'w');
            if ($fp === false) {
                $this->logger->err(new Message(
                    'Unable to open file "%s" for writing.', // @translate
                    $tsvFilepath
                ));
                $tempFile->delete();
                return false;
            }
        }

        foreach ($xml->body->doc->page ?? [] as $xmlPage) {
            ++$indexXmlPage;

            $pageAttribute = $xmlPage->attributes();
            $pageWidth = (float) $pageAttribute->width;
            $pageHeigth = (float) $pageAttribute->height;
            if (!$pageWidth || !$pageHeigth) {
                continue;
            }

            // There may be no media when there is only a single pdf without image.
            $mediaImage = $listMediaImages[$indexXmlPage - 1] ?? null;
            $mediaImageWidth = $mediaImage ? $mediaImage['width'] : $pageWidth;
            $mediaImageHeight = $mediaImage ? $mediaImage['height'] : $pageHeigth;
            $scaleX = $mediaImageWidth / $pageWidth;
            $scaleY = $mediaImageHeight / $pageHeigth;

            foreach ($xmlPage->word ?? [] as $xmlword) {
                $word = (string) $xmlword;
                $word = $this->normalize($word);
                if (!strlen($word)) {
                    continue;
                }

                $attributes = $xmlword->attributes();

                $xMax = $attributes->xMax;
                $yMax = $attributes->yMax;
                $xMin = $attributes->xMin;
                $yMin = $attributes->yMin;

                $xMax = $xMax * $scaleX;
                $yMax = $yMax * $scaleY;
                $xMin = $xMin * $scaleX;
                $yMin = $yMin * $scaleY;

                $width = round($xMax - $xMin);
                $height = round($yMax - $yMin);

                $xywh = round((float) $xMin) . ',' . round((float) $yMin) . ',' . $width . ',' . $height;

                if ($isFullTsv) {
                    $row = [$word, $indexXmlPage, $xywh];
                    fputcsv($fp, $row, "\t", "\0", "\0");
                    $hasRow = true;
                } else {
                    $word = mb_strtolower($word, 'UTF-8');
                    $resultTsv[$word][] = $indexXmlPage . ':' . $xywh;
                }
            }
        }

        if ($isFullTsv) {
            $result = fclose($fp);
            $tempFile->delete();
            if (!$hasRow && !$this->createEmptyFile) {
                @unlink($tsvFilepath);
                return true;
            }
            return $result;
        }

        if (!$resultTsv && !$this->createEmptyFile) {
            $tempFile->delete();
            return true;
        }

        $fp = fopen($tsvFilepath, 'w');
        if ($fp === false) {
            $this->logger->err(new Message(
                'Unable to open file "%s" for writing.', // @translate
                $tsvFilepath
            ));
            $tempFile->delete();
            return false;
        }
        foreach ($resultTsv as $word => $positions) {
            fputcsv($fp, [$word, implode(';', $positions)], "\t", "\0", "\0");
        }

        $tempFile->delete();

        return fclose($fp);
    }

    /**
     * Check if xml is valid.
     *
     * Copy in:
     * @see \IiifSearch\Job\ExtractOcr::fixXmlDom()
     * @see \IiifSearch\View\Helper\IiifSearch::fixXmlDom()
     * @see \IiifSearch\View\Helper\XmlAltoSingle::fixXmlDom()
     * @see \IiifServer\Iiif\TraitXml::fixXmlDom()
     */
    protected function fixXmlDom(string $xmlContent): ?SimpleXMLElement
    {
        libxml_use_internal_errors(true);

        $dom = new DOMDocument('1.1', 'UTF-8');
        $dom->strictErrorChecking = false;
        $dom->validateOnParse = false;
        $dom->recover = true;
        try {
            $result = $dom->loadXML($xmlContent, LIBXML_NONET);
            $result = $result ? simplexml_import_dom($dom) : null;
        } catch (Exception $e) {
            $result = null;
        }

        libxml_clear_errors();
        libxml_use_internal_errors(false);

        return $result;
    }

    /**
     * Copy in:
     * @see \IiifSearch\Job\ExtractOcr::fixXmlPdf2Xml()
     * @see \IiifSearch\View\Helper\IiifSearch::fixXmlPdf2Xml()
     * @see \IiifServer\Iiif\TraitXml::fixXmlPdf2Xml()
     */
    protected function fixXmlPdf2Xml(?string $xmlContent): string
    {
        if (!$xmlContent) {
            return (string) $xmlContent;
        }

        // When the content is not a valid unicode text, a null is output.
        // Replace all series of spaces by a single space.
        $xmlContent = preg_replace('~\s{2,}~S', ' ', $xmlContent) ?? $xmlContent;
        // Remove bold and italic.
        $xmlContent = preg_replace('~</?[bi]>~S', '', $xmlContent) ?? $xmlContent;
        // Remove fontspecs, useless for search and sometime incorrect with old
        // versions of pdftohtml. Exemple with pdftohtml 0.71 (debian 10):
        // <fontspec id="^C
        // <fontspec id=" " size="^P" family="PBPMTB+ArialUnicodeMS" color="#000000"/>
        /*
        if (preg_match('~<fontspec id=".*>$~S', '', $xmlContent)) {
            $xmlContent = preg_replace('~<fontspec id=".*>$~S', '', $xmlContent) ?? $xmlContent;
        }
        */
        // Keep incomplete font specs in order to keep order of font ids.
        $xmlContent = preg_replace('~<fontspec id="[^>]*$~S', '<fontspec/>*\n', $xmlContent) ?? $xmlContent;
        $xmlContent = str_replace('<!doctype pdf2xml system "pdf2xml.dtd">', '<!DOCTYPE pdf2xml SYSTEM "pdf2xml.dtd">', $xmlContent);
        return $xmlContent;
    }

    protected function processXslt(SimpleXMLElement $simpleXml, string $xsltPath, array $params = []): ?DOMDocument
    {
        try {
            $domXml = dom_import_simplexml($simpleXml);
            $domXsl = new DOMDocument('1.1', 'UTF-8');
            $domXsl->load($xsltPath, LIBXML_NONET);
            $proc = new XSLTProcessor();
            $proc->setSecurityPrefs(XSL_SECPREF_CREATE_DIRECTORY | XSL_SECPREF_WRITE_FILE | XSL_SECPREF_READ_NETWORK | XSL_SECPREF_WRITE_NETWORK);
            $proc->importStyleSheet($domXsl);
            $proc->setParameter('', $params);
            return $proc->transformToDoc($domXml) ?: null;
        } catch (Exception $e) {
            return null;
        }
    }

    /**
     * Append the content text to a resource.
     *
     * A check is done to avoid to duplicate content.
     */
    protected function storeContentInProperty(AbstractResourceEntityRepresentation $resource, ?string $textContent): void
    {
        if ($textContent === null || $textContent === '') {
            return;
        }

        $contentValue = [
            'type' => 'literal',
            'property_id' => $this->propertyId,
            '@value' => $textContent ,
            '@language' => $this->language,
        ];

        $existingValues = $resource->value($this->propertyTerm, ['all' => true]);
        foreach ($existingValues as $v) {
            if ($v->value() === $contentValue['@value']) {
                return;
            }
        }

        // TODO Check it is working only with job, not the manual edition.
        // With append, there is no need to pass all property values.
        $resourceJson = [$this->propertyTerm => [$contentValue]];

        $this->api->update(
            $resource->resourceName(),
            $resource->id(),
            $resourceJson,
            [],
            ['isPartial' => true, 'collectionAction' => 'append']
        );
    }

    /**
     * Move a media at the last position of the item.
     *
     * @see \CSVImport\Job\Import::reorderMedias()
     *
     * @todo Move this process in the core.
     */
    protected function reorderMediasAndSetType(MediaRepresentation $media): void
    {
        // Note: the position is not available in representation.

        $entityManager = $this->entityManager;
        $mediaRepository = $entityManager->getRepository(\Omeka\Entity\Media::class);
        $medias = $mediaRepository->findBy(['item' => $media->item()->id()]);
        if (count($medias) <= 1) {
            return;
        }

        $lastMedia = null;
        $lastMediaId = (int) $media->id();
        $key = 0;
        foreach ($medias as $itemMedia) {
            $itemMediaId = (int) $itemMedia->getId();
            if ($itemMediaId !== $lastMediaId) {
                $itemMedia->setPosition(++$key);
            } else {
                $lastMedia = $itemMedia;
            }
        }

        if (!$lastMedia) {
            return;
        }

        $lastMedia->setPosition(++$key);

        $lastMedia->setMediaType($this->targetMediaType);

        // Flush one time to use a transaction and to avoid a duplicate issue
        // with the index item_id/position.
        $entityManager->flush();
    }

    /**
     * Save a temp file into the files/temp directory.
     *
     * @see \DerivativeMedia\Module::makeTempFileDownloadable()
     * @see \Ebook\Mvc\Controller\Plugin\Ebook::saveFile()
     * @see \IiifSearch\Job\ExtractOcr::makeTempFileDownloadable()
     */
    protected function makeTempFileDownloadable(TempFile $tempFile, string $base = ''): ?array
    {
        $baseDestination = '/temp';
        $destinationDir = $this->basePath . $baseDestination . $base;
        if (!$this->checkDestinationDir($destinationDir)) {
            return null;
        }

        $source = $tempFile->getTempPath();

        // Find a unique meaningful filename instead of a hash.
        $name = date('Ymd_His');
        $i = 0;
        do {
            $filename = $name . ($i ? '-' . $i : '') . '.' . $this->targetExtension;
            $destination = $destinationDir . '/' . $filename;
            if (!file_exists($destination)) {
                $result = @copy($source, $destination);
                if (!$result) {
                    $this->logger->err(new Message(
                        'File cannot be saved in temporary directory "%1$s" (temp file: "%2$s")', // @translate
                        $destination, $source
                    ));
                    return null;
                }
                $storageId = $base . $name . ($i ? '-' . $i : '');
                break;
            }
        } while (++$i);

        return [
            'filepath' => $destination,
            'filename' => $filename,
            'url' => $this->baseUri . $baseDestination . $base . '/' . $filename,
            'url_file' => $baseDestination . $base . '/' . $filename,
            'storageId' => $storageId,
        ];
    }

    /**
     * Check or create the destination folder.
     *
     * @param string $dirPath Absolute path.
     * @return string|null
     */
    protected function checkDestinationDir(string $dirPath): ?string
    {
        if (file_exists($dirPath)) {
            if (!is_dir($dirPath) || !is_readable($dirPath) || !is_writeable($dirPath)) {
                $this->logger->err(new Message(
                    'The directory "%s" is not writeable.', // @translate
                    $dirPath
                ));
                return null;
            }
            return $dirPath;
        }

        $result = @mkdir($dirPath, 0775, true);
        if (!$result) {
            $this->logger->err(new Message(
                'The directory "%1$s" is not writeable: %2$s.', // @translate
                $dirPath, error_get_last()['message'] ?? 'unknown error'
            ));
            return null;
        }
        return $dirPath;
    }

    /**
     * Create a list of doctrine expressions for a range.
     *
     * @param string $column
     * @param array|string $ids
     */
    protected function exprRange(string $column, $ids): array
    {
        $ranges = $this->rangeToArray($ids);
        if (empty($ranges)) {
            return [];
        }

        $conditions = [];

        foreach ($ranges as $range) {
            if (strpos($range, '-') === false) {
                $conditions[] = $column . ' = ' . (int) $range;
            } else {
                [$from, $to] = explode('-', $range);
                $from = strlen($from) ? (int) $from : null;
                $to = strlen($to) ? (int) $to : null;
                if ($from && $to) {
                    $conditions[] = "`$column` >= $from AND `$column` <= $to";
                } elseif ($from) {
                    $conditions[] = "`$column` >= $from";
                } elseif ($to) {
                    $conditions[] = "`$column` <= $to";
                }
            }
        }

        return $conditions;
    }

    /**
     * Clean a list of ranges of ids.
     *
     * @param string|array $ids
     */
    protected function rangeToArray($ids): array
    {
        $clean = function ($str): string {
            $str = preg_replace('/[^0-9-]/', ' ', (string) $str);
            $str = preg_replace('/\s+/', ' ', $str);
            return trim($str);
        };

        $ids = is_array($ids)
            ? array_map($clean, $ids)
            : explode(' ', $clean($ids));

        // Skip empty ranges, fake ranges  and ranges with multiple "-".
        return array_values(array_filter($ids, function ($v) {
            return !empty($v) && $v !== '-' && substr_count($v, '-') <= 1;
        }));
    }

    /**
     * Normalize a string as utf8.
     *
     * @todo Check if it is working for non-latin languages.
     * Should be the same normalization in IiifSearch and ExtractOcr.
     *
     * @param string $input
     * @return string
     */
    protected function normalize($input): string
    {
        if ($this->transliterator === null) {
            $this->transliterator = extension_loaded('intl')
                ? (\Transliterator::createFromRules(':: NFD; :: [:Nonspacing Mark:] Remove; :: NFC;') ?: false)
                : false;
        }
        if ($this->transliterator) {
            return (string) $this->transliterator->transliterate((string) $input);
        }
        if (extension_loaded('iconv')) {
            return (string) iconv('UTF-8', 'ASCII//TRANSLIT//IGNORE', (string) $input);
        }
        return (string) $input;
    }

    protected function listMediaImagesData(ItemRepresentation $item): array
    {
        $imageSizes = [];

        foreach ($item->media() as $media) {
            $mediaId = $media->id();
            $mediaType = $media->mediaType();
            if (strtok((string) $mediaType, '/') === 'image') {
                // TODO The images sizes may be stored by xml files too, so skip size retrieving once the matching between images and text is done by page.
                $mediaData = $media->mediaData();
                // Iiif info stored by Omeka.
                if (isset($mediaData['width'])) {
                    $imageSizes[] = [
                        'id' => $mediaId,
                        'width' => $mediaData['width'],
                        'height' => $mediaData['height'],
                        'source' => $media->source(),
                    ];
                }
                // Info stored by Iiif Server.
                elseif (isset($mediaData['dimensions']['original']['width'])) {
                    $imageSizes[] = [
                        'id' => $mediaId,
                        'width' => $mediaData['dimensions']['original']['width'],
                        'height' => $mediaData['dimensions']['original']['height'],
                        'source' => $media->source(),
                    ];
                } elseif ($media->hasOriginal() && strtok($mediaType, '/') === 'image') {
                    $size = ['id' => $mediaId];
                    $size += $this->imageSizeLocal($media);
                    $size['source'] = $media->source();
                    $imageSizes[] = $size;
                }
            }
        }

        return $imageSizes;
    }

    protected function imageSizeLocal(MediaRepresentation $media): array
    {
        // Some media types don't save the file locally.
        $filepath = ($filename = $media->filename())
            ? $this->basePath . '/original/' . $filename
            : $media->originalUrl();
        $size = @getimagesize($filepath);
        // EXIF orientations 5-8 indicate a 90° or 270°
        // rotation, so width and height must be swapped.
        if ($size) {
            $exif = @exif_read_data($filepath);
            if ($exif && !empty($exif['Orientation']) && $exif['Orientation'] >= 5) {
                [$size[0], $size[1]] = [$size[1], $size[0]];
            }
        }
        return $size
            ? ['width' => $size[0], 'height' => $size[1]]
            : ['width' => 0, 'height' => 0];
    }
}
