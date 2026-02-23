<?php declare(strict_types=1);

namespace IiifSearch;

if (!class_exists('Common\TraitModule', false)) {
    require_once file_exists(dirname(__DIR__) . '/Common/src/TraitModule.php')
        ? dirname(__DIR__) . '/Common/src/TraitModule.php'
        : dirname(__DIR__) . '/Common/TraitModule.php';
}

use Common\Stdlib\PsrMessage;
use Common\TraitModule;
use Laminas\EventManager\Event;
use Laminas\EventManager\SharedEventManagerInterface;
use Laminas\Mvc\MvcEvent;
use Omeka\Module\AbstractModule;

class Module extends AbstractModule
{
    use TraitModule;

    const NAMESPACE = __NAMESPACE__;

    public function onBootstrap(MvcEvent $event): void
    {
        parent::onBootstrap($event);

        $acl = $this->getServiceLocator()->get('Omeka\Acl');
        $acl->allow(null, 'IiifSearch\Controller\Search');

        // Re-encode decoded slashes in identifiers for search routes.
        $event->getApplication()->getEventManager()
            ->attach(MvcEvent::EVENT_ROUTE, [$this, 'reencodeIdentifierSlashes'], 1000);
    }

    /**
     * Re-encode decoded slashes in IIIF Search URL identifiers.
     *
     * @see \IiifServer\Module::reencodeIdentifierSlashes()
     */
    public function reencodeIdentifierSlashes(MvcEvent $event): void
    {
        $request = $event->getRequest();
        if (!$request instanceof \Laminas\Http\PhpEnvironment\Request) {
            return;
        }

        $path = $request->getUri()->getPath();

        // Quick check: only process IIIF Search paths.
        if (strpos($path, '/iiif-search/') === false) {
            return;
        }

        // Parse: /iiif-search/{identifier}[/search|/list/{name}]
        if (!preg_match('#^(/iiif-search)(/[^?]*)$#', $path, $matches)) {
            return;
        }

        $remainder = substr($matches[2], 1);
        $segments = explode('/', $remainder);
        $count = count($segments);

        if ($count <= 1) {
            return;
        }

        // Known IIIF Search keywords that appear after the identifier.
        // @see https://iiif.io/api/search/1.0/
        // @see https://iiif.io/api/search/2.0/
        static $searchKeywords = [
            'search' => true,
            'autocomplete' => true,
            'list' => true,
        ];

        $suffixCount = 0;
        for ($i = $count - 1; $i >= 1; $i--) {
            if (isset($searchKeywords[$segments[$i]])) {
                $suffixCount = $count - $i;
                break;
            }
        }

        $idCount = $count - $suffixCount;
        if ($idCount <= 1) {
            return;
        }

        $idParts = array_slice($segments, 0, $idCount);
        $suffixParts = array_slice($segments, $idCount);
        $newRemainder = implode('%2F', $idParts);
        if ($suffixParts) {
            $newRemainder .= '/' . implode('/', $suffixParts);
        }

        $newPath = $matches[1] . '/' . $newRemainder;
        if ($newPath !== $path) {
            $request->getUri()->setPath($newPath);
        }
    }

    protected function preInstall(): void
    {
        $services = $this->getServiceLocator();
        $translate = $services->get('ControllerPluginManager')
            ->get('translate');

        if (!method_exists($this, 'checkModuleActiveVersion')
            || !$this->checkModuleActiveVersion('Common', '3.4.80')
        ) {
            $message = new PsrMessage(
                $translate('The module %1$s should be upgraded to version %2$s or later.'), // @translate
                'Common', '3.4.80'
            );
            throw new \Omeka\Module\Exception\ModuleCannotInstallException(
                (string) $message
            );
        }

        $this->checkExtractOcr();
    }

    public function attachListeners(SharedEventManagerInterface $sharedEventManager): void
    {
        $sharedEventManager->attach(
            '*',
            'iiifserver.manifest',
            [$this, 'handleIiifServerManifest']
        );
    }

    public function handleIiifServerManifest(Event $event): void
    {
        // Target is the view.
        // Available keys: "format", the manifest, info etc according to format, "resource", "type".

        // This is the iiif type, not omeka one.
        $type = $event->getParam('type');
        if ($type !== 'item') {
            return;
        }

        $resource = $event->getParam('resource');

        // Check first if there is a simple file with data (see module ExtractOcr).
        $services = $this->getServiceLocator();
        $config = $services->get('Config');
        $basePath = $config['file_store']['local']['base_path'] ?: (OMEKA_PATH . '/files');
        $resourceId = $resource->id();
        $simpleFilepath = null;
        $localPaths = [
            $basePath . '/iiif-search/' . $resourceId . '.tsv',
            $basePath . '/alto/' . $resourceId . '.alto.xml',
            $basePath . '/hocr/' . $resourceId . '.hocr.html',
            $basePath . '/pdf2xml/' . $resourceId . '.xml',
            // Old path before ExtractOcr 3.4.7.
            $basePath . '/iiif-search/' . $resourceId . '.xml',
        ];
        foreach ($localPaths as $path) {
            if (file_exists($path)) {
                $simpleFilepath = $path;
                break;
            }
        }

        // Else check if resource has at least one XML file for search.
        if (!$simpleFilepath) {
            $searchServiceAvailable = false;
            $searchMediaTypes = [
                'application/alto+xml',
                'text/vnd.hocr+html',
                'application/vnd.pdf2xml+xml',
                'text/tab-separated-values',
            ];
            foreach ($resource->media() as $media) {
                $mediaType = $media->mediaType();
                if (in_array($mediaType, $searchMediaTypes)) {
                    $searchServiceAvailable = true;
                    break;
                }
            }
            if (!$searchServiceAvailable) {
                return;
            }
        }

        $plugins = $this->getServiceLocator()->get('ViewHelperManager');
        $urlHelper = $plugins->get('url');
        $identifier = $plugins->has('iiifCleanIdentifiers')
            ? $plugins->get('iiifCleanIdentifiers')->__invoke($resource->id())
            : $resource->id();

        // When the server does not support encoded slashes, restore literal
        // slashes in generated URLs so they remain functional.
        $settings = $this->getServiceLocator()->get('Omeka\Settings');
        $encodeSlash = (bool) $settings->get('iiifserver_identifier_encode_slash', false);
        $fixSlash = function (string $url) use ($encodeSlash): string {
            return $encodeSlash ? $url : strtr($url, ['%252F' => '/', '%2F' => '/']);
        };

        /** @var \IiifServer\Iiif\Manifest $manifest */
        $manifest = $event->getParam('manifest');

        // Manage last or recent version of module Iiif Server.
        // TODO Why profile is /0/?
        $isVersion2 = !is_object($manifest);
        if ($isVersion2) {
            $manifest['service'][] = [
                '@context' => 'http://iiif.io/api/search/0/context.json',
                '@id' => $fixSlash($urlHelper('iiifsearch', ['id' => $identifier], ['force_canonical' => true])),
                'profile' => 'http://iiif.io/api/search/0/search',
                'label' => 'Search within this manifest', // @translate
            ];
        } else {
            // Use of "@" is slightly more compatible with old viewers.
            // The context is not required.
            // The SearchService0 is not an official service, but managed by
            // old versions of Universal Viewer and used by Wellcome library.
            $service0 = [
                '@context' => 'http://iiif.io/api/search/0/context.json',
                '@id' => $fixSlash($urlHelper('iiifsearch', ['id' => $identifier], ['force_canonical' => true])),
                '@type' => 'SearchService0',
                'profile' => 'http://iiif.io/api/search/0/search',
                'label' => 'Search within this manifest', // @translate
            ];
            $service1 = [
                '@context' => 'http://iiif.io/api/search/1/context.json',
                'id' => $fixSlash($urlHelper('iiifsearch/search', ['id' => $identifier], ['force_canonical' => true])),
                'type' => 'SearchService1',
                'profile' => 'http://iiif.io/api/search/1/search',
                'label' => 'Search within this manifest', // @translate
            ];
            // Check version of module IiifServer.
            if (method_exists($manifest, 'getPropertyRequirements')) {
                $manifest['service'][] = new \IiifServer\Iiif\Service($service0);
                $manifest['service'][] = new \IiifServer\Iiif\Service($service1);
            } else {
                $manifest
                    ->appendService(new \IiifServer\Iiif\Service($resource, $service0))
                    ->appendService(new \IiifServer\Iiif\Service($resource, $service1))
                ;
            }
        }

        $event->setParam('manifest', $manifest);
    }

    protected function checkExtractOcr()
    {
        if (class_exists('ExtractOcr\Module', false)) {
            $services = $this->getServiceLocator();
            $translator = $services->get('MvcTranslator');
            $connection = $services->get('Omeka\Connection');
            $qb = $connection->createQueryBuilder();
            $qb
                ->select('module.version')
                ->from('module', 'module')
                ->where($qb->expr()->eq('module.id', ':module'));
            $moduleVersion = $connection->executeQuery($qb, ['module' => 'ExtractOcr'])->fetchOne();
            if (version_compare($moduleVersion, '3.4.8', '<')) {
                $message = new \Omeka\Stdlib\Message(
                    $translator->translate('The module %1$s should be upgraded to version %2$s or later.'), // @translate
                    'ExtractOcr', '3.4.8'
                );
                throw new \Omeka\Module\Exception\ModuleCannotInstallException((string) $message);
            }
        }
    }
}
