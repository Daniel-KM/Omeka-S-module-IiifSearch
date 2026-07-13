<?php declare(strict_types=1);

namespace IiifSearch\Stdlib;

use Omeka\Api\Representation\MediaRepresentation;

/**
 * Classify an XML/HTML media by inspecting its root element.
 *
 * Required because Omeka media_type for OCR uploads is usually application/xml
 * or text/xml (rarely application/alto+xml), and the file extension is usually
 * .xml whether the payload is alto, mets, pdf2xml or another xml dialect.
 * Extension or mime alone are not discriminating; only content inspection is.
 */
class XmlMediaClassifier
{
    const TYPE_ALTO = 'alto';
    const TYPE_METS = 'mets';
    const TYPE_HOCR = 'hocr';
    const TYPE_PDF2XML = 'pdf2xml';
    const TYPE_OTHER = 'other';
    const TYPE_UNKNOWN = 'unknown';

    /**
     * @var array<int|string,string>
     */
    protected $cache = [];

    public function classifyFile(string $filepath): string
    {
        if (!is_readable($filepath)) {
            return self::TYPE_UNKNOWN;
        }
        $fp = @fopen($filepath, 'rb');
        if (!$fp) {
            return self::TYPE_UNKNOWN;
        }
        $head = (string) fread($fp, 4096);
        fclose($fp);
        return $this->classifyString($head);
    }

    public function classifyString(string $head): string
    {
        if ($head === '') {
            return self::TYPE_UNKNOWN;
        }
        // hOCR first: it is HTML, root <html>, with ocr-* hints.
        if (stripos($head, '<html') !== false
            && (preg_match('~<meta[^>]+name=["\']ocr-system~i', $head)
                || preg_match('~class=["\'][^"\']*ocr(?:_page|x_word|_carea|_line|_par)~i', $head))
        ) {
            return self::TYPE_HOCR;
        }
        if (preg_match('~<(?:[\w-]+:)?alto\b~i', $head)) {
            return self::TYPE_ALTO;
        }
        if (preg_match('~<(?:[\w-]+:)?mets\b~i', $head)) {
            return self::TYPE_METS;
        }
        if (preg_match('~<pdf2xml\b~i', $head)) {
            return self::TYPE_PDF2XML;
        }
        return self::TYPE_OTHER;
    }

    public function classifyMedia(MediaRepresentation $media, string $basePath): string
    {
        $mediaId = (int) $media->id();
        if ($mediaId && isset($this->cache[$mediaId])) {
            return $this->cache[$mediaId];
        }
        $result = self::TYPE_UNKNOWN;
        $filename = $media->filename();
        if ($filename) {
            $result = $this->classifyFile($basePath . '/original/' . $filename);
        }
        if ($mediaId) {
            $this->cache[$mediaId] = $result;
        }
        return $result;
    }

    /**
     * Pre-filter: media whose payload could plausibly be xml or hocr, based on
     * extension and mime, before content inspection. Avoids sniffing every
     * media of an item.
     */
    public function isXmlLikeMedia(MediaRepresentation $media): bool
    {
        $ext = strtolower((string) $media->extension());
        if (in_array($ext, ['xml', 'html', 'htm', 'hocr', 'alto'], true)) {
            return true;
        }
        $mime = strtolower((string) $media->mediaType());
        if ($mime === '') {
            return false;
        }
        return strpos($mime, 'xml') !== false
            || $mime === 'text/html'
            || $mime === 'application/octet-stream';
    }
}
