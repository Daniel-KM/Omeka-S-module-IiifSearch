<?php declare(strict_types=1);

namespace IiifSearch\Stdlib;

/**
 * Read the image-filename hint embedded in an OCR source so the pairer's Layer
 * 0b (sourceImageInformation) can use it.
 *
 * Supports:
 * - ALTO: Description/sourceImageInformation/fileName.
 * - hOCR: <*[class~="ocr_page"] title="image foo.tif; bbox …">
 * - TEI:  <facsimile><surface @source="…">
 *
 * Returns null when no hint is present; the file is read only once, no caching
 * here since PageSource construction is itself a one-shot per item.
 */
class PageSourceHintReader
{
    public function readImageHint(string $filepath, string $classification): ?string
    {
        if (!is_readable($filepath)) {
            return null;
        }
        switch ($classification) {
            case XmlMediaClassifier::TYPE_ALTO:
                return $this->readAltoHint($filepath);
            case XmlMediaClassifier::TYPE_HOCR:
                return $this->readHocrHint($filepath);
            case XmlMediaClassifier::TYPE_TEI:
                return $this->readTeiHint($filepath);
        }
        return null;
    }

    protected function readAltoHint(string $filepath): ?string
    {
        $head = $this->readHead($filepath, 8192);
        if ($head === '' || stripos($head, 'sourceimageinformation') === false) {
            return null;
        }
        if (preg_match('~<(?:[\w-]+:)?fileName>\s*([^<]+?)\s*</(?:[\w-]+:)?fileName>~i', $head, $m)) {
            return trim($m[1]);
        }
        return null;
    }

    protected function readHocrHint(string $filepath): ?string
    {
        $head = $this->readHead($filepath, 8192);
        if ($head === '' || stripos($head, 'ocr_page') === false) {
            return null;
        }
        if (preg_match('~class=["\'][^"\']*ocr_page[^"\']*["\'][^>]*title=["\'][^"\']*\bimage\s+([^;"\']+)~i', $head, $m)) {
            return trim($m[1], " \t\"'");
        }
        return null;
    }

    protected function readTeiHint(string $filepath): ?string
    {
        $head = $this->readHead($filepath, 8192);
        if ($head === '') {
            return null;
        }
        if (preg_match('~<(?:[\w-]+:)?surface\b[^>]*\s(?:[\w-]+:)?source=["\']([^"\']+)~i', $head, $m)) {
            return trim($m[1]);
        }
        if (preg_match('~<(?:[\w-]+:)?graphic\b[^>]*\s(?:[\w-]+:)?url=["\']([^"\']+)~i', $head, $m)) {
            return trim($m[1]);
        }
        return null;
    }

    protected function readHead(string $filepath, int $bytes): string
    {
        $fp = @fopen($filepath, 'rb');
        if (!$fp) {
            return '';
        }
        $head = (string) fread($fp, $bytes);
        fclose($fp);
        return $head;
    }
}
