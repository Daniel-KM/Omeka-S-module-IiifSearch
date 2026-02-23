<?php declare(strict_types=1);

namespace IiifSearchTest\View\Helper;

use IiifSearchTest\IiifSearchTestTrait;
use Laminas\View\Renderer\PhpRenderer;
use PHPUnit\Framework\TestCase;
use ReflectionProperty;

/**
 * Tests the searchFulltextTsv() method via reflection for by-word TSV.
 *
 * Uses sample-byword.tsv with entries:
 * fox  on pages 1 and 2
 * quick on page 1
 * brown on page 1
 */
class SearchFulltextTsvTest extends TestCase
{
    use IiifSearchTestTrait;

    protected function setProperty(object $helper, string $name, $value): void
    {
        $ref = new ReflectionProperty(get_class($helper), $name);
        $ref->setAccessible(true);
        $ref->setValue($helper, $value);
    }

    /**
     * Create a helper stub with all dependencies injected for TSV search.
     */
    protected function buildTsvHelper(array $queryWords, string $query = ''): object
    {
        $helper = $this->createHelperStub();

        // Mock item with id().
        $item = $this->createMock(\Omeka\Api\Representation\ItemRepresentation::class);
        $item->method('id')->willReturn(42);
        $this->setProperty($helper, 'item', $item);

        // Image sizes: 2 pages.
        $this->setProperty($helper, 'imageSizes', [
            0 => ['id' => 101, 'width' => 2480, 'height' => 3508],
            1 => ['id' => 102, 'width' => 2480, 'height' => 3508],
        ]);

        $this->setProperty($helper, 'queryWords', $queryWords);
        $this->setProperty($helper, 'query', $query ?: implode(' ', $queryWords));
        $this->setProperty($helper, 'queryIsExactSearch', false);

        // Mock mediaTsv (nullable).
        $mediaTsv = $this->createMock(\Omeka\Api\Representation\MediaRepresentation::class);
        $mediaTsv->method('id')->willReturn(201);
        $this->setProperty($helper, 'mediaTsv', $mediaTsv);

        $this->setProperty($helper, 'baseResultUrl', 'http://example.com/iiif/42/annotation/search-result/');
        $this->setProperty($helper, 'baseCanvasUrl', 'http://example.com/iiif/42/canvas/p');

        // Mock view with iiifUrl plugin.
        $view = $this->createMock(PhpRenderer::class);
        $iiifUrlPlugin = function () {
            return 'http://example.com/iiif/42';
        };
        $view->method('plugin')->with('iiifUrl')->willReturn($iiifUrlPlugin);
        $view->method('__call')->willReturnCallback(function ($method, $args) {
            return 'http://example.com/iiif/42';
        });
        $helper->setView($view);

        // Mock logger.
        $logger = $this->createMock(\Laminas\Log\Logger::class);
        $this->setProperty($helper, 'logger', $logger);

        return $helper;
    }

    public function testTsvByWordSearchFindsWord(): void
    {
        $helper = $this->buildTsvHelper(['fox']);
        $filepath = $this->getFixturePath('sample-byword.tsv');
        $result = $this->callProtected($helper, 'searchFulltextTsv', [$filepath, true]);
        $this->assertNotNull($result);
        $this->assertSame(2, $result['hit']);
        $this->assertCount(2, $result['resources']);
    }

    public function testTsvByWordHitCounter(): void
    {
        $helper = $this->buildTsvHelper(['fox']);
        $filepath = $this->getFixturePath('sample-byword.tsv');
        $result = $this->callProtected($helper, 'searchFulltextTsv', [$filepath, true]);
        // hit counter = total number of hits.
        $this->assertSame(2, $result['hit']);
    }

    public function testTsvByWordPageNumbering(): void
    {
        $helper = $this->buildTsvHelper(['fox']);
        $filepath = $this->getFixturePath('sample-byword.tsv');
        $result = $this->callProtected($helper, 'searchFulltextTsv', [$filepath, true]);
        // First hit on page 1, second on page 2.
        $firstOn = $result['resources'][0]->getContent()['on'];
        $secondOn = $result['resources'][1]->getContent()['on'];
        $this->assertStringContainsString('/p1#xywh=', $firstOn);
        $this->assertStringContainsString('/p2#xywh=', $secondOn);
    }

    public function testTsvByWordZoneCoordinates(): void
    {
        $helper = $this->buildTsvHelper(['fox']);
        $filepath = $this->getFixturePath('sample-byword.tsv');
        $result = $this->callProtected($helper, 'searchFulltextTsv', [$filepath, true]);
        // Both results should have xywh in on field.
        $firstOn = $result['resources'][0]->getContent()['on'];
        $this->assertMatchesRegularExpression(
            '/xywh=\d+,\d+,\d+,\d+/',
            $firstOn
        );
    }

    public function testTsvByWordNoMatch(): void
    {
        $helper = $this->buildTsvHelper(['xyz']);
        $filepath = $this->getFixturePath('sample-byword.tsv');
        $result = $this->callProtected($helper, 'searchFulltextTsv', [$filepath, true]);
        $this->assertNull($result);
    }
}
