<?php declare(strict_types=1);

namespace IiifSearchTest\Iiif;

use IiifSearch\Iiif\AnnotationSearchResult;
use PHPUnit\Framework\TestCase;

class AnnotationSearchResultTest extends TestCase
{
    /**
     * Minimal result data for building an AnnotationSearchResult.
     */
    protected function sampleResult(): array
    {
        $resource = new \stdClass();
        return [
            'resource' => $resource,
            'image' => ['id' => 10, 'width' => 1000, 'height' => 1000],
            'page' => ['number' => '1', 'width' => '1000', 'height' => '1000'],
            'zone' => [
                'text' => 'fox',
                'left' => '100',
                'top' => '200',
                'width' => '70',
                'height' => '40',
            ],
            'chars' => 'fox',
            'hit' => 1,
        ];
    }

    protected function buildAnnotation(array $result = null): AnnotationSearchResult
    {
        $annotation = new AnnotationSearchResult();
        $annotation->initOptions([
            'baseResultUrl' => 'http://example.com/annotation/',
            'baseCanvasUrl' => 'http://example.com/canvas/p',
        ]);
        $annotation->setResult($result ?? $this->sampleResult());
        return $annotation;
    }

    /**
     * @see https://iiif.io/api/search/1.0/#presentation-api-compatible-responses
     * The resource type MUST be cnt:ContentAsText per the IIIF spec.
     */
    public function testResourceTypeIsContentAsText(): void
    {
        $annotation = $this->buildAnnotation();
        $resource = $annotation->resource();
        $this->assertSame(
            'cnt:ContentAsText',
            $resource['@type'],
            'The resource @type must be cnt:ContentAsText (not cnt:ContextAstext).'
        );
    }

    public function testResourceChars(): void
    {
        $annotation = $this->buildAnnotation();
        $resource = $annotation->resource();
        $this->assertSame('fox', $resource['chars']);
    }

    public function testIdFormat(): void
    {
        $annotation = $this->buildAnnotation();
        $id = $annotation->id();
        // Format: {baseResultUrl}a{page}h{hit}r{x},{y},{w},{h}
        $this->assertMatchesRegularExpression(
            '#^http://example\.com/annotation/a1h1r\d+,\d+,\d+,\d+$#',
            $id
        );
    }

    public function testOnFormat(): void
    {
        $annotation = $this->buildAnnotation();
        $on = $annotation->on();
        // Format: {baseCanvasUrl}{page}#xywh={x},{y},{w},{h}
        $this->assertMatchesRegularExpression(
            '/^http:\/\/example\.com\/canvas\/p1#xywh=\d+,\d+,\d+,\d+$/',
            $on
        );
    }

    public function testPrepareBoxScaling(): void
    {
        $result = $this->sampleResult();
        // Image is half the page size → coordinates should be halved.
        $result['image'] = ['id' => 10, 'width' => 500, 'height' => 500];
        $result['page'] = ['number' => '1', 'width' => '1000', 'height' => '1000'];
        $result['zone'] = [
            'text' => 'fox',
            'left' => '200',
            'top' => '400',
            'width' => '100',
            'height' => '50',
        ];
        $result['chars'] = '';

        $annotation = $this->buildAnnotation($result);
        $on = $annotation->on();
        // Extract xywh from the on string.
        preg_match('/#xywh=(\d+),(\d+),(\d+),(\d+)/', $on, $m);
        $this->assertNotEmpty($m, 'on() should contain xywh coordinates');
        // Scale factor is 0.5 in both directions.
        // With empty chars, x = left * scale, y = top * scale.
        $this->assertEquals(100, (int) $m[1], 'x should be scaled');
        $this->assertEquals(200, (int) $m[2], 'y should be scaled');
        $this->assertEquals(50, (int) $m[3], 'w should be scaled');
        $this->assertEquals(25, (int) $m[4], 'h should be scaled');
    }

    public function testPrepareBoxIdentityScale(): void
    {
        $result = $this->sampleResult();
        // Image = page → no scaling, with empty chars for simple coords.
        $result['image'] = ['id' => 10, 'width' => 2480, 'height' => 3508];
        $result['page'] = ['number' => '1', 'width' => '2480', 'height' => '3508'];
        $result['zone'] = [
            'text' => 'fox',
            'left' => '550',
            'top' => '100',
            'width' => '70',
            'height' => '40',
        ];
        $result['chars'] = '';

        $annotation = $this->buildAnnotation($result);
        $on = $annotation->on();
        preg_match('/#xywh=(\d+),(\d+),(\d+),(\d+)/', $on, $m);
        $this->assertNotEmpty($m);
        $this->assertEquals(550, (int) $m[1], 'x unchanged');
        $this->assertEquals(100, (int) $m[2], 'y unchanged');
        $this->assertEquals(70, (int) $m[3], 'w unchanged');
        $this->assertEquals(40, (int) $m[4], 'h unchanged');
    }

    public function testGetContentStructure(): void
    {
        $annotation = $this->buildAnnotation();
        $content = $annotation->getContent();
        $this->assertArrayHasKey('@context', $content);
        $this->assertArrayHasKey('@id', $content);
        $this->assertArrayHasKey('@type', $content);
        $this->assertArrayHasKey('motivation', $content);
        $this->assertArrayHasKey('resource', $content);
        $this->assertArrayHasKey('on', $content);
        $this->assertSame('oa:Annotation', $content['@type']);
        $this->assertSame('sc:painting', $content['motivation']);
    }
}
