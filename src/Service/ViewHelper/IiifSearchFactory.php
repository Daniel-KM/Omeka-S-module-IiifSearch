<?php declare(strict_types=1);

namespace IiifSearch\Service\ViewHelper;

use IiifSearch\View\Helper\IiifSearch;
use Interop\Container\ContainerInterface;

class IiifSearchFactory
{
    public function __invoke(ContainerInterface $services, $requestedName, array $options = null)
    {
        $config = $services->get('Config');
        $basePath = $config['file_store']['local']['base_path'] ?: (OMEKA_PATH . '/files');
        $plugins = $services->get('ControllerPluginManager');
        $helpers = $services->get('ViewHelperManager');
        $settings = $services->get('Omeka\Settings');

        return new IiifSearch(
            $services->get('Omeka\ApiManager'),
            $helpers->has('derivativeList') ? $helpers->get('derivativeList') : null,
            $helpers->get('fixUtf8'),
            $plugins->has('imageSize') ? $plugins->get('imageSize') : null,
            $services->get('Omeka\Logger'),
            $helpers->get('xmlAltoSingle'),
            $basePath,
            !$settings->get('iiifsearch_disable_search_media_values'),
            $settings->get('iiifsearch_xml_fix_mode', 'no'),
            $settings->get('iiifsearch_xml_image_match', 'order')
        );
    }
}
