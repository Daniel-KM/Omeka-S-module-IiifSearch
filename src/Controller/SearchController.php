<?php declare(strict_types=1);

namespace IiifSearch\Controller;

use Laminas\Http\Response;
use Laminas\Mvc\Controller\AbstractActionController;
use Laminas\View\Model\JsonModel;

class SearchController extends AbstractActionController
{
    public function indexAction()
    {
        // TODO The output must be a valid json iiif response.

        $id = $this->params('id');
        if (empty($id)) {
            $this->getResponse()->setStatusCode(Response::STATUS_CODE_400);
            return new JsonModel([
                'status' => 'error',
                'message' => $this->translate('Missing or empty query.'), // @translate
            ]);
        }

        $q = (string) $this->params()->fromQuery('q');
        if (!strlen($q)) {
            $this->getResponse()->setStatusCode(Response::STATUS_CODE_400);
            return new JsonModel([
                'status' => 'error',
                'message' => $this->translate('Missing or empty query.'), // @translate
            ]);
        }

        // Exception is automatically thrown by api.
        if (is_numeric($id)) {
            try {
                $item = $this->api()->read('items', $id)->getContent();
            } catch (\Exception $e) {
                // See below.
            }
        } elseif (class_exists('CleanUrl\Module', false)) {
            $item = $this->viewHelpers()->get('getResourceFromIdentifier')($id);
        }

        if (empty($item)) {
            $this->getResponse()->setStatusCode(Response::STATUS_CODE_404);
            return new JsonModel([
                'status' => 'error',
                'message' => $this->translate('Resource not found or unavailable.'),  // @translate
            ]);
        }

        $iiifSearch = $this->viewHelpers()->get('iiifSearch');
        $searchResponse = $iiifSearch($item);

        if (!$searchResponse) {
            $this->getResponse()->setStatusCode(Response::STATUS_CODE_400);
            return new JsonModel([
                'status' => 'error',
                'message' => sprintf($this->translate('Search is not supported for resource #%d (missing XML and/or image files).'), $id), // @translate
            ]);
        }

        return $this->jsonLd($searchResponse);
    }

    public function annotationListAction()
    {
        // TODO Implement annotation-list action.
        $this->getResponse()->setStatusCode(Response::STATUS_CODE_501);
        return new JsonModel([
            'status' => 'error',
            'message' => $this->translate('Direct request to annotation-list is not implemented.'), // @translate
        ]);
    }
}
