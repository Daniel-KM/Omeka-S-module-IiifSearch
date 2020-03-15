<?php

namespace IiifSearch\Controller;

use Omeka\Mvc\Exception\NotFoundException;
use Zend\Mvc\Controller\AbstractActionController;
use Zend\View\Model\JsonModel;

class SearchController extends AbstractActionController
{
    public function indexAction()
    {
        $id = $this->params('id');
        if (empty($id)) {
            throw new NotFoundException;
        }

        $q = $this->params()->fromQuery('q');
        if (!strlen($q)) {
            $this->getResponse()->setStatusCode(400);
            return new JsonModel([
                'status' => 'error',
                'message' => $this->translate('Missing or empty query.'), // @translate
            ]);
        }

        // Exception is automatically thrown by api.
        $item = $this->api()->read('items', $id)->getContent();

        $iiifSearch = $this->viewHelpers()->get('iiifSearch');
        $searchResponse = $iiifSearch($item);

        $searchResponse = (object) $searchResponse;
        return $this->jsonLd($searchResponse);
    }
}
