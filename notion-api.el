(require 'url)
(require 'json)


(defconst *notion-api-v1-endpoint-base*
  "https://api.notion.com/v1")


(defconst *notion-api-v1-endpoint-search*
  (format "%s%s"
          *notion-api-v1-endpoint-base*
          "/search"))


(defun notion-api-default-headers ()
  `(("Content-Type" . "application-json")
    ("Authorization" . ,(format "Bearer %s"
                                (notion-current-workspace-attribute :secret-key)))
    ("Notion-Version" . "2022-02-22")))


(defun notion-api-call-post (endpoint body)
    (let ((url-request-extra-headers (notion-api-default-headers))
          (url-request-method "POST")
          (url-request-data body))
    (switch-to-buffer (url-retrieve (format "%s%s" *notion-api-v1-endpoint-base* endpoint)
                   (lambda (response) t)))))


(defun notion-api-call-get (endpoint)
    (let ((url-request-extra-headers (notion-api-default-headers)))
    (switch-to-buffer (url-retrieve (format "%s%s" *notion-api-v1-endpoint-base* endpoint)
                   (lambda (response) t)))))


(defun notion-api-search (query &optional direction timestamp)
  (notion-api-call-post "/search"
                        (when query (notion-search-body query direction timestamp))))


(defun notion-api-search-body (query &optional direction timestamp)
  (let ((params nil))
    (when query (add-to-list 'params `("query" . ,query)))
    (when (or direction timestamp)
      (let ((sort-params nil))
        (when 'direction (add-to-list 'sort-params `("direction" . ,direction)))
        (when 'timestamp (add-to-list 'sort-params `("timestamp" . ,timestamp)))
        (add-to-list 'params `("sort" . ,sort-params))))
    (json-encode-list params)))


(defun notion-api-retrieve-page (page-id)
  (notion-api-call-get (format "/pages/%s" page-id)))


(provide 'notion-api)
