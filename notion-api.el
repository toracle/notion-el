(require 'url)
(require 'json)


(defconst *notion-api-v1-endpoint-base*
  "https://api.notion.com/v1")


(defconst *notion-api-v1-endpoint-search*
  (format "%s%s"
          *notion-api-v1-endpoint-base*
          "/search"))


(defun notion-default-api-headers ()
  `(("Content-Type" . "application-json")
    ("Authorization" . ,(format "Bearer %s"
                                (notion-current-workspace-attribute :secret-key)))
    ("Notion-Version" . "2021-08-16")))


(defun notion-api-call-post (endpoint body)
    (let ((url-request-extra-headers (notion-default-api-headers))
          (url-request-method "POST")
          (url-request-data body))
    (switch-to-buffer (url-retrieve (format "%s%s" *notion-api-v1-endpoint-base* endpoint)
                   (lambda (response) (message "%S" response))))))


(defun notion-api-search (query &optional direction timestamp)
  (notion-api-call-post "/search"
                        (notion-search-body query direction timestamp)))


(defun notion-api-search-body (query &optional direction timestamp)
  (let ((direction (unless direction "ascending")))
    (json-encode-list `(("query" . ,query)
                        ("sort" . (("direction" . ,direction)
                                   ("timestamp" . timestamp)))))))


(provide 'notion-api)
