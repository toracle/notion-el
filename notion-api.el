(require 'url)
(require 'json)


(defconst *notion-api-v1-endpoint-base*
  "https://api.notion.com/v1"
  "Base Notion API endpoint URL.")


(defun notion-api-default-headers ()
  "Return default API request headers."
  `(("Content-Type" . "application/json; charset=utf-8")
    ("Authorization" . ,(format "Bearer %s"
                                (notion-current-workspace-attribute :secret-key)))
    ("Notion-Version" . "2022-02-22")))


(defun notion-api-encode-json (lst)
  (encode-coding-string (json-encode-list lst)
                        'utf-8))


(defun notion-api-decode-json (s)
  (json-read-from-string (decode-coding-string s 'utf-8)))


(defun notion-api-call-post (endpoint body)
  "Request POST API call."
  (let ((url-request-extra-headers (notion-api-default-headers))
        (url-request-method "POST")
        (url-request-data body))
    (notion-api-decode-json
     (notion-api-extract-response
      (url-retrieve-synchronously (format "%s%s" *notion-api-v1-endpoint-base* endpoint)
                                  t)))))


(defun notion-api-call-get (endpoint)
  "Request GET API call."
  (let ((url-request-extra-headers (notion-api-default-headers)))
    (notion-api-decode-json
     (notion-api-extract-response
      (url-retrieve-synchronously (format "%s%s" *notion-api-v1-endpoint-base* endpoint)
                                  t)))))


(defun notion-api-current-line-string ()
  (buffer-substring (line-beginning-position)
                    (line-end-position)))


(defun notion-api-current-line-http-preamble-p ()
  (let* ((first-line (notion-api-current-line-string))
         (match-position (string-match "HTTP/1.+ 200 OK" first-line)))
    (equal 0 match-position)))


(defun notion-api-current-line-http-header-p ()
  (string-match "[^ ]: .*" (notion-api-current-line-string)))


(defun notion-api-response-body-to-string ()
  (buffer-substring (point) (buffer-end 1)))


(defun notion-api-extract-response (buffer)
  ;; assert HTTP/1
  ;; assert Key: Value <newline>
  ;; assert empty line
  (with-current-buffer buffer
    (let ((m (point-marker))
          (response nil))
      (goto-char 0)

      (unless (notion-api-current-line-http-preamble-p)
        (error "Response doesn't have a proper HTTP preamble"))
      (next-line)

      (while (notion-api-current-line-http-header-p)
        (next-line))

      ;; skip empty line between header and body
      (next-line)

      (beginning-of-line)
      (setq response (notion-api-response-body-to-string))
      (goto-char m)
      response)))


(defun notion-api-search (&optional query direction timestamp)
  "Request search API call with QUERY term. 

DIRECTION to sort. Possible values are either 'ascending' or 'descending'.
TIMESTAMP to sort against."
  (notion-api-call-post "/search"
                        (notion-api-encode-json (notion-api-search-body query direction timestamp))))


(defun notion-api-search-body (&optional query direction timestamp)
  (let ((params nil))
    (when query (add-to-list 'params `("query" . ,query)))
    (when (or direction timestamp)
      (let ((sort-params nil))
        (when 'direction (add-to-list 'sort-params `("direction" . ,direction)))
        (when 'timestamp (add-to-list 'sort-params `("timestamp" . ,timestamp)))
        (add-to-list 'params `("sort" . ,sort-params))))
    params))


(defun notion-api-retrieve-page (page-id)
  (notion-api-call-get (format "/pages/%s" page-id)))


(defun notion-api-create-page (title &optional parent-id)
  (notion-api-call-post "/pages"
                        (notion-api-encode-json (notion-api-create-page-body title parent-id))))


(defun notion-api-create-page-body (title &optional parent-id)
  (let ((params nil))
    (if parent-id
        (add-to-list 'params `("parent" . (("page_id" . ,parent-id)
                                           ("type" . "page_id"))))
      (add-to-list 'params `("parent" . (("workspace" . t)
                                         ("type" . "workspace")))))
    (add-to-list 'params `("properties" . (("title" . (("title" . ((("text" . (("content" . ,title)))
                                                                    ("type" . "text")))))))))
    params))


(defun notion-api-list-users ()
  (notion-api-call-get "/users"))


(defun notion-api-me ()
  (notion-api-call-get "/users/me"))


(provide 'notion-api)
