
(ert-deftest notion-api-current-line-string ()
  (with-temp-buffer
    (insert "Hello world")
    (newline)
    (insert "Line 2")
    (newline)
    (goto-char 0)
    (should (equal "Hello world" (notion-api-current-line-string)))
    (next-line)
    (should (equal "Line 2" (notion-api-current-line-string)))))


(ert-deftest notion-api-current-line-http-preamble-p ()
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK")
    (newline)
    (goto-char 0)
    (should (notion-api-current-line-http-preamble-p)))

  (with-temp-buffer
    (insert "HTTP/1.0 200 OK")
    (newline)
    (goto-char 0)
    (should (notion-api-current-line-http-preamble-p)))

  (with-temp-buffer
    (insert "Not HTTP Document")
    (newline)
    (goto-char 0)
    (should (not (notion-api-current-line-http-preamble-p)))))


(ert-deftest notion-api-current-line-http-header-p ()
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK")
    (newline)
    (insert "Date: 2022-01-01T00:00:00Z")
    (newline)
    (insert "Authorization: Bearer new-token")
    (newline)
    (newline)
    (goto-char 0)
    (should (not (notion-api-current-line-http-header-p)))
    (next-line)
    (should (notion-api-current-line-http-header-p))
    (next-line)
    (should (notion-api-current-line-http-header-p))
    (next-line)
    (should (not (notion-api-current-line-http-header-p)))))


(ert-deftest notion-api-extract-response ()
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK")
    (newline)
    (insert "Date: 2022-01-01T00:00:00Z")
    (newline)
    (insert "Authorization: Bearer new-token")
    (newline)
    (newline)
    (insert "Response body")
    (goto-char 0)
    (should (equal "Response body"
                   (notion-api-extract-response (current-buffer))))))

(provide 'notion-api-test)
