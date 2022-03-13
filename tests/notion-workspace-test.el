(ert-deftest notion-list-workspace-names ()
  (should (equal '("toracle" "work")
                 (notion-list-workspace-names '(("toracle" . ((:secret-key . "abcd") (:workspace-name . "toracle")))
                                                ("work" . ((:secret-key . "aaaa") (:workspace-name . "work"))))))))
