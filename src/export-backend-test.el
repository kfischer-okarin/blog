(ert-deftest test-org-export-website-headline ()
  (should (equal (org-export-website-headline '(headline (:raw-value "foo" :level 1)) nil nil)
                 "<h1>foo</h1>"))
  (should (equal (org-export-website-headline '(headline (:raw-value "bar" :level 2)) nil nil)
                 "<h2>bar</h2>"))
  (should (equal (org-export-website-headline '(headline (:raw-value "baz" :level 3)) "Some content" nil)
                  "<h3>baz</h3>\n\nSome content")))

(ert-deftest test-regexp-match-column ()
  (should (equal (regexp-match-column "b\\w\\w" "foo\nfoo bar baz") 4)))

(ert-deftest test-indent-string ()
  (should (equal (indent-string "foo\nbar\nbaz" 2) "  foo\n  bar\n  baz"))
  (should (equal (indent-string "foo\n\nbar" 4) "    foo\n\n    bar"))) ; empty line should not be indented

(ert-deftest test-replace-placeholder-with-indent ()
  (should (equal (replace-placeholder-with-indent "{{here}}" "content1\ncontent2" "foo\n  {{here}}\nbaz")
                  "foo\n  content1\n  content2\nbaz")))
