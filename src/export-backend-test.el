(defun concat-lines (&rest lines)
  (mapconcat
   (lambda (line) (if (string-empty-p line) "" line))
   lines
   "\n"))

(defun export-org-lines (&rest lines)
  (let ((org-export-website-template-string "{{ content }}")
        (org-content (apply #'concat-lines lines)))
    (with-temp-buffer
      (insert org-content)
      (string-trim-right (org-export-as org-export-website-backend)))))

(defun article-headline (title &optional published-at)
  (concat-lines (concat "* " title)
                ":PROPERTIES:"
                (concat ":PUBLISHED_AT: " (or published-at "[2024-02-16 Fri 00:21]"))
                ":END:"))

(ert-deftest test-org-export-website-headline ()
  (should (equal (export-org-lines "* foo"
                                   ":PROPERTIES:"
                                   ":PUBLISHED_AT: [2024-02-16 Fri 00:21]"
                                   ":END:")
                 "<h1>foo</h1>"))
  (should (equal (export-org-lines "* foo") ""))
  (should (equal (export-org-lines "** bar") "<h2>bar</h2>"))
  (should (equal (export-org-lines "*** baz"
                                   "Some content")
                 (concat-lines "<h3>baz</h3>"
                               ""
                               "<p>Some content</p>"))))

(ert-deftest test-org-export-website-headline-footnotes-are-attached-after-level-1-headlines ()
  (should (equal (export-org-lines (article-headline "foo")
                                   "** Subheader"
                                   "Something secret[fn::not telling you]")
                 (concat-lines "<h1>foo</h1>"
                               ""
                               "<h2>Subheader</h2>"
                               ""
                               "<p>Something secret<sup><a href=\"#fn-1\" id=\"fnref-1\">[1]</a></sup></p>"
                               ""
                               ""
                               "<div class=\"footnotes\"><ol><li><a href=\"#fnref-1\" id=\"fn-1\">[1]</a> not telling you</li></ol></div>"))))

(ert-deftest test-org-export-website-link-http-https ()
  (should (equal (export-org-lines "[[http://example.com][Link Text]]")
                 "<p><a href=\"http://example.com\">Link Text</a></p>"))
  (should (equal (export-org-lines "[[https://example.com][Link Text 2]]")
                 "<p><a href=\"https://example.com\">Link Text 2</a></p>"))
  (should (equal (export-org-lines "[[http://example.com]]")
                 "<p><a href=\"http://example.com\">http://example.com</a></p>")))

(ert-deftest test-org-export-website-link-image-file ()
  (should (equal (export-org-lines "[[file:./image.png][Image Text]]")
                 "<p><img src=\"./image.png\" alt=\"Image Text\" /></p>"))
  (should (equal (export-org-lines "[[file:./image.png]]")
                 "<p><img src=\"./image.png\" /></p>")))

(ert-deftest test-org-export-website-link-video-file ()
  (should (equal (export-org-lines "[[file:./video.mp4][Video Text]]")
                                   "<p><figure><video controls width=\"640\"><source src=\"./video.mp4\" type=\"video/mp4\" /></video><figcaption>Video Text</figcaption></figure></p>")))

(ert-deftest test-org-export-website-italic ()
  (should (equal (export-org-lines "/foo/")
                 "<p><em>foo</em></p>")))

(ert-deftest test-org-export-website-src-block ()
  (should (equal (export-org-lines "#+begin_src python"
                                   "  print('foo')"
                                   ""
                                   "  1 + 2"
                                   "#+end_src")
                 "<pre><code class=\"language-python\">print('foo')\n\n1 + 2\n</code></pre>")))

(ert-deftest test-regexp-match-column ()
  (should (equal (regexp-match-column "b\\w\\w" "foo\nfoo bar baz") 4)))

(ert-deftest test-indent-html-string ()
  (should (equal (indent-html-string "foo\nbar\nbaz" 2) "  foo\n  bar\n  baz"))
  (should (equal (indent-html-string "foo\n\nbar" 4) "    foo\n\n    bar")) ; empty line should not be indented
  (should (equal (indent-html-string "<pre><code>foo\nbar\nbaz</code></pre>" 2)
                 "  <pre><code>foo\nbar\nbaz</code></pre>"))) ; should not indent code block contents

(ert-deftest test-replace-placeholder-with-indent ()
  (should (equal (replace-placeholder-with-indent "{{here}}" "content1\ncontent2" "foo\n  {{here}}\nbaz")
                 "foo\n  content1\n  content2\nbaz")))
