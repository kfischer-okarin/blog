(defun concat-lines (&rest lines)
  (mapconcat
   (lambda (line) (if (string-empty-p line) "" line))
   lines
   "\n"))

(defun export-org-lines (&rest lines)
  (apply #'export-org-lines-with-options nil lines))

(defun export-org-lines-with-options (options &rest lines)
  (let ((org-content (apply #'concat-lines lines)))
    (with-temp-buffer
      (insert org-content)
      (string-trim-right
       (org-export-as org-export-website-backend nil nil nil
                      (org-combine-plists '(:page-template "{{ content }}") options))))))

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
  (should (equal (export-org-lines "** bar")
                 (concat-lines "<section>"
                               "<h2>bar</h2>"
                               "</section>")))
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
                               "<section>"
                               "<h2>Subheader</h2>"
                               ""
                               "<p>Something secret<sup><a href=\"#fn-1\" id=\"fnref-1\">[1]</a></sup></p>"
                               ""
                               "</section>"
                               ""
                               ""
                               "<div class=\"footnotes\"><ol><li><a href=\"#fnref-1\" id=\"fn-1\">[1]</a> not telling you</li></ol></div>"))))

(ert-deftest test-org-export-website-first-paragraph-without-markup-and-newlines-becomes-description ()
  (should (equal (export-org-lines-with-options '(:page-template "{{ description }}")
                                                (article-headline "foo")
                                                "Some /content/"
                                                "with newlines."
                                                ""
                                                "More content")
                 "Some content with newlines.")))

(ert-deftest test-org-export-website-description-can-be-overwritten ()
  (should (equal (export-org-lines-with-options '(:page-template "{{ description }}")
                                                "* foo"
                                                ":PROPERTIES:"
                                                ":PUBLISHED_AT: [2024-02-16 Fri 00:21]"
                                                ":EXPORT_DESCRIPTION: Overwritten description."
                                                ":END:"
                                                "This will not be used as description.")
                  "Overwritten description.")))

(ert-deftest test-org-export-website-link-http-https ()
  (should (equal (export-org-lines "[[http://example.com][Link Text]]")
                 "<p><a href=\"http://example.com\">Link Text</a></p>"))
  (should (equal (export-org-lines "[[https://example.com][Link Text 2]]")
                 "<p><a href=\"https://example.com\">Link Text 2</a></p>"))
  (should (equal (export-org-lines "[[http://example.com]]")
                 "<p><a href=\"http://example.com\">http://example.com</a></p>")))

(ert-deftest test-org-export-website-link-image-file ()
  (should (equal (export-org-lines "[[file:image.png][Image Text]]")
                 "<p><img src=\"./image.png\" alt=\"Image Text\" /></p>"))
  (should (equal (export-org-lines "[[file:image.png]]")
                 "<p><img src=\"./image.png\" /></p>"))
  (should (equal (export-org-lines-with-options '(:media-path "./media/") "[[file:image.png]]")
                 "<p><img src=\"./media/image.png\" /></p>")))

(ert-deftest test-org-export-website-link-video-file ()
  (should (equal (export-org-lines "[[file:video.mp4][Video Text]]")
                 "<p><figure><video controls width=\"640\"><source src=\"./video.mp4\" type=\"video/mp4\" /></video><figcaption>Video Text</figcaption></figure></p>"))
  (should (equal (export-org-lines-with-options '(:media-path "./media/") "[[file:video.mp4][Video Text]]")
                  "<p><figure><video controls width=\"640\"><source src=\"./media/video.mp4\" type=\"video/mp4\" /></video><figcaption>Video Text</figcaption></figure></p>")))

(ert-deftest test-org-export-website-bold ()
  (should (equal (export-org-lines "Some *bold* text.")
                 "<p>Some <strong>bold</strong> text.</p>")))

(ert-deftest test-org-export-website-italic ()
  (should (equal (export-org-lines "/foo/")
                 "<p><em>foo</em></p>")))

(ert-deftest test-org-export-website-code ()
  (should (equal (export-org-lines "Some ~code~.")
                 "<p>Some <code>code</code>.</p>")))

(ert-deftest test-org-export-website-src-block ()
  (should (equal (export-org-lines "#+begin_src python"
                                   "  # some comment"
                                   "  print('foo')"
                                   ""
                                   "  1 + 2"
                                   "#+end_src")
                 "<pre><code class=\"language-python\"># some comment\nprint('foo')\n\n1 + 2\n</code></pre>")))

(ert-deftest test-org-export-website-plain-list ()
  (should (equal (export-org-lines "- foo"
                                   "- bar")
                 (concat-lines "<ul>"
                               "<li>foo</li>"
                               "<li>bar</li>"
                               "</ul>"))))

(ert-deftest test-replace-placeholders ()
  (should (equal (replace-placeholders "foo {{foo}} bar {{bar}} baz" "{{foo}}" "FOO" "{{bar}}" "BAR")
                 "foo FOO bar BAR baz")))

(ert-deftest test-remove-html-tags ()
  (should (equal (remove-html-tags "<p>Some <em>emphasized</em> text and a <a href=\"somewhere\">link</a>.</p>")
                 "Some emphasized text and a link.")))

(ert-deftest test-shorten-and-normalize-whitespace ()
  (should (equal (shorten-and-normalize-whitespace "Some \n content\n\twith\nnewlines.")
                 "Some content with newlines.")))
