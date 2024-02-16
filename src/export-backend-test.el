(ert-deftest test-org-export-website-headline ()
  (should (equal (org-export-website-headline
                  '(headline (:raw-value "foo" :level 1 :PUBLISHED_AT "[2024-02-16 Fri 00:21]")) nil nil)
                 "<h1>foo</h1>"))
  (should (equal (org-export-website-headline
                  '(headline (:raw-value "foo" :level 1)) nil nil)
                 nil))
  (should (equal (org-export-website-headline
                  '(headline (:raw-value "bar" :level 2)) nil nil)
                 "<h2>bar</h2>"))
  (should (equal (org-export-website-headline
                  '(headline (:raw-value "baz" :level 3)) "Some content" nil)
                 "<h3>baz</h3>\n\nSome content")))

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

(ert-deftest test-org-export-website-link-http-https ()
  (should (equal
           (org-export-website-link '(link (:raw-link "http://example.com" :type "http")) "Link Text" nil)
           "<a href=\"http://example.com\">Link Text</a>"))
  (should (equal
           (org-export-website-link '(link (:raw-link "https://example.com" :type "https")) "Link Text 2" nil)
           "<a href=\"https://example.com\">Link Text 2</a>"))
  (should (equal
           (org-export-website-link '(link (:raw-link "http://example.com") :type "http") nil nil)
           "<a href=\"http://example.com\">http://example.com</a>")))

(ert-deftest test-org-export-website-link-image-file ()
  (should (equal
           (org-export-website-link
            '(link (:raw-link "file:./image.png" :path "./image.png" :type "file")) "Image Text" nil)
           "<img src=\"./image.png\" alt=\"Image Text\" />"))
  (should (equal
           (org-export-website-link
            '(link (:raw-link "file:./image.png" :path "./image.png" :type "file")) nil nil)
           "<img src=\"./image.png\" />")))

(ert-deftest test-org-export-website-link-video-file ()
  (should (equal
           (org-export-website-link
            '(link (:raw-link "file:./video.mp4" :path "./video.mp4" :type "file")) "Video Text" nil)
           "<figure><video controls width=\"640\"><source src=\"./video.mp4\" type=\"video/mp4\" /></video><figcaption>Video Text</figcaption></figure>")))

(ert-deftest test-org-export-website-italic ()
  (should (equal (org-export-website-italic '(italic) "foo" nil)
                 "<em>foo</em>")))

(ert-deftest test-org-export-website-src-block ()
  (should (equal (org-export-website-src-block
                  '(src-block (:language "python" :value "  print('foo')\n\n  1 + 2")) nil nil)
                  "<pre><code class=\"language-python\">print('foo')\n\n1 + 2</code></pre>")))
