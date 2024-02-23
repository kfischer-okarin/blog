(require 'ox)

(defconst org-export-website-backend (org-export-create-backend
                                      :transcoders '((template . org-export-website-template)
                                                     (headline . org-export-website-headline)
                                                     (section . org-export-website-section)
                                                     (paragraph . org-export-website-paragraph)
                                                     (link . org-export-website-link)
                                                     (italic . org-export-website-italic)
                                                     (src-block . org-export-website-src-block)
                                                     (footnote-reference . org-export-website-footnote-reference)
                                                     (plain-list . org-export-website-plain-list)
                                                     (item . org-export-website-item))))

(defvar org-export-website-template-string)

(defun org-export-website-template (contents info)
  (let* ((contents-placeholder-indent (regexp-match-column "{{ content }}" org-export-website-template-string)))
    (replace-placeholder-with-indent "{{ content }}" contents org-export-website-template-string)))

(defun org-export-website-headline (headline contents info)
  (let* ((level (org-element-property :level headline))
         (published-at (org-element-property :PUBLISHED_AT headline))
         (should-process (or (> level 1) published-at)))
    (when should-process
      (let ((footnotes-part
             (when (eq level 1)
               (concat "\n\n"
                       (org-export-website--build-footnotes (plist-get info :footnotes))))))
        (when (eq level 1)
          (plist-put info :footnotes nil))
        (let* ((title (org-element-property :raw-value headline))
               (section-start (when (eq level 2) "<section>\n"))
               (section-end (when (eq level 2) "\n</section>"))
               (html-heading (format "<h%d>%s</h%d>" level title level))
               (body-part (when contents (concat "\n\n" contents))))
          (concat section-start
                  html-heading
                  body-part
                  section-end
                  footnotes-part))))))

(defun org-export-website--build-footnotes (footnotes)
  (when footnotes
    (concat "<div class=\"footnotes\">"
            "<ol>"
            (mapconcat (lambda (footnote) (concat "<li>" footnote "</li>")) footnotes)
            "</ol>"
            "</div>")))

(defun org-export-website-section (section contents _info)
  contents)

(defun org-export-website-paragraph (paragraph contents _info)
  (concat "<p>" contents "</p>"))

(defun org-export-website-link (link contents _info)
  (let* ((type (org-element-property :type link))
         (extension (file-name-extension (org-element-property :raw-link link))))
    (cond
      ((and (string= type "file") (seq-contains-p '("jpg" "jpeg" "png" "gif" "svg") extension))
       (org-export-website-link--image link contents _info))
      ((and (string= type "file") (string= extension "mp4"))
       (org-export-website-link--video link contents _info))
      (t
       (org-export-website-link--fallback link contents _info)))))

(defun org-export-website-link--image (link contents _info)
  (let* ((path (org-element-property :path link))
         (attributes (if contents (concat " alt=\"" contents "\"") )))
    (concat "<img src=\"" path "\"" attributes " />")))

(defun org-export-website-link--video (link contents _info)
  (let* ((path (org-element-property :path link)))
    (concat "<figure>"
            "<video controls width=\"640\"><source src=\"" path "\" type=\"video/mp4\" /></video>"
            "<figcaption>" contents "</figcaption>"
            "</figure>")))

(defun org-export-website-link--fallback (link contents _info)
  (let* ((type (org-element-property :type link))
         (url (org-element-property :raw-link link))
         (description (or contents url)))
    (concat "<a href=\"" url "\">" description "</a>")))

(defun org-export-website-italic (italic contents _info)
  (concat "<em>" contents "</em>"))

(defun org-export-website-src-block (src-block contents _info)
  (let* ((language (org-element-property :language src-block))
         (code-lines (split-string (org-element-property :value src-block) "\n"))
         (indent (string-match "\\w" (nth 0 code-lines))))
    (concat "<pre><code class=\"language-" language "\">"
            (mapconcat
             (lambda (line)
               (if (string-empty-p line)
                   line
                 (substring line indent)))
             code-lines
             "\n")
            "</code></pre>")))

(defun org-export-website-footnote-reference (footnote-reference contents info)
  (let* ((footnotes (plist-get info :footnotes))
         (footnote-number (+ 1 (length footnotes))))
    (plist-put info :footnotes
               (append footnotes
                       (list
                        (concat "<a href=\"#fnref-" (number-to-string footnote-number) "\" id=\"fn-" (number-to-string footnote-number) "\">"
                                "[" (number-to-string footnote-number) "]"
                                "</a> "
                                contents))))
    (concat "<sup>"
            "<a href=\"#fn-" (number-to-string footnote-number) "\" id=\"fnref-" (number-to-string footnote-number) "\">"
            "[" (number-to-string footnote-number) "]"
            "</a>"
            "</sup>")))

(defun org-export-website-plain-list (plain-list contents _info)
  (let* ((type (org-element-property :type plain-list))
         (tag (if (string= type "ordered") "ol" "ul")))
    (concat "<" tag ">\n"
            contents
            "</" tag ">")))

(defun org-export-website-item (item contents _info)
  (concat "<li>"
          (string-trim contents "\\(<p>\\|[ \t\n\r]\\)+" "\\(</p>\\|[ \t\n\r]\\)+")
          "</li>\n"))

(defun replace-placeholder-with-indent (placeholder replacement string)
  (let* ((placeholder-indent (regexp-match-column placeholder string))
         (replacement-lines (split-string replacement "\n"))
         (first-replacement-line (car replacement-lines))
         (remaining-replacement-lines (cdr replacement-lines)))
    (replace-regexp-in-string placeholder
                              (concat first-replacement-line "\n"
                                      (indent-html-string (mapconcat 'identity remaining-replacement-lines "\n")
                                                     placeholder-indent))
                              string)))

(defun regexp-match-column (regexp string)
  (let* ((lines (split-string string "\n"))
         (matched-column (seq-find 'identity (seq-map (lambda (line) (string-match-p regexp line)) lines))))
    matched-column))

(defun indent-html-string (string n)
  (let ((lines (split-string string "\n"))
        (inside-code-block nil))
    (mapconcat (lambda (line)
                 (cond
                  ((string-empty-p line) "")
                  (inside-code-block
                    (when (string-match-p "</code>" line)
                      (setq inside-code-block nil))
                    line)
                  (t (setq inside-code-block (string-match-p "<code" line))
                     (concat (make-string n ?\ ) line))))
               lines "\n")))

; For debugging
(defun print-element (element)
  (print (org-element--format-element element)))
