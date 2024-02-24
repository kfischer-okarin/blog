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
                                                     (item . org-export-website-item))
                                      :options '((:media-path nil nil "."))))

(defun org-export-website-template (contents info)
  (replace-placeholders
   (plist-get info :page-template)
   "{{ content }}" contents
   "{{ stylesheet-path }}" (plist-get info :stylesheet-path)
   "{{ title }}" (plist-get info :title)
   "{{ description }}" (plist-get info :description)))

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

(defun org-export-website-paragraph (paragraph contents info)
  (let ((description (plist-get info :description)))
    (unless description
      (plist-put info :description (remove-html-tags (shorten-and-normalize-whitespace contents)))))
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

(defun org-export-website-link--image (link contents info)
  (let* ((path (file-name-concat (plist-get info :media-path) (org-element-property :path link)))
         (attributes (if contents (concat " alt=\"" contents "\"") )))
    (concat "<img src=\"" path "\"" attributes " />")))

(defun org-export-website-link--video (link contents _info)
  (let* ((path (file-name-concat (plist-get _info :media-path) (org-element-property :path link))))
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

;;; Blog Index Page

(defconst org-export-blog-index-backend (org-export-create-backend
                                         :transcoders '((template . org-export-blog-index-template)
                                                        (headline . org-export-blog-index-headline))))

(defun org-export-blog-index-template (_contents info)
  (let* ((newest-to-oldest-headlines
          (sort (plist-get info :headlines)
                (lambda (a b)
                  (time-less-p
                   (plist-get b :published-at)
                   (plist-get a :published-at)))))
         (contents (concat "<ul>"
                           (mapconcat (lambda (headline) (plist-get headline :html))
                                      newest-to-oldest-headlines
                                      "\n")
                           "</ul>")))
    (org-export-website-template contents info)))

(defun org-export-blog-index-headline (headline _contents info)
  (let* ((post-id (org-element-property :POST_ID headline)))
    (when post-id
      (let* ((title (org-element-property :raw-value headline))
             (published-at (org-time-string-to-time (org-element-property :PUBLISHED_AT headline)))
             (published-at-string (format-time-string "%Y-%m-%d" published-at))
             (headlines (plist-get info :headlines)))
        (plist-put info :headlines
                   (cons (list
                          :published-at published-at
                          :html (concat "<li>"
                                        "<a href=\"posts/" post-id "\">"
                                        title
                                        "</a>"
                                        "<br />"
                                        "<time datetime=\"" published-at-string "\">"
                                        published-at-string
                                        "</time>"
                                        "</li>"))
                         headlines))
        ""))))

;;; Helper functions

(defun replace-placeholders (template &rest replacements)
  (while replacements
    (setq template (replace-regexp-in-string (car replacements) (cadr replacements) template))
    (setq replacements (cddr replacements)))
  template)

(defun remove-html-tags (string)
  (replace-regexp-in-string "<[^>]+>" "" string))

(defun shorten-and-normalize-whitespace (string)
  (replace-regexp-in-string "[ \n\t]+" " " string))

; For debugging
(defun print-element (element)
  (print (org-element--format-element element)))
