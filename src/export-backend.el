(require 'ox)

(defconst org-export-website-backend (org-export-create-backend
                                      :transcoders '((template . org-export-website-template)
                                                     (headline . org-export-website-headline)
                                                     (section . org-export-website-section)
                                                     (paragraph . org-export-website-paragraph)
                                                     (link . org-export-website-link))))


(defun org-export-website-template (contents info)
  (let* ((template (with-temp-buffer
                    (insert-file-contents "templates/index.html")
                    (buffer-string)))
         (contents-placeholder-indent (regexp-match-column "{{ content }}" template)))
    (replace-placeholder-with-indent "{{ content }}" contents template)))

(defun org-export-website-headline (headline contents _info)
  (let* ((level (org-element-property :level headline))
         (title (org-element-property :raw-value headline))
         (html-heading (format "<h%d>%s</h%d>" level title level)))
    (if contents
        (concat html-heading "\n\n" contents)
      html-heading)))

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
      (t
       (org-export-website-link--fallback link contents _info)))))

(defun org-export-website-link--image (link contents _info)
  (let* ((type (org-element-property :type link))
         (path (org-element-property :path link))
         (attributes (if contents (concat " alt=\"" contents "\"") )))
    (concat "<img src=\"" path "\"" attributes " />")))

(defun org-export-website-link--fallback (link contents _info)
  (let* ((type (org-element-property :type link))
         (url (org-element-property :raw-link link))
         (description (or contents url)))
    (concat "<a href=\"" url "\">" description "</a>")))

(defun replace-placeholder-with-indent (placeholder replacement string)
  (let* ((placeholder-indent (regexp-match-column placeholder string))
         (replacement-lines (split-string replacement "\n"))
         (first-replacement-line (car replacement-lines))
         (remaining-replacement-lines (cdr replacement-lines)))
    (replace-regexp-in-string placeholder
                              (concat first-replacement-line "\n"
                                      (indent-string (mapconcat 'identity remaining-replacement-lines "\n")
                                                     placeholder-indent))
                              string)))

(defun regexp-match-column (regexp string)
  (let* ((lines (split-string string "\n"))
         (matched-column (seq-find 'identity (seq-map (lambda (line) (string-match-p regexp line)) lines))))
    matched-column))

(defun indent-string (string n)
  (let ((lines (split-string string "\n")))
    (mapconcat (lambda (line)
                  (if (string-empty-p line)
                      ""
                    (concat (make-string n ?\s) line)))
               lines
               "\n")))

; For debugging
(defun print-element (element)
  (print (org-element--format-element element)))
