(require 'ox)

(defconst org-export-website-backend (org-export-create-backend
                                      :transcoders '((template . org-export-website-template)
                                                     (headline . org-export-website-headline))))

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
