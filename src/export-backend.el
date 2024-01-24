(require 'ox)

(defconst org-export-website-backend (org-export-create-backend
                                      :transcoders '((template . org-export-website-template)
                                                     (headline . org-export-website-headline))))

(defun org-export-website-template (contents info)
  (let ((template (with-temp-buffer
                    (insert-file-contents "templates/index.html")
                    (buffer-string))))
    (replace-regexp-in-string "{{ content }}" contents template)))

(defun org-export-website-headline (headline contents _info)
  (let* ((level (org-element-property :level headline))
         (title (org-element-property :raw-value headline))
         (html-heading (format "<h%d>%s</h%d>" level title level)))
    (if contents
        (concat html-heading "\n\n" contents)
      html-heading)))
