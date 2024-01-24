(require 'ox)

(defconst org-export-website-backend (org-export-create-backend
                                      :transcoders '((template . org-export-website-template))))

(defun org-export-website-template (contents info)
  (concat "Hello, world at " (format-time-string "%Y-%m-%d %H:%M:%S") "."))
