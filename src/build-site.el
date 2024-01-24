(require 'ox)

(print "Building site...")

(defun kf-website-template (contents info)
  (concat "Hello, world at " (format-time-string "%Y-%m-%d %H:%M:%S") "."))


(let ((export-backend (org-export-create-backend
                       :transcoders '((template . kf-website-template)))))

  (find-file "articles.org")
  (let ((output (org-export-as export-backend)))
    (with-temp-buffer
      (insert output)
      (write-file "../dist/index.html"))))
