(load-file "./src/export-backend.el")

(print "Building site...")

(let ((enable-local-variables :all))
  (find-file "articles.org"))

(let ((output (org-export-as org-export-website-backend)))
  (with-temp-buffer
    (insert output)
    (write-file "./dist/index.html")))
