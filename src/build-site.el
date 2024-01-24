(load-file "./src/export-backend.el")

(defun log-message (message)
  (princ (concat message "\n")))

(defun shell-command-with-echo (command)
  (log-message (concat "$ " command))
  (shell-command command))

(shell-command-with-echo "rm -rf ./dist/*")

(log-message "Building site...")

(let ((enable-local-variables :all))
  (find-file "articles.org"))

(let ((output (org-export-as org-export-website-backend)))
  (with-temp-buffer
    (insert output)
    (write-file "./dist/index.html")))

(shell-command-with-echo "cp -r ./static/* ./dist/")
