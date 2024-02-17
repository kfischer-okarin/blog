; For straight.el support
(add-to-list 'load-path (expand-file-name "straight/build/ob-mermaid" user-emacs-directory))

(package-initialize)

(setq org-babel-load-languages
      '((mermaid . t)))

(setq org-confirm-babel-evaluate
      (lambda (lang body) (not (string= lang "mermaid"))))

(load-file "./src/export-backend.el")

(defun log-message (message)
  (princ (concat message "\n")))

(defun shell-command-with-echo (command)
  (log-message (concat "$ " command))
  (shell-command command))

(shell-command-with-echo "git clean -fx ./dist")
(shell-command-with-echo "git clean -fx ./images")

(log-message "Building site...")

(let ((enable-local-variables :all))
  (find-file "articles.org"))

(let* ((org-export-website-template-string
        (with-temp-buffer
          (insert-file-contents "templates/index.html")
          (buffer-string)))
       (output (org-export-as org-export-website-backend)))
  (with-temp-buffer
    (insert output)
    (write-file "./dist/index.html")))

(log-message "Copying static files...")
(shell-command-with-echo "cp -r ./static/* ./dist/")

(log-message "Copying images...")
(shell-command-with-echo "cp -r ./images ./dist/")

(log-message "Copying videos...")
(shell-command-with-echo "cp -r ./videos ./dist/")

(log-message "Done!")
