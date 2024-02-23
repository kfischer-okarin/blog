(defun log-message (message)
  (princ (concat message "\n")))

(defun shell-command-with-echo (command)
  (log-message (concat "$ " command))
  (shell-command command))

(defun read-file-as-string (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun write-string-to-file (string file)
  (with-temp-buffer
    (insert string)
    (write-file file)))

; For straight.el support
(add-to-list 'load-path (expand-file-name "straight/build/ob-mermaid" user-emacs-directory))

(package-initialize)

(load-file "./src/export-backend.el")

(defun export-article ()
  (let ((org-babel-load-languages
         '((mermaid . t)))
        (org-confirm-babel-evaluate
         (lambda (lang body) (not (string= lang "mermaid"))))
        (org-export-website-template-string
         (read-file-as-string "templates/index.html")))
    (org-export-as org-export-website-backend)))

(shell-command-with-echo "git clean -fx ./dist")
(shell-command-with-echo "git clean -fx ./images")

(log-message "Building site...")

(let ((enable-local-variables :all))
  (find-file "articles.org"))

(write-string-to-file
 (export-article)
  "./dist/index.html")

(log-message "Copying static files...")
(shell-command-with-echo "cp -r ./static/* ./dist/")

(log-message "Copying images...")
(shell-command-with-echo "cp -r ./images ./dist/")

(log-message "Copying videos...")
(shell-command-with-echo "cp -r ./videos ./dist/")

(log-message "Done!")
