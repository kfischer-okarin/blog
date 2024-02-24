; For straight.el support
(add-to-list 'load-path (expand-file-name "straight/build/ob-mermaid" user-emacs-directory))

(package-initialize)

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

(defun export-article ()
  (interactive)
  (let* ((article-slug (org-entry-get (point) "POST_ID"))
         ; GitHub Pages will automatically redirect from /xyz to /xyz.html - just need to make sure the links are
         ; correct (i.e. not including the .html extension)
         (output-filename (concat "./dist/posts/" article-slug ".html")))
    (log-message (concat "Exporting " output-filename "..."))
    (if (file-exists-p output-filename)
        (error (concat output-filename " already exists!")))
    (save-restriction
      (org-narrow-to-subtree)
      (let ((org-babel-load-languages
             '((mermaid . t)))
            (org-confirm-babel-evaluate
             (lambda (lang body) (not (string= lang "mermaid")))))
        (write-string-to-file
         (org-export-as org-export-website-backend nil nil nil
                        `(:page-template
                          ,(read-file-as-string "templates/index.html")
                          :stylesheet-path
                          "../styles.css"
                          :media-path
                          "../"))
         output-filename)))))

(defun export-all-articles ()
  (org-map-entries
   #'export-article
   "POST_ID<>\"\""
   'file))

(defun export-index ()
  (write-string-to-file
   (org-export-as org-export-blog-index-backend nil nil nil
                  `(:page-template
                    ,(read-file-as-string "templates/index.html")
                    :stylesheet-path
                    "styles.css"))
   "./dist/index.html"))

(load-file "./src/export-backend.el")

(shell-command-with-echo "git clean -fx ./dist")
(shell-command-with-echo "git clean -fx ./images")

(log-message "Building site...")

(let ((enable-local-variables :all))
  (find-file "articles.org"))

(export-all-articles)

(export-index)

(log-message "Copying static files...")
(shell-command-with-echo "cp -r ./static/* ./dist/")

(log-message "Copying images...")
(shell-command-with-echo "cp -r ./images ./dist/")

(log-message "Copying videos...")
(shell-command-with-echo "cp -r ./videos ./dist/")

(log-message "Done!")
