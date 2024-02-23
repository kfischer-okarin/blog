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
  (let ((org-babel-load-languages
         '((mermaid . t)))
        (org-confirm-babel-evaluate
         (lambda (lang body) (not (string= lang "mermaid")))))
    (save-restriction
      (org-narrow-to-subtree)
      (org-export-as org-export-website-backend nil nil nil
                     `(:page-template
                       ,(read-file-as-string "templates/index.html")
                       :stylesheet-path
                       "../styles.css"
                       :media-path
                       "../")))))

(defun export-all-articles ()
  (org-map-entries
   (lambda ()
     (let* ((article-slug (org-entry-get (point) "POST_ID"))
            (published-at (org-time-string-to-time (org-entry-get (point) "PUBLISHED_AT")))
            ; GitHub Pages will automatically redirect from /xyz to /xyz.html - just need to make sure the links are
            ; correct (i.e. not including the .html extension)
            (filename (concat "./dist/posts/" article-slug ".html")))
       (log-message (concat "Exporting " filename "..."))
       (if (file-exists-p filename)
           (error (concat filename " already exists!")))
       (write-string-to-file (export-article) filename)
       '(:article-slug article-slug :published-at published-at)))
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
