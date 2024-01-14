(defun kf-org-current-headline-in-snakecase ()
  (save-excursion
    (org-back-to-heading)
    (let ((headline (nth 4 (org-heading-components))))
      (replace-regexp-in-string
       " " "_"
       (replace-regexp-in-string
        "[^[:alnum:] ]" ""
        (downcase headline))))))

(defun kf-org-babel-src-block-body-hash-abbrev ()
  (let* ((src-block-body (org-element-property :value (org-element-at-point)))
         (hash (sha1 src-block-body)))
    (substring hash 0 8)))

(defun kf-mermaid-result-filename ()
  (concat "images/generated/"
          (kf-org-current-headline-in-snakecase)
          "-"
          (kf-org-babel-src-block-body-hash-abbrev)
          ".svg"))
