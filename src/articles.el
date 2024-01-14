(defun kf-org-current-headline-in-snakecase ()
  (save-excursion
    (org-back-to-heading)
    (let ((headline (nth 4 (org-heading-components))))
      (replace-regexp-in-string
       " " "_"
       (replace-regexp-in-string
        "[^[:alnum:] ]" ""
        (downcase headline))))))
