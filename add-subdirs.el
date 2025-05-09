;; Add subdirectories to load-path
(defun add-subdirs-to-load-path (base-dir)
  "Add BASE-DIR and its subdirectories to `load-path'."
  (add-to-list 'load-path base-dir)
  (dolist (dir (directory-files base-dir t))
    (when (and (file-directory-p dir)
               (not (string-match "\\`\\." (file-name-nondirectory dir))))
      (add-to-list 'load-path dir))))

;; Add project root and subdirectories
(let ((project-root (file-name-directory load-file-name)))
  (add-subdirs-to-load-path project-root))