;; Test for load-path configuration

;; Current load-path
(message "Initial load-path:")
(dolist (path load-path)
  (message "  %s" path))

;; Test function to add subdirectories
(defun add-subdirs-to-load-path (base-dir)
  "Add BASE-DIR and its subdirectories to `load-path'."
  (add-to-list 'load-path base-dir)
  (dolist (dir (directory-files base-dir t))
    (when (and (file-directory-p dir)
               (not (string-match "\\`\\." (file-name-nondirectory dir))))
      (add-to-list 'load-path dir))))

;; Add our subdirectories
(let ((base-dir "/home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager"))
  (add-subdirs-to-load-path base-dir))

;; Show updated load-path
(message "\nUpdated load-path:")
(dolist (path load-path)
  (message "  %s" path))

;; Try to load modules
(message "\nTrying to load modules...")
(condition-case err
    (progn
      (require 'etm-core-variables)
      (message "etm-core-variables loaded successfully"))
  (error (message "Error loading etm-core-variables: %S" err)))

(condition-case err
    (progn
      (require 'etm-core-helpers)
      (message "etm-core-helpers loaded successfully"))
  (error (message "Error loading etm-core-helpers: %S" err)))