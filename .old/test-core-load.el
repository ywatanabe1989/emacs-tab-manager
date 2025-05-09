;; Tests for etm-core module loading

;; Add necessary paths
(let ((this-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path this-dir)
  (add-to-list 'load-path (expand-file-name "etm-core" this-dir)))

(message "Testing etm-core-variables loading...")
(condition-case err
    (progn
      (require 'etm-core-variables)
      (message "etm-core-variables loaded successfully")
      (message "etm-version: %s" etm-version))
  (error (message "Error loading etm-core-variables: %S" err)))

(message "Testing etm-core-helpers loading...")
(condition-case err
    (progn
      (require 'etm-core-helpers)
      (message "etm-core-helpers loaded successfully")
      (message "--my/ssh-select-host function exists: %s" 
               (if (fboundp '--my/ssh-select-host) "YES" "NO")))
  (error (message "Error loading etm-core-helpers: %S" err)))

(message "Testing etm-core module loading...")
(condition-case err
    (progn
      (require 'etm-core)
      (message "etm-core loaded successfully"))
  (error (message "Error loading etm-core: %S" err)))