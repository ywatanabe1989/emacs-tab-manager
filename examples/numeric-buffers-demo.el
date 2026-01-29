;;; numeric-buffers-demo.el --- Demo of ETM numeric buffer system
;;; Author: ywatanabe
;;; Commentary:
;; This file demonstrates the numeric buffer system in ETM

;;; Code:

;; Example 1: Manual registration workflow
(defun etm-demo-manual-registration ()
  "Demonstrate manual buffer registration."
  (interactive)
  ;; Create some buffers
  (switch-to-buffer "*demo-main*")
  (insert "This is the main buffer\n")
  (etm-numeric-register-current-buffer)  ; Register as #1
  
  (switch-to-buffer "*demo-test*")
  (insert "This is the test buffer\n")
  (etm-numeric-register-current-buffer)  ; Register as #2
  
  (switch-to-buffer "*demo-docs*")
  (insert "This is the docs buffer\n")
  (etm-numeric-register-current-buffer)  ; Register as #3
  
  ;; Show what we have
  (etm-numeric-list-buffers)
  
  (message "Now try: M-t 1, M-t 2, M-t 3 to jump between buffers!"))

;; Example 2: Custom layout with auto-registration
(defun etm-demo-layout-with-auto-register ()
  "Create a demo layout that will auto-register buffers."
  (interactive)
  (let ((tab-name "demo-auto")
        (specs '((file "~/.emacs.d/init.el" 0 0 80 24)
                 (file "~/.bashrc" 80 0 80 24)
                 (file "~/.profile" 0 24 80 24))))
    ;; This will automatically register all files (up to 9 by default)
    (--etm-layout-create-from-positions tab-name specs)
    (message "Files auto-registered! Try M-t 1, M-t 2, M-t 3")))

;; Example 3: Temporarily disable auto-registration
(defun etm-demo-layout-without-auto-register ()
  "Create a layout without auto-registration."
  (interactive)
  (let ((etm-layout-auto-register-numeric nil)  ; Temporarily disable
        (tab-name "demo-manual")
        (specs '((file "~/.emacs.d/init.el" 0 0 80 24))))
    (--etm-layout-create-from-positions tab-name specs)
    (message "No auto-registration. Use M-t b r to register manually.")))

;; Example 4: Check numeric buffer status
(defun etm-demo-check-numeric-status ()
  "Show the current numeric buffer configuration."
  (interactive)
  (let ((tab-name (alist-get 'name (tab-bar--current-tab))))
    (message "=== Numeric Buffer Status for tab '%s' ===" tab-name)
    (message "Auto-registration enabled: %s" etm-layout-auto-register-numeric)
    (message "Max auto-register: %d" etm-layout-auto-register-max)
    (message "Max numeric buffers: %d" etm-max-numeric-buffers)
    (etm-numeric-list-buffers)))

;; Interactive demo menu
(defun etm-numeric-demo ()
  "Interactive demo of numeric buffer features."
  (interactive)
  (let ((choice (completing-read
                 "Choose demo: "
                 '("Manual Registration"
                   "Auto-Registration Layout"
                   "No Auto-Registration Layout"
                   "Check Status"
                   "Quick Start Help")
                 nil t)))
    (cond
     ((string= choice "Manual Registration")
      (etm-demo-manual-registration))
     ((string= choice "Auto-Registration Layout")
      (etm-demo-layout-with-auto-register))
     ((string= choice "No Auto-Registration Layout")
      (etm-demo-layout-without-auto-register))
     ((string= choice "Check Status")
      (etm-demo-check-numeric-status))
     ((string= choice "Quick Start Help")
      (etm-numeric-quick-start)))))

(provide 'numeric-buffers-demo)

;;; numeric-buffers-demo.el ends here