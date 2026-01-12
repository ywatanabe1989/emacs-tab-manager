;;; test-etm-remote-indicators.el --- Tests for ETM remote visual indicators -*- coding: utf-8; lexical-binding: t -*-

;; Author: Yuki Watanabe
;; Date: 2025-01-13
;; Version: 1.0.0

;;; Commentary:
;; Test suite for ETM remote visual indicators functionality.
;; Tests tab name enhancement, buffer prefixing, and mode line indicators.

;;; Code:

(add-to-list 'load-path (expand-file-name "../.." (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../etm-core" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../etm-remote" (file-name-directory load-file-name)))

(require 'ert)
(require 'cl-lib)
(require 'etm-core-variables)
(require 'etm-remote-connection)

;; Mock functions for testing
(defvar test-etm-remote-indicators-original-functions nil
  "Store original functions for restoration.")

(defvar test-etm-remote-indicators-tab-name nil
  "Mock tab name for testing.")

(defvar test-etm-remote-indicators-buffer-name nil
  "Mock buffer name for testing.")

(defun test-etm-remote-indicators-setup ()
  "Set up test environment."
  ;; Save original functions
  (setq test-etm-remote-indicators-original-functions
        (list (cons 'tab-bar--current-tab (symbol-function 'tab-bar--current-tab))
              (cons 'buffer-name (symbol-function 'buffer-name))
              (cons 'tab-bar-rename-tab (symbol-function 'tab-bar-rename-tab))))
  
  ;; Mock tab-bar functions - use a fixed tab name for consistency
  (fset 'tab-bar--current-tab
        (lambda () '((name . "test-tab"))))
  
  (fset 'tab-bar-rename-tab
        (lambda (name)
          (setq test-etm-remote-indicators-tab-name name)))
  
  ;; Mock buffer functions
  (fset 'buffer-name
        (lambda (&optional buffer)
          test-etm-remote-indicators-buffer-name))
  
  ;; Initialize test variables
  (setq test-etm-remote-indicators-tab-name "test-tab")
  (setq test-etm-remote-indicators-buffer-name "test-buffer")
  (setq etm-remote-connections (make-hash-table :test 'equal))
  (setq etm-remote-global-connections (make-hash-table :test 'equal))
  ;; Initialize the tab connections hash
  (puthash "test-tab" (make-hash-table :test 'equal) etm-remote-connections))

(defun test-etm-remote-indicators-teardown ()
  "Tear down test environment."
  ;; Restore original functions
  (dolist (func test-etm-remote-indicators-original-functions)
    (when (cdr func)
      (fset (car func) (cdr func))))
  (setq test-etm-remote-indicators-original-functions nil))

(ert-deftest test-etm-remote-indicators-enhance-tab-name ()
  "Test enhancing tab name with remote host information."
  (test-etm-remote-indicators-setup)
  (unwind-protect
      (progn
        ;; Load the module we're testing
        (require 'etm-remote-indicators)
        
        ;; Create a remote connection
        (let ((conn (etm-remote-connect "ssh" "user" "example.com")))
          (setf (etm-remote-connection-status conn) :connected)
          
          ;; Test tab name enhancement
          (etm-remote-enhance-tab-name)
          (should (string-match "@example\\.com" test-etm-remote-indicators-tab-name))
          
          ;; Test with multiple connections
          (let ((conn2 (etm-remote-connect "ssh" "user" "other.com")))
            (setf (etm-remote-connection-status conn2) :connected)
            (etm-remote-enhance-tab-name)
            (should (string-match "\\[2 hosts\\]" test-etm-remote-indicators-tab-name)))))
    (test-etm-remote-indicators-teardown)))

(ert-deftest test-etm-remote-indicators-prefix-buffer-name ()
  "Test prefixing buffer names with remote host information."
  (test-etm-remote-indicators-setup)
  (unwind-protect
      (progn
        (require 'etm-remote-indicators)
        
        ;; Test local buffer (no prefix)
        (should (equal (etm-remote-prefix-buffer-name "local-file.txt")
                       "local-file.txt"))
        
        ;; Test remote buffer
        (with-temp-buffer
          (setq default-directory "/ssh:user@example.com:/home/user/")
          ;; Mock file-remote-p to return expected values
          (cl-letf (((symbol-function 'file-remote-p)
                     (lambda (file &optional identification)
                       (if identification
                           (if (eq identification 'host)
                               "example.com"
                             nil)
                         "/ssh:user@example.com:"))))
            (should (string-match "^\\[example\\.com\\]" 
                                  (etm-remote-prefix-buffer-name "remote-file.txt"))))))
    (test-etm-remote-indicators-teardown)))

(ert-deftest test-etm-remote-indicators-mode-line ()
  "Test mode line indicator for remote connections."
  (test-etm-remote-indicators-setup)
  (unwind-protect
      (progn
        (require 'etm-remote-indicators)
        
        ;; Test with no connections
        (should (equal (etm-remote-mode-line-indicator) ""))
        
        ;; Test with connected host
        (let ((conn (etm-remote-connect "ssh" "user" "example.com")))
          (setf (etm-remote-connection-status conn) :connected)
          (let ((indicator (etm-remote-mode-line-indicator)))
            (should (string-match "R:" indicator))
            (should (string-match "example\\.com" indicator))))
        
        ;; Test with error status
        (let ((conn (etm-remote-get-connection "example.com")))
          (setf (etm-remote-connection-status conn) :error)
          (let ((indicator (etm-remote-mode-line-indicator)))
            (should (string-match "!" indicator)))))
    (test-etm-remote-indicators-teardown)))

(ert-deftest test-etm-remote-indicators-color-coding ()
  "Test color coding for remote connection status."
  (test-etm-remote-indicators-setup)
  (unwind-protect
      (progn
        (require 'etm-remote-indicators)
        
        ;; Test color for connected status
        (should (equal (etm-remote-get-status-color :connected) "green"))
        
        ;; Test color for connecting status
        (should (equal (etm-remote-get-status-color :connecting) "yellow"))
        
        ;; Test color for error status
        (should (equal (etm-remote-get-status-color :error) "red"))
        
        ;; Test color for disconnected status
        (should (equal (etm-remote-get-status-color :disconnected) "gray")))
    (test-etm-remote-indicators-teardown)))

(ert-deftest test-etm-remote-indicators-integration ()
  "Test integration of visual indicators with ETM."
  (test-etm-remote-indicators-setup)
  (unwind-protect
      (progn
        (require 'etm-remote-indicators)
        
        ;; Test initialization
        (etm-remote-indicators-init)
        (should (member etm-remote-mode-line-format mode-line-misc-info))
        
        ;; Test cleanup
        (etm-remote-indicators-cleanup)
        (should-not (member etm-remote-mode-line-format mode-line-misc-info)))
    (test-etm-remote-indicators-teardown)))

;; Run tests if executed directly
;; (when (and (boundp 'load-file-name) load-file-name)
;;   (ert-run-tests-batch-and-exit))

(provide 'test-etm-remote-indicators)
;;; test-etm-remote-indicators.el ends here

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-remote/etm-remote-indicators.el
;; --------------------------------------------------------------------------------
;; ;;; etm-remote-indicators.el --- Visual indicators for ETM remote connections -*- coding: utf-8; lexical-binding: t -*-
;; 
;; ;; Author: Yuki Watanabe
;; ;; Date: 2025-01-13
;; ;; Version: 1.0.0
;; 
;; ;;; Commentary:
;; ;; This module provides visual indicators for remote connections in ETM.
;; ;; It enhances tab names, prefixes buffer names, and provides mode line
;; ;; indicators to show remote connection status.
;; 
;; ;;; Code:
;; 
;; (require 'cl-lib)
;; (require 'etm-remote-connection)
;; 
;; (defvar etm-remote-show-host-in-tab t
;;   "Whether to show remote host information in tab names.")
;; 
;; (defvar etm-remote-show-host-in-buffer t
;;   "Whether to prefix buffer names with remote host information.")
;; 
;; (defvar etm-remote-show-mode-line t
;;   "Whether to show remote connection status in mode line.")
;; 
;; (defvar etm-remote-status-colors
;;   '((:connected . "green")
;;     (:connecting . "yellow")
;;     (:error . "red")
;;     (:disconnected . "gray"))
;;   "Color mapping for connection status.")
;; 
;; (defun etm-remote-get-status-color (status)
;;   "Get color for connection STATUS."
;;   (or (cdr (assq status etm-remote-status-colors)) "default"))
;; 
;; (defun etm-remote-enhance-tab-name ()
;;   "Enhance current tab name with remote host information."
;;   (when etm-remote-show-host-in-tab
;;     (let ((connections (etm-remote--get-tab-connections))
;;           (active-hosts '()))
;;       ;; Collect active hosts
;;       (maphash (lambda (host conn)
;;                  (when (eq (etm-remote-connection-status conn) :connected)
;;                    (push host active-hosts)))
;;                connections)
;;       ;; Update tab name if there are active connections
;;       (when active-hosts
;;         (let* ((current-tab (tab-bar--current-tab))
;;                (current-name (or (alist-get 'name current-tab) ""))
;;                (base-name (replace-regexp-in-string 
;;                            " \\[@[^]]+\\]\\|\\[[0-9]+ hosts\\]" "" current-name))
;;                (suffix (if (= (length active-hosts) 1)
;;                            (format " [@%s]" (car active-hosts))
;;                          (format " [%d hosts]" (length active-hosts)))))
;;           (message "[ETM DEBUG] etm-remote-enhance-tab-name: Modifying tab '%s' -> '%s' (hosts: %s)"
;;                    current-name (concat base-name suffix) active-hosts)
;;           (tab-bar-rename-tab (concat base-name suffix)))))))
;; 
;; (defun etm-remote-prefix-buffer-name (buffer-name)
;;   "Prefix BUFFER-NAME with remote host information if applicable."
;;   (if (and etm-remote-show-host-in-buffer
;;            (boundp 'default-directory)
;;            default-directory)
;;       (let ((remote-id (file-remote-p default-directory)))
;;         (if remote-id
;;             (let ((host (file-remote-p default-directory 'host)))
;;               (if host
;;                   (progn
;;                     (message "[ETM DEBUG] etm-remote-prefix-buffer-name: Adding prefix to buffer '%s' -> '[%s] %s'"
;;                              buffer-name host buffer-name)
;;                     (format "[%s] %s" host buffer-name))
;;                 buffer-name))
;;           buffer-name))
;;     buffer-name))
;; 
;; (defun etm-remote-mode-line-indicator ()
;;   "Generate mode line indicator for remote connections."
;;   (if etm-remote-show-mode-line
;;       (let ((connections (etm-remote--get-tab-connections))
;;             (indicators '()))
;;         (maphash (lambda (host conn)
;;                    (let* ((status (etm-remote-connection-status conn))
;;                           (color (etm-remote-get-status-color status))
;;                           (symbol (cond ((eq status :connected) "●")
;;                                         ((eq status :connecting) "○")
;;                                         ((eq status :error) "!")
;;                                         (t "◌"))))
;;                      (push (format "%s%s" symbol host) indicators)))
;;                  connections)
;;         (if indicators
;;             (format " R:%s" (mapconcat 'identity (nreverse indicators) ","))
;;           ""))
;;     ""))
;; 
;; (defvar etm-remote-mode-line-format
;;   '(:eval (etm-remote-mode-line-indicator))
;;   "Mode line format for remote indicators.")
;; 
;; (defun etm-remote-indicators-init ()
;;   "Initialize remote visual indicators."
;;   (interactive)
;;   ;; Add to mode line
;;   (unless (memq 'etm-remote-mode-line-indicator mode-line-misc-info)
;;     (push etm-remote-mode-line-format mode-line-misc-info))
;;   
;;   ;; Set up hooks for tab name enhancement
;;   (add-hook 'etm-remote-connection-change-hook 'etm-remote-enhance-tab-name)
;;   
;;   ;; Set up advice for buffer naming
;;   (advice-add 'rename-buffer :filter-args 'etm-remote--rename-buffer-advice)
;;   (advice-add 'set-visited-file-name :after 'etm-remote--update-buffer-name)
;;   
;;   (message "ETM remote indicators initialized"))
;; 
;; (defun etm-remote-indicators-cleanup ()
;;   "Clean up remote visual indicators."
;;   (interactive)
;;   ;; Remove from mode line
;;   (setq mode-line-misc-info 
;;         (delq etm-remote-mode-line-format mode-line-misc-info))
;;   
;;   ;; Remove hooks
;;   (remove-hook 'etm-remote-connection-change-hook 'etm-remote-enhance-tab-name)
;;   
;;   ;; Remove advice
;;   (advice-remove 'rename-buffer 'etm-remote--rename-buffer-advice)
;;   (advice-remove 'set-visited-file-name 'etm-remote--update-buffer-name)
;;   
;;   (message "ETM remote indicators cleaned up"))
;; 
;; (defun etm-remote--rename-buffer-advice (args)
;;   "Advice for `rename-buffer' to add remote prefix.
;; ARGS are the original arguments to rename-buffer."
;;   (let ((newname (car args)))
;;     (let ((prefixed-name (etm-remote-prefix-buffer-name newname)))
;;       (unless (string= newname prefixed-name)
;;         (message "[ETM DEBUG] etm-remote--rename-buffer-advice: Renaming buffer '%s' -> '%s'"
;;                  newname prefixed-name))
;;       (cons prefixed-name (cdr args)))))
;; 
;; (defun etm-remote--update-buffer-name (&rest _)
;;   "Update current buffer name with remote prefix if needed."
;;   (when (and buffer-file-name
;;              (file-remote-p buffer-file-name))
;;     (let ((base-name (file-name-nondirectory buffer-file-name)))
;;       (let ((prefixed-name (etm-remote-prefix-buffer-name base-name)))
;;         (message "[ETM DEBUG] etm-remote--update-buffer-name: Updating buffer name '%s' -> '%s' (file: %s)"
;;                  base-name prefixed-name buffer-file-name)
;;         (rename-buffer prefixed-name t)))))
;; 
;; (provide 'etm-remote-indicators)
;; ;;; etm-remote-indicators.el ends here
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-remote/etm-remote-indicators.el
;; --------------------------------------------------------------------------------

;;; test-etm-remote-indicators.el ends here
