;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-24 16:40:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/etm-buffer/test-etm-buffer-numeric.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for numeric buffer system

(require 'ert)
(require 'etm-buffer-numeric)

;; Helper function to create a mock tab
(defun --test-setup-mock-tab (tab-name)
  "Setup a mock tab for testing."
  (setq etm-numeric-buffers nil)  ; Clear state
  ;; Mock tab-bar--current-tab to return our test tab
  (cl-letf (((symbol-function 'tab-bar--current-tab)
             (lambda () `((name . ,tab-name)))))
    tab-name))

;; Test basic functionality
;; ----------------------------------------

(ert-deftest test-etm-numeric-register-buffer ()
  "Test registering buffers with numeric IDs."
  (let ((tab-name "test-tab"))
    (--test-setup-mock-tab tab-name)
    
    ;; Test registering first buffer
    (should (equal 1 (--etm-numeric-register-buffer "buffer1" tab-name)))
    (should (equal "buffer1" (--etm-numeric-get-buffer-by-id 1 tab-name)))
    (should (equal 1 (--etm-numeric-get-id-by-buffer "buffer1" tab-name)))
    
    ;; Test registering second buffer
    (should (equal 2 (--etm-numeric-register-buffer "buffer2" tab-name)))
    (should (equal "buffer2" (--etm-numeric-get-buffer-by-id 2 tab-name)))
    
    ;; Test getting next available ID
    (should (equal 3 (--etm-numeric-get-next-id tab-name)))))

(ert-deftest test-etm-numeric-unregister-buffer ()
  "Test unregistering buffers."
  (let ((tab-name "test-tab"))
    (--test-setup-mock-tab tab-name)
    
    ;; Setup: register some buffers
    (--etm-numeric-register-buffer "buffer1" tab-name)
    (--etm-numeric-register-buffer "buffer2" tab-name)
    (--etm-numeric-register-buffer "buffer3" tab-name)
    
    ;; Test unregister by ID
    (--etm-numeric-unregister-buffer 2 tab-name)
    (should (null (--etm-numeric-get-buffer-by-id 2 tab-name)))
    (should (equal "buffer1" (--etm-numeric-get-buffer-by-id 1 tab-name)))
    (should (equal "buffer3" (--etm-numeric-get-buffer-by-id 3 tab-name)))
    
    ;; Test unregister by buffer name
    (--etm-numeric-unregister-buffer "buffer1" tab-name)
    (should (null (--etm-numeric-get-buffer-by-id 1 tab-name)))
    (should (equal "buffer3" (--etm-numeric-get-buffer-by-id 3 tab-name)))))

(ert-deftest test-etm-numeric-max-buffers ()
  "Test maximum buffer limit."
  (let ((tab-name "test-tab")
        (etm-max-numeric-buffers 3))
    (--test-setup-mock-tab tab-name)
    
    ;; Register up to maximum
    (should (equal 1 (--etm-numeric-register-buffer "buffer1" tab-name)))
    (should (equal 2 (--etm-numeric-register-buffer "buffer2" tab-name)))
    (should (equal 3 (--etm-numeric-register-buffer "buffer3" tab-name)))
    
    ;; Should fail to register beyond maximum
    (should (null (--etm-numeric-register-buffer "buffer4" tab-name)))
    (should (null (--etm-numeric-get-next-id tab-name)))))

(ert-deftest test-etm-numeric-multiple-tabs ()
  "Test numeric buffers work independently across tabs."
  (setq etm-numeric-buffers nil)  ; Clear state
  
  ;; Register buffers in different tabs
  (--etm-numeric-register-buffer "buffer1-tab1" "tab1")
  (--etm-numeric-register-buffer "buffer1-tab2" "tab2")
  (--etm-numeric-register-buffer "buffer2-tab1" "tab1")
  
  ;; Verify independence
  (should (equal "buffer1-tab1" (--etm-numeric-get-buffer-by-id 1 "tab1")))
  (should (equal "buffer2-tab1" (--etm-numeric-get-buffer-by-id 2 "tab1")))
  (should (equal "buffer1-tab2" (--etm-numeric-get-buffer-by-id 1 "tab2")))
  (should (null (--etm-numeric-get-buffer-by-id 2 "tab2"))))

(ert-deftest test-etm-numeric-id-reuse ()
  "Test that unregistered IDs can be reused."
  (let ((tab-name "test-tab"))
    (--test-setup-mock-tab tab-name)
    
    ;; Register and unregister
    (--etm-numeric-register-buffer "buffer1" tab-name)
    (--etm-numeric-register-buffer "buffer2" tab-name)
    (--etm-numeric-unregister-buffer 1 tab-name)
    
    ;; Next registration should reuse ID 1
    (should (equal 1 (--etm-numeric-register-buffer "buffer3" tab-name)))
    (should (equal "buffer3" (--etm-numeric-get-buffer-by-id 1 tab-name)))))

;; Test edge cases
;; ----------------------------------------

(ert-deftest test-etm-numeric-empty-tab ()
  "Test behavior with empty tab."
  (let ((tab-name "empty-tab"))
    (--test-setup-mock-tab tab-name)
    
    (should (equal 1 (--etm-numeric-get-next-id tab-name)))
    (should (null (--etm-numeric-get-buffer-by-id 1 tab-name)))
    (should (null (--etm-numeric-get-id-by-buffer "nonexistent" tab-name)))))

(ert-deftest test-etm-numeric-nonexistent-tab ()
  "Test behavior with nonexistent tab."
  (setq etm-numeric-buffers nil)  ; Clear state
  
  (should (equal 1 (--etm-numeric-get-next-id "nonexistent")))
  (should (null (--etm-numeric-get-buffer-by-id 1 "nonexistent"))))

;; Test module loading
;; ----------------------------------------

(ert-deftest test-etm-buffer-numeric-loadable ()
  "Test that etm-buffer-numeric module loads correctly."
  (should (featurep 'etm-buffer-numeric)))

(ert-deftest test-etm-buffer-numeric-functions-exist ()
  "Test that key functions are defined."
  (should (fboundp '--etm-numeric-register-buffer))
  (should (fboundp '--etm-numeric-unregister-buffer))
  (should (fboundp '--etm-numeric-get-buffer-by-id))
  (should (fboundp '--etm-numeric-get-id-by-buffer))
  (should (fboundp 'etm-numeric-register-current-buffer))
  (should (fboundp 'etm-numeric-jump-to-buffer))
  (should (fboundp 'etm-numeric-list-buffers)))

(provide 'test-etm-buffer-numeric)

;;; test-etm-buffer-numeric.el ends here

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-buffer/etm-buffer-numeric.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-05-24 16:30:00>
;; ;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-buffer/etm-buffer-numeric.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; ;;; Commentary:
;; ;; Numeric buffer ID system for ETM
;; ;; Automatically assigns buffer IDs (1, 2, 3...) and provides keybindings
;; 
;; (require 'etm-core-variables)
;; 
;; ;; Use variables from core
;; ;; ----------------------------------------
;; 
;; ;; etm-numeric-buffers and etm-max-numeric-buffers are defined in etm-core-variables
;; 
;; ;; Core Functions
;; ;; ----------------------------------------
;; 
;; (defun --etm-numeric-get-tab-buffers (tab-name)
;;   "Get numeric buffer configuration for TAB-NAME."
;;   (cdr (assoc tab-name etm-numeric-buffers)))
;; 
;; (defun --etm-numeric-get-next-id (tab-name)
;;   "Get next available numeric ID for TAB-NAME."
;;   (let* ((tab-buffers (--etm-numeric-get-tab-buffers tab-name))
;;          (used-ids (mapcar #'car tab-buffers))
;;          (next-id 1))
;;     (while (and (<= next-id etm-max-numeric-buffers)
;;                 (member next-id used-ids))
;;       (setq next-id (1+ next-id)))
;;     (if (<= next-id etm-max-numeric-buffers)
;;         next-id
;;       nil)))
;; 
;; (defun --etm-numeric-register-buffer (buffer-name &optional tab-name)
;;   "Register BUFFER-NAME with next available numeric ID for TAB-NAME.
;; Returns the assigned ID or nil if no slots available."
;;   (unless tab-name
;;     (setq tab-name (alist-get 'name (tab-bar--current-tab))))
;; 
;;   (let ((next-id (--etm-numeric-get-next-id tab-name)))
;;     (when next-id
;;       (let* ((tab-entry (assoc tab-name etm-numeric-buffers))
;;              (new-entry (cons next-id buffer-name)))
;;         (if tab-entry
;;             ;; Add to existing tab
;;             (setcdr tab-entry (cons new-entry (cdr tab-entry)))
;;           ;; Create new tab entry
;;           (push (cons tab-name (list new-entry)) etm-numeric-buffers))
;; 
;;         (message
;; 	 "[ETM DEBUG] --etm-numeric-register-buffer: Assigning numeric ID %d to buffer '%s' in tab '%s'"
;;          next-id buffer-name tab-name)
;;         ;; Don't show numeric IDs to user
;;         ;; (message "Buffer '%s' registered as ID %d in tab '%s'" 
;;         ;;          buffer-name next-id tab-name)
;;         next-id))))
;; 
;; (defun --etm-numeric-unregister-buffer
;;     (id-or-buffer &optional tab-name)
;;   "Unregister buffer by ID or buffer name from TAB-NAME."
;;   (unless tab-name
;;     (setq tab-name (alist-get 'name (tab-bar--current-tab))))
;; 
;;   (let* ((tab-entry (assoc tab-name etm-numeric-buffers))
;;          (tab-buffers (cdr tab-entry)))
;;     (when tab-buffers
;;       (let ((updated-buffers
;;              (if (numberp id-or-buffer)
;;                  ;; Remove by ID
;;                  (assq-delete-all id-or-buffer tab-buffers)
;;                ;; Remove by buffer name
;;                (cl-remove-if (lambda (entry) 
;;                                (string= (cdr entry) id-or-buffer))
;;                              tab-buffers))))
;;         (message
;; 	 "[ETM DEBUG] --etm-numeric-unregister-buffer: Unregistering %s from tab '%s'"
;;          (if (numberp id-or-buffer)
;;              (format "ID %d" id-or-buffer)
;;            (format "buffer '%s'" id-or-buffer))
;;          tab-name)
;;         (setcdr tab-entry updated-buffers)))))
;; 
;; (defun --etm-numeric-get-buffer-by-id (id &optional tab-name)
;;   "Get buffer name for ID in TAB-NAME."
;;   (unless tab-name
;;     (setq tab-name (alist-get 'name (tab-bar--current-tab))))
;; 
;;   (let ((tab-buffers (--etm-numeric-get-tab-buffers tab-name)))
;;     (cdr (assq id tab-buffers))))
;; 
;; (defun --etm-numeric-get-id-by-buffer (buffer-name &optional tab-name)
;;   "Get numeric ID for BUFFER-NAME in TAB-NAME."
;;   (unless tab-name
;;     (setq tab-name (alist-get 'name (tab-bar--current-tab))))
;; 
;;   (let ((tab-buffers (--etm-numeric-get-tab-buffers tab-name)))
;;     (car (rassoc buffer-name tab-buffers))))
;; 
;; ;; Interactive Functions
;; ;; ----------------------------------------
;; 
;; (defun etm-numeric-register-current-buffer ()
;;   "Register current buffer with next available numeric ID."
;;   (interactive)
;;   (let ((buffer-name (buffer-name)))
;;     (if (--etm-numeric-register-buffer buffer-name)
;;         (message "Registered current buffer '%s'" buffer-name)
;;       (message "No available numeric slots"))))
;; 
;; (defun etm-numeric-jump-to-buffer (id)
;;   "Jump to buffer with numeric ID."
;;   (interactive "nBuffer ID (1-9): ")
;;   (let ((buffer-name (--etm-numeric-get-buffer-by-id id)))
;;     (if buffer-name
;;         (if (get-buffer buffer-name)
;;             (switch-to-buffer buffer-name)
;;           (message "Buffer '%s' no longer exists" buffer-name)
;;           (--etm-numeric-unregister-buffer id))
;;       (message
;;        "No buffer at position %d. Use M-t b r to register current buffer"
;;        id))))
;; 
;; (defun etm-numeric-list-buffers ()
;;   "List all numeric buffers for current tab."
;;   (interactive)
;;   (let* ((tab-name (alist-get 'name (tab-bar--current-tab)))
;;          (tab-buffers (--etm-numeric-get-tab-buffers tab-name)))
;;     (if tab-buffers
;;         (progn
;;           (message "Registered buffers in tab '%s':" tab-name)
;;           (dolist
;; 	      (entry
;; 	       (sort tab-buffers (lambda (a b) (< (car a) (car b)))))
;;             (let ((buffer-name (cdr entry)))
;;               (message "  %s%s" buffer-name
;;                        (if (get-buffer buffer-name) "" " (not found)")))))
;;       (message
;;        "No registered buffers in tab '%s'. Use M-t b r to register current buffer"
;;        tab-name))))
;; 
;; (defun etm-numeric-auto-register-buffer ()
;;   "Auto-register current buffer if it's not already registered."
;;   (when (and (buffer-name)
;;              (not (string-prefix-p " " (buffer-name)))  ; Skip hidden buffers
;;              (not (minibufferp)))
;; 					; Skip minibuffer
;;     (let* ((buffer-name (buffer-name))
;;            (tab-name (alist-get 'name (tab-bar--current-tab)))
;;            (existing-id
;; 	    (--etm-numeric-get-id-by-buffer buffer-name tab-name)))
;;       (unless existing-id
;;         (--etm-numeric-register-buffer buffer-name tab-name)))))
;; 
;; (defun etm-numeric-quick-start ()
;;   "Quick start guide for numeric buffer system."
;;   (interactive)
;;   (with-help-window "*ETM Numeric Buffers Help*"
;;     (princ "ETM Numeric Buffer System - Quick Start Guide\n")
;;     (princ "============================================\n\n")
;;     (princ
;;      "The numeric buffer system allows you to quickly jump to buffers using number keys.\n\n")
;;     (princ "GETTING STARTED:\n")
;;     (princ "1. First, register buffers you want quick access to:\n")
;;     (princ "   - Switch to a buffer you use frequently\n")
;;     (princ "   - Press M-t b r to register it\n")
;;     (princ
;;      "   - The buffer gets assigned the next available number (1-9)\n\n")
;;     (princ "2. Jump to registered buffers:\n")
;;     (princ "   - M-t 1 to jump to buffer #1\n")
;;     (princ "   - M-t 2 to jump to buffer #2\n")
;;     (princ "   - ... and so on\n\n")
;;     (princ "KEY BINDINGS:\n")
;;     (princ "  M-t b r - Register current buffer\n")
;;     (princ "  M-t b l - List all registered buffers\n")
;;     (princ "  M-t b 1-9 - Jump to buffer by number\n")
;;     (princ "  M-t 1-9 - Quick jump to buffer by number\n")
;;     (princ "  M-t b c - Clean up dead buffer entries\n")
;;     (princ "  M-t b ? - Show help\n\n")
;;     (princ "NOTES:\n")
;;     (princ "- Each tab has its own set of numeric buffers\n")
;;     (princ
;;      "- Buffers are NOT automatically registered - you must use M-t b r\n")
;;     (princ "- Maximum 9 buffers per tab by default\n")))
;; 
;; ;; Keybinding Generation
;; ;; ----------------------------------------
;; 
;; (defun etm-numeric-define-keybindings (prefix-key)
;;   "Define keybindings for numeric buffer jumping.
;; PREFIX-KEY should be like 'M-t b' - this will create M-t b 1, M-t b 2, etc."
;;   (dotimes (i etm-numeric-max-buffers)
;;     (let ((id (1+ i))
;;           (key-sequence
;; 	   (concat prefix-key " " (number-to-string (1+ i)))))
;;       (global-set-key (kbd key-sequence)
;;                       `(lambda () 
;;                          (interactive)
;;                          (etm-numeric-jump-to-buffer ,id))))))
;; 
;; ;; Cleanup function
;; ;; ----------------------------------------
;; 
;; (defun etm-numeric-cleanup-dead-buffers ()
;;   "Remove numeric buffer entries for buffers that no longer exist."
;;   (interactive)
;;   (let ((cleaned-count 0))
;;     (dolist (tab-entry etm-numeric-buffers)
;;       (let* ((tab-name (car tab-entry))
;;              (tab-buffers (cdr tab-entry))
;;              (live-buffers (cl-remove-if
;;                             (lambda (entry)
;;                               (let ((buffer-name (cdr entry)))
;; 				(unless (get-buffer buffer-name)
;;                                   (setq cleaned-count
;; 					(1+ cleaned-count))
;;                                   t)))
;;                             tab-buffers)))
;;         (setcdr tab-entry live-buffers)))
;;     (message "Cleaned up %d dead buffer entries" cleaned-count)))
;; 
;; (defun etm-numeric-clear-tab (tab-name)
;;   "Clear all numeric buffer entries for TAB-NAME.
;; This should be called when a tab is closed."
;;   (setq etm-numeric-buffers
;;         (assoc-delete-all tab-name etm-numeric-buffers)))
;; 
;; ;; Hook Integration
;; ;; ----------------------------------------
;; 
;; (defun etm-numeric-setup-hooks ()
;;   "Setup hooks for automatic buffer management."
;;   ;; Don't use buffer-list-update-hook as it's too aggressive
;;   ;; Instead, users should manually register buffers with M-t b r
;;   ;; or use etm-numeric-register-current-buffer
;; 
;;   ;; Clean up dead buffers periodically
;;   (run-with-timer 60 60 #'etm-numeric-cleanup-dead-buffers))
;; 
;; ;; Home Buffer Jump Functions
;; ;; ----------------------------------------
;; 
;; (defun etm-jump-to-home-1 ()
;;   "Jump to home buffer 1 (numeric buffer 1)."
;;   (interactive)
;;   (etm-numeric-jump-to-buffer 1))
;; 
;; (defun etm-jump-to-home-2 ()
;;   "Jump to home buffer 2 (numeric buffer 2)."
;;   (interactive)
;;   (etm-numeric-jump-to-buffer 2))
;; 
;; (defun etm-jump-to-home-3 ()
;;   "Jump to home buffer 3 (numeric buffer 3)."
;;   (interactive)
;;   (etm-numeric-jump-to-buffer 3))
;; 
;; (defun etm-jump-to-home-4 ()
;;   "Jump to home buffer 4 (numeric buffer 4)."
;;   (interactive)
;;   (etm-numeric-jump-to-buffer 4))
;; 
;; (defun etm-jump-to-home-5 ()
;;   "Jump to home buffer 5 (numeric buffer 5)."
;;   (interactive)
;;   (etm-numeric-jump-to-buffer 5))
;; 
;; (defun etm-jump-to-home-6 ()
;;   "Jump to home buffer 6 (numeric buffer 6)."
;;   (interactive)
;;   (etm-numeric-jump-to-buffer 6))
;; 
;; (defun etm-jump-to-home-7 ()
;;   "Jump to home buffer 7 (numeric buffer 7)."
;;   (interactive)
;;   (etm-numeric-jump-to-buffer 7))
;; 
;; (defun etm-jump-to-home-8 ()
;;   "Jump to home buffer 8 (numeric buffer 8)."
;;   (interactive)
;;   (etm-numeric-jump-to-buffer 8))
;; 
;; (defun etm-jump-to-home-9 ()
;;   "Jump to home buffer 9 (numeric buffer 9)."
;;   (interactive)
;;   (etm-numeric-jump-to-buffer 9))
;; 
;; (provide 'etm-buffer-numeric)
;; 
;; ;;; etm-buffer-numeric.el ends here

;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-buffer/etm-buffer-numeric.el
;; --------------------------------------------------------------------------------

;;; test-etm-buffer-numeric.el ends here
