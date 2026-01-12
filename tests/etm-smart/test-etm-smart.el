;;; -*- coding: utf-8; lexical-binding: t -*-
;;; test-etm-smart.el --- Tests for ETM Smart Suggestions main module
;;; Author: ywatanabe
;;; Time-stamp: <2025-01-26 04:16:00 ywatanabe>
;;; Commentary:
;;; Tests for the main Smart Suggestions integration module.

;;; Code:

(require 'ert)
(require 'etm-test-utils)

;; Load the module under test
(require 'etm-smart)

;;; Test Initialization

(ert-deftest test-etm-smart-init ()
  "Test Smart Suggestions initialization."
  (with-etm-test-environment
    ;; Reset state
    (setq etm-smart-initialized nil
          etm-smart-enabled nil)
    
    ;; Initialize
    (etm-smart-init)
    
    ;; Check initialization
    (should etm-smart-initialized)
    (should-not etm-smart-enabled)))

(ert-deftest test-etm-smart-toggle ()
  "Test toggling Smart Suggestions on/off."
  (with-etm-test-environment
    (etm-smart-init)
    
    ;; Initially disabled
    (should-not etm-smart-enabled)
    
    ;; Toggle on
    (etm-smart-toggle)
    (should etm-smart-enabled)
    
    ;; Toggle off
    (etm-smart-toggle)
    (should-not etm-smart-enabled)))

;;; Test Hook Integration

(ert-deftest test-etm-smart-buffer-switch-tracking ()
  "Test that buffer switches are tracked when enabled."
  (with-etm-test-environment
    (etm-smart-init)
    (etm-smart-toggle) ; Enable

    ;; Create test buffers
    (let ((buf1 (get-buffer-create "test-buffer-1"))
          (buf2 (get-buffer-create "test-buffer-2")))

      ;; Switch to buf1
      (switch-to-buffer buf1)

      ;; Mock the hook callback
      (let ((etm-smart-enabled t))
        (with-current-buffer buf2
          (etm-smart--on-buffer-switch)))

      ;; Should have tracked the switch (use etm-smart-patterns not etm-smart--patterns)
      (let ((patterns (gethash (etm-core-get-current-tab-id) etm-smart-patterns)))
        (should patterns)))))

;;; Test Commands

(ert-deftest test-etm-smart-clear-patterns ()
  "Test clearing patterns for current tab."
  (with-etm-test-environment
    (etm-smart-init)
    
    ;; Add some test patterns
    (let ((tab-id (etm-core-get-current-tab-id)))
      (puthash tab-id (make-hash-table :test 'equal) etm-smart--patterns)
      
      ;; Mock yes-or-no-p
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (etm-smart-clear-patterns))
      
      ;; Patterns should be cleared
      (should-not (gethash tab-id etm-smart--patterns)))))

(ert-deftest test-etm-smart-reset-scores ()
  "Test resetting scores for patterns."
  (with-etm-test-environment
    (etm-smart-init)
    
    ;; Create test pattern
    (let* ((tab-id (etm-core-get-current-tab-id))
           (patterns (make-hash-table :test 'equal))
           (pattern (make-etm-smart-pattern
                     :from-buffer "buf1"
                     :to-buffer "buf2"
                     :count 5
                     :timestamps '(1234567890)
                     :context nil
                     :score 10.0)))
      
      (puthash "buf1->buf2" pattern patterns)
      (puthash tab-id patterns etm-smart--patterns)
      
      ;; Mock yes-or-no-p
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (etm-smart-reset-scores))
      
      ;; Score should be reset
      (should (= 0.0 (etm-smart-pattern-score pattern))))))

;;; Test Storage

(ert-deftest test-etm-smart-save-load-data ()
  "Test saving and loading Smart Suggestions data."
  (with-etm-test-environment
    (etm-smart-init)
    
    ;; Create test data
    (let ((tab-id (etm-core-get-current-tab-id))
          (test-file (make-temp-file "etm-smart-test-")))
      
      ;; Set up test pattern
      (etm-smart-track-switch "buffer-1" "buffer-2")
      
      ;; Save to test file
      (let ((etm-smart-storage-file test-file))
        (etm-smart-save-data)
        
        ;; Clear patterns
        (setq etm-smart--patterns (make-hash-table :test 'equal))
        
        ;; Load back
        (etm-smart-load-data)
        
        ;; Should have patterns restored
        (let ((patterns (gethash tab-id etm-smart--patterns)))
          (should patterns)))
      
      ;; Clean up
      (delete-file test-file))))

;;; Test Mode

(ert-deftest test-etm-smart-mode ()
  "Test ETM Smart minor mode."
  (with-etm-test-environment
    ;; Mode should be off initially
    (should-not etm-smart-mode)
    (should-not etm-smart-enabled)
    
    ;; Enable mode
    (etm-smart-mode 1)
    (should etm-smart-mode)
    (should etm-smart-enabled)
    
    ;; Disable mode
    (etm-smart-mode -1)
    (should-not etm-smart-mode)
    (should-not etm-smart-enabled)))

;;; Test Auto-save

(ert-deftest test-etm-smart-auto-save ()
  "Test auto-save functionality."
  (with-etm-test-environment
    (etm-smart-init)
    
    ;; Timer should be started
    (should etm-smart-auto-save-timer)
    (should (timerp etm-smart-auto-save-timer))
    
    ;; Shutdown should stop timer
    (etm-smart-shutdown)
    (should-not etm-smart-auto-save-timer)))

(provide 'test-etm-smart)
;;; test-etm-smart.el ends here

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-smart/etm-smart.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; etm-smart.el --- Smart Suggestions for Emacs Tab Manager
;; ;;; Author: ywatanabe
;; ;;; Time-stamp: <2025-01-26 04:15:00 ywatanabe>
;; ;;; Commentary:
;; ;;; This module integrates all Smart Suggestions components for ETM,
;; ;;; providing intelligent buffer switching recommendations based on usage patterns.
;; 
;; ;;; Code:
;; 
;; (require 'etm-core)
;; (require 'etm-smart-patterns)
;; (require 'etm-smart-suggest)
;; (require 'etm-smart-ui)
;; 
;; ;;; Integration with ETM
;; 
;; (defvar etm-smart-enabled nil
;;   "Whether Smart Suggestions are enabled.")
;; 
;; (defvar etm-smart-initialized nil
;;   "Whether Smart Suggestions have been initialized.")
;; 
;; (defvar etm-smart--buffer-switch-hook-added nil
;;   "Whether the buffer switch hook has been added.")
;; 
;; ;;; Hook Integration
;; 
;; (defun etm-smart--on-buffer-switch ()
;;   "Track buffer switches when Smart Suggestions are enabled."
;;   (when etm-smart-enabled
;;     (let ((from-buffer (window-buffer (get-buffer-window nil 'visible)))
;;           (to-buffer (current-buffer)))
;;       (when (and from-buffer to-buffer
;;                  (not (eq from-buffer to-buffer)))
;;         (etm-smart-track-switch (buffer-name from-buffer)
;;                                 (buffer-name to-buffer))))))
;; 
;; (defun etm-smart--setup-hooks ()
;;   "Set up hooks for Smart Suggestions."
;;   (unless etm-smart--buffer-switch-hook-added
;;     (add-hook 'window-buffer-change-functions
;;               (lambda (_frame)
;;                 (etm-smart--on-buffer-switch))
;;               nil t)
;;     (setq etm-smart--buffer-switch-hook-added t)))
;; 
;; (defun etm-smart--remove-hooks ()
;;   "Remove hooks for Smart Suggestions."
;;   (when etm-smart--buffer-switch-hook-added
;;     (remove-hook 'window-buffer-change-functions
;;                  (lambda (_frame)
;;                    (etm-smart--on-buffer-switch))
;;                  t)
;;     (setq etm-smart--buffer-switch-hook-added nil)))
;; 
;; ;;; Keybinding Integration
;; 
;; (defvar etm-smart-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "s") #'etm-smart-switch-to-suggested)
;;     (define-key map (kbd "S") #'etm-smart-show-suggestions)
;;     (define-key map (kbd "o") #'etm-smart-ui-show-overlay)
;;     (define-key map (kbd "t") #'etm-smart-toggle)
;;     (define-key map (kbd "c") #'etm-smart-clear-patterns)
;;     (define-key map (kbd "r") #'etm-smart-reset-scores)
;;     map)
;;   "Keymap for Smart Suggestions commands.")
;; 
;; ;;; Main Commands
;; 
;; (defun etm-smart-switch-to-suggested ()
;;   "Switch to the top suggested buffer."
;;   (interactive)
;;   (etm-smart-suggest-quick-switch))
;; 
;; (defun etm-smart-show-suggestions ()
;;   "Show buffer suggestions in a dedicated buffer."
;;   (interactive)
;;   (etm-smart-suggest-show-buffer))
;; 
;; (defun etm-smart-toggle ()
;;   "Toggle Smart Suggestions on/off."
;;   (interactive)
;;   (setq etm-smart-enabled (not etm-smart-enabled))
;;   (if etm-smart-enabled
;;       (progn
;;         (etm-smart--setup-hooks)
;;         (message "ETM Smart Suggestions enabled"))
;;     (progn
;;       (etm-smart--remove-hooks)
;;       (etm-smart-ui-cancel-overlay-timer)
;;       (message "ETM Smart Suggestions disabled"))))
;; 
;; (defun etm-smart-clear-patterns ()
;;   "Clear all tracked patterns for the current tab."
;;   (interactive)
;;   (when (yes-or-no-p "Clear all Smart Suggestions patterns for this tab? ")
;;     (let ((tab-id (etm-core-get-current-tab-id)))
;;       (remhash tab-id etm-smart-patterns))
;;     (message "Smart Suggestions patterns cleared")))
;; 
;; (defun etm-smart-reset-scores ()
;;   "Reset scores for all patterns in the current tab."
;;   (interactive)
;;   (when (yes-or-no-p "Reset all Smart Suggestions scores for this tab? ")
;;     (let ((patterns (gethash (etm-core-get-current-tab-id) etm-smart-patterns)))
;;       (when patterns
;;         (maphash (lambda (_key pattern)
;;                    (setf (etm-smart-pattern-score pattern) 0.0))
;;                  patterns)))
;;     (message "Smart Suggestions scores reset")))
;; 
;; ;;; Storage Integration
;; 
;; (defvar etm-smart-storage-file
;;   (expand-file-name "etm-smart-data.el" user-emacs-directory)
;;   "File for storing Smart Suggestions data.")
;; 
;; (defun etm-smart-save-data ()
;;   "Save Smart Suggestions data to disk."
;;   (interactive)
;;   (with-temp-file etm-smart-storage-file
;;     (insert ";; ETM Smart Suggestions Data\n")
;;     (insert ";; This file is auto-generated. Do not edit manually.\n\n")
;;     (pp (list :version 1
;;               :patterns etm-smart-patterns
;;               :config (list :min-count etm-smart-min-pattern-count
;;                             :max-suggestions etm-smart-max-suggestions
;;                             :decay-factor etm-smart-decay-factor))
;;         (current-buffer)))
;;   (message "Smart Suggestions data saved"))
;; 
;; (defun etm-smart-load-data ()
;;   "Load Smart Suggestions data from disk."
;;   (interactive)
;;   (when (file-exists-p etm-smart-storage-file)
;;     (condition-case err
;;         (let ((data (with-temp-buffer
;;                       (insert-file-contents etm-smart-storage-file)
;;                       (goto-char (point-min))
;;                       (while (looking-at "^;;")
;;                         (forward-line))
;;                       (read (current-buffer)))))
;;           (when (and (listp data) (eq (plist-get data :version) 1))
;;             (setq etm-smart-patterns (plist-get data :patterns))
;;             (let ((config (plist-get data :config)))
;;               (when config
;;                 (setq etm-smart-min-pattern-count (plist-get config :min-count)
;;                       etm-smart-max-suggestions (plist-get config :max-suggestions)
;;                       etm-smart-decay-factor (plist-get config :decay-factor))))
;;             (message "Smart Suggestions data loaded")))
;;       (error
;;        (message "Error loading Smart Suggestions data: %s" (error-message-string err))))))
;; 
;; ;;; Auto-save Integration
;; 
;; (defvar etm-smart-auto-save-timer nil
;;   "Timer for auto-saving Smart Suggestions data.")
;; 
;; (defcustom etm-smart-auto-save-interval 300
;;   "Interval in seconds for auto-saving Smart Suggestions data."
;;   :type 'integer
;;   :group 'etm-smart)
;; 
;; (defun etm-smart--start-auto-save ()
;;   "Start auto-saving Smart Suggestions data."
;;   (when etm-smart-auto-save-timer
;;     (cancel-timer etm-smart-auto-save-timer))
;;   (setq etm-smart-auto-save-timer
;;         (run-with-timer etm-smart-auto-save-interval
;;                         etm-smart-auto-save-interval
;;                         #'etm-smart-save-data)))
;; 
;; (defun etm-smart--stop-auto-save ()
;;   "Stop auto-saving Smart Suggestions data."
;;   (when etm-smart-auto-save-timer
;;     (cancel-timer etm-smart-auto-save-timer)
;;     (setq etm-smart-auto-save-timer nil)))
;; 
;; ;;; Initialization
;; 
;; (defun etm-smart-init ()
;;   "Initialize Smart Suggestions."
;;   (unless etm-smart-initialized
;;     ;; Load saved data
;;     (etm-smart-load-data)
;;     
;;     ;; Set up UI integration
;;     (etm-smart-ui-setup-minibuffer)
;;     
;;     ;; Start with Smart Suggestions disabled
;;     (setq etm-smart-enabled nil)
;;     
;;     ;; Start auto-save timer
;;     (etm-smart--start-auto-save)
;;     
;;     ;; Add shutdown hook
;;     (add-hook 'kill-emacs-hook #'etm-smart-save-data)
;;     
;;     (setq etm-smart-initialized t)
;;     (message "ETM Smart Suggestions initialized")))
;; 
;; (defun etm-smart-shutdown ()
;;   "Shutdown Smart Suggestions."
;;   (when etm-smart-initialized
;;     ;; Disable if active
;;     (when etm-smart-enabled
;;       (etm-smart-toggle))
;;     
;;     ;; Stop auto-save
;;     (etm-smart--stop-auto-save)
;;     
;;     ;; Save data
;;     (etm-smart-save-data)
;;     
;;     ;; Clean up UI
;;     (etm-smart-ui-cancel-overlay-timer)
;;     
;;     (setq etm-smart-initialized nil)
;;     (message "ETM Smart Suggestions shutdown")))
;; 
;; ;;; Integration with ETM's existing commands
;; 
;; (defun etm-smart-enhance-buffer-jump (orig-fun &rest args)
;;   "Enhance buffer jump commands with Smart Suggestions.
;; ORIG-FUN is the original function, ARGS are its arguments."
;;   (if etm-smart-enabled
;;       (let* ((suggestions (etm-smart-suggest-buffers 3))
;;              (suggestion-names (mapcar (lambda (s) (plist-get s :buffer)) suggestions)))
;;         ;; Add suggestions to completion candidates if available
;;         (if suggestion-names
;;             (let ((completing-read-function
;;                    (lambda (prompt collection &rest args)
;;                      (apply completing-read-function prompt
;;                             (append suggestion-names
;;                                     (if (listp collection)
;;                                         collection
;;                                       (all-completions "" collection)))
;;                             args))))
;;               (apply orig-fun args))
;;           (apply orig-fun args)))
;;     (apply orig-fun args)))
;; 
;; ;;; Mode Definition
;; 
;; (define-minor-mode etm-smart-mode
;;   "Minor mode for ETM Smart Suggestions."
;;   :global t
;;   :group 'etm-smart
;;   :keymap (let ((map (make-sparse-keymap)))
;;             (define-key map (kbd "C-c t S") etm-smart-map)
;;             map)
;;   (if etm-smart-mode
;;       (progn
;;         (unless etm-smart-initialized
;;           (etm-smart-init))
;;         (etm-smart-toggle)
;;         (message "ETM Smart mode enabled"))
;;     (when etm-smart-enabled
;;       (etm-smart-toggle))
;;     (message "ETM Smart mode disabled")))
;; 
;; (provide 'etm-smart)
;; ;;; etm-smart.el ends here
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-smart/etm-smart.el
;; --------------------------------------------------------------------------------

;;; test-etm-smart.el ends here
