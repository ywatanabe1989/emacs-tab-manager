;;; test-etm-remote-errors.el --- Tests for ETM remote error handling -*- coding: utf-8; lexical-binding: t -*-

;; Author: Yuki Watanabe
;; Date: 2025-01-13
;; Version: 1.0.0

;;; Commentary:
;; Test suite for ETM remote error handling functionality.
;; Tests error recovery, user notifications, and fallback strategies.

;;; Code:

(add-to-list 'load-path (expand-file-name "../.." (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../etm-core" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../etm-remote" (file-name-directory load-file-name)))

(require 'ert)
(require 'cl-lib)
(require 'etm-core-variables)
(require 'etm-remote-connection)

;; Mock variables for testing
(defvar test-etm-remote-errors-messages nil
  "Captured messages during tests.")

(defvar test-etm-remote-errors-tramp-fails nil
  "Whether TRAMP operations should fail in tests.")

(defvar test-etm-remote-errors-retry-count 0
  "Count of retry attempts.")

(defun test-etm-remote-errors-setup ()
  "Set up test environment."
  ;; Initialize test variables
  (setq test-etm-remote-errors-messages '())
  (setq test-etm-remote-errors-tramp-fails nil)
  (setq test-etm-remote-errors-retry-count 0)
  (setq etm-remote-connections (make-hash-table :test 'equal))
  (setq etm-remote-global-connections (make-hash-table :test 'equal))
  (puthash "test-tab" (make-hash-table :test 'equal) etm-remote-connections)
  
  ;; Mock tab-bar functions
  (fset 'tab-bar--current-tab
        (lambda () '((name . "test-tab"))))
  
  ;; Mock message function to capture messages
  (advice-add 'message :override
              (lambda (format-string &rest args)
                (push (apply #'format format-string args) test-etm-remote-errors-messages)))
  
  ;; Mock TRAMP operations
  (fset 'tramp-get-connection-property
        (lambda (vec prop &optional default)
          (if test-etm-remote-errors-tramp-fails
              (error "Connection failed: Network unreachable")
            (or default t))))
  
  (fset 'tramp-make-tramp-file-name
        (lambda (&rest _args) "/mock/tramp/path"))
  
  (fset 'file-remote-p
        (lambda (filename &optional _identification)
          (when (string-match "^/[^:]+:" filename)
            filename)))
  
  ;; Mock sleep-for to avoid delays in tests
  (fset 'sleep-for (lambda (_seconds) nil)))

(defun test-etm-remote-errors-teardown ()
  "Tear down test environment."
  (advice-remove 'message (lambda (format-string &rest args)
                            (push (apply #'format format-string args) test-etm-remote-errors-messages))))

(ert-deftest test-etm-remote-error-handler-exists ()
  "Test that error handling functions are defined."
  (test-etm-remote-errors-setup)
  (unwind-protect
      (progn
        (require 'etm-remote-errors)
        (should (fboundp 'etm-remote-handle-error))
        (should (fboundp 'etm-remote-notify-error))
        (should (fboundp 'etm-remote-retry-operation))
        (should (fboundp 'etm-remote-fallback-local)))
    (test-etm-remote-errors-teardown)))

(ert-deftest test-etm-remote-connection-error-handling ()
  "Test handling of connection errors."
  (test-etm-remote-errors-setup)
  (unwind-protect
      (progn
        (require 'etm-remote-errors)
        
        ;; Simulate connection failure
        (setq test-etm-remote-errors-tramp-fails t)
        
        ;; Try to connect with error handling
        (condition-case err
            (etm-remote-safe-connect "ssh" "user" "example.com")
          (error
           ;; Should have been handled gracefully
           (should (string-match "Connection failed" (car test-etm-remote-errors-messages)))
           (should (memq 'etm-remote-connection-error (get (car err) 'error-conditions))))))
    (test-etm-remote-errors-teardown)))

(ert-deftest test-etm-remote-retry-mechanism ()
  "Test automatic retry functionality."
  (test-etm-remote-errors-setup)
  (unwind-protect
      (progn
        (require 'etm-remote-errors)
        
        ;; Define a function that fails initially then succeeds
        (let ((attempts 0))
          (cl-flet ((flaky-operation ()
                      (cl-incf attempts)
                      (if (< attempts 3)
                          (error "Temporary failure")
                        "Success")))
            
            ;; Test retry mechanism
            (let ((result (etm-remote-retry-operation #'flaky-operation 5)))
              (should (equal result "Success"))
              (should (= attempts 3))))))
    (test-etm-remote-errors-teardown)))

(ert-deftest test-etm-remote-user-notifications ()
  "Test user notification system."
  (test-etm-remote-errors-setup)
  (unwind-protect
      (progn
        (require 'etm-remote-errors)
        
        ;; Test error notification
        (etm-remote-notify-error "example.com" "Connection timeout")
        (should (cl-some (lambda (msg)
                           (string-match "example.com.*Connection timeout" msg))
                         test-etm-remote-errors-messages))
        
        ;; Test warning notification
        (etm-remote-notify-warning "Slow connection detected")
        (should (cl-some (lambda (msg)
                           (string-match "Warning.*Slow connection" msg))
                         test-etm-remote-errors-messages)))
    (test-etm-remote-errors-teardown)))

(ert-deftest test-etm-remote-fallback-strategies ()
  "Test fallback to local operations."
  (test-etm-remote-errors-setup)
  (unwind-protect
      (progn
        (require 'etm-remote-errors)
        
        ;; Mock buffer functions
        (fset 'find-file
              (lambda (filename)
                (if (and (file-remote-p filename)
                         test-etm-remote-errors-tramp-fails)
                    (error "Cannot access remote file")
                  (get-buffer-create (file-name-nondirectory filename)))))
        
        ;; Mock yes-or-no-p to always return yes
        (fset 'yes-or-no-p (lambda (_prompt) t))
        
        ;; Test fallback when remote fails
        (setq test-etm-remote-errors-tramp-fails t)
        (let ((buffer (etm-remote-safe-find-file "/ssh:user@host:/path/to/file.txt")))
          ;; Should fall back to local copy or provide alternative
          (should buffer)
          (should (cl-some (lambda (msg)
                             (string-match "fallback\\|local" msg))
                           test-etm-remote-errors-messages))))
    (test-etm-remote-errors-teardown)))

(ert-deftest test-etm-remote-error-recovery ()
  "Test error recovery mechanisms."
  (test-etm-remote-errors-setup)
  (unwind-protect
      (progn
        (require 'etm-remote-errors)
        
        ;; Create a connection
        (let ((conn (etm-remote-connect "ssh" "user" "example.com")))
          (setf (etm-remote-connection-status conn) :connected)
          
          ;; Simulate error
          (setq test-etm-remote-errors-tramp-fails t)
          (etm-remote-check-connection "example.com")
          
          ;; Connection should be marked as error
          (should (eq (etm-remote-connection-status conn) :disconnected))
          
          ;; Recovery attempt
          (setq test-etm-remote-errors-tramp-fails nil)
          ;; Clear messages before recovery
          (setq test-etm-remote-errors-messages '())
          (etm-remote-recover-connection "example.com")
          
          ;; Should attempt reconnection
          (should (cl-some (lambda (msg)
                             (string-match "Recovering\\|Reconnecting" msg))
                           test-etm-remote-errors-messages))))
    (test-etm-remote-errors-teardown)))

;; Run tests if executed directly
;; (when (and (boundp 'load-file-name) load-file-name)
;;   (ert-run-tests-batch-and-exit))

(provide 'test-etm-remote-errors)
;;; test-etm-remote-errors.el ends here

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-remote/etm-remote-errors.el
;; --------------------------------------------------------------------------------
;; ;;; etm-remote-errors.el --- Error handling for ETM remote connections -*- coding: utf-8; lexical-binding: t -*-
;; 
;; ;; Author: Yuki Watanabe
;; ;; Date: 2025-01-13
;; ;; Version: 1.0.0
;; 
;; ;;; Commentary:
;; ;; This module provides comprehensive error handling for ETM remote operations.
;; ;; It includes error recovery mechanisms, user-friendly notifications,
;; ;; automatic retry logic, and fallback strategies.
;; 
;; ;;; Code:
;; 
;; (require 'cl-lib)
;; (require 'etm-remote-connection)
;; 
;; (defcustom etm-remote-error-retry-count 3
;;   "Number of times to retry failed operations."
;;   :type 'integer
;;   :group 'etm-remote)
;; 
;; (defcustom etm-remote-error-retry-delay 2
;;   "Delay in seconds between retry attempts."
;;   :type 'number
;;   :group 'etm-remote)
;; 
;; (defcustom etm-remote-error-show-details t
;;   "Whether to show detailed error messages to the user."
;;   :type 'boolean
;;   :group 'etm-remote)
;; 
;; (define-error 'etm-remote-error "ETM Remote Error" 'error)
;; (define-error 'etm-remote-connection-error "ETM Remote Connection Error" 'etm-remote-error)
;; (define-error 'etm-remote-timeout-error "ETM Remote Timeout Error" 'etm-remote-error)
;; 
;; (defvar etm-remote-error-history nil
;;   "History of recent errors for debugging.")
;; 
;; (defvar etm-remote-error-handlers
;;   '((file-error . etm-remote-handle-file-error)
;;     (tramp-error . etm-remote-handle-tramp-error)
;;     (etm-remote-connection-error . etm-remote-handle-connection-error))
;;   "Alist of error types and their handlers.")
;; 
;; (defun etm-remote-handle-error (error-data)
;;   "Handle ERROR-DATA appropriately based on error type."
;;   (let* ((error-type (car error-data))
;;          (handler (alist-get error-type etm-remote-error-handlers)))
;;     ;; Log error
;;     (push (list :time (current-time)
;;                 :type error-type
;;                 :data error-data)
;;           etm-remote-error-history)
;;     ;; Invoke specific handler or use default
;;     (if handler
;;         (funcall handler error-data)
;;       (etm-remote-handle-generic-error error-data))))
;; 
;; (defun etm-remote-handle-file-error (error-data)
;;   "Handle file-related errors in ERROR-DATA."
;;   (let ((operation (nth 1 error-data))
;;         (reason (nth 2 error-data)))
;;     (etm-remote-notify-error nil
;;                              (format "File operation failed: %s - %s" operation reason))))
;; 
;; (defun etm-remote-handle-tramp-error (error-data)
;;   "Handle TRAMP-specific errors in ERROR-DATA."
;;   (let ((msg (error-message-string error-data)))
;;     (cond
;;      ((string-match "Login failed" msg)
;;       (etm-remote-notify-error nil "Authentication failed. Check credentials."))
;;      ((string-match "Timeout" msg)
;;       (etm-remote-notify-error nil "Connection timeout. The remote host may be unreachable."))
;;      (t
;;       (etm-remote-notify-error nil (format "Remote connection error: %s" msg))))))
;; 
;; (defun etm-remote-handle-connection-error (error-data)
;;   "Handle connection-specific errors in ERROR-DATA."
;;   (let ((host (plist-get (cdr error-data) :host)))
;;     (when host
;;       ;; Mark connection as failed
;;       (let ((conn (etm-remote-get-connection host)))
;;         (when conn
;;           (setf (etm-remote-connection-status conn) :error))))
;;     (etm-remote-notify-error host "Connection failed")))
;; 
;; (defun etm-remote-handle-generic-error (error-data)
;;   "Handle generic errors in ERROR-DATA."
;;   (etm-remote-notify-error nil (error-message-string error-data)))
;; 
;; (defun etm-remote-notify-error (host message)
;;   "Notify user of an error for HOST with MESSAGE."
;;   (let ((full-message (if host
;;                           (format "ETM Remote Error [%s]: %s" host message)
;;                         (format "ETM Remote Error: %s" message))))
;;     (message "%s" full-message)
;;     (when etm-remote-error-show-details
;;       (with-current-buffer (get-buffer-create "*ETM Remote Errors*")
;;         (goto-char (point-max))
;;         (insert (format "[%s] %s\n" (format-time-string "%Y-%m-%d %H:%M:%S") full-message))))))
;; 
;; (defun etm-remote-notify-warning (message)
;;   "Notify user of a warning with MESSAGE."
;;   (message "ETM Remote Warning: %s" message))
;; 
;; (defun etm-remote-retry-operation (operation &optional max-retries)
;;   "Retry OPERATION up to MAX-RETRIES times with exponential backoff."
;;   (let ((retries 0)
;;         (max-retries (or max-retries etm-remote-error-retry-count))
;;         (delay etm-remote-error-retry-delay)
;;         result)
;;     (while (and (< retries max-retries)
;;                 (not result))
;;       (condition-case err
;;           (setq result (funcall operation))
;;         (error
;;          (cl-incf retries)
;;          (if (< retries max-retries)
;;              (progn
;;                (etm-remote-notify-warning
;;                 (format "Operation failed, retrying (%d/%d)..." retries max-retries))
;;                (sleep-for delay)
;;                (setq delay (* delay 1.5))) ; Exponential backoff
;;            (signal (car err) (cdr err))))))
;;     result))
;; 
;; (defun etm-remote-safe-connect (method user host &optional port)
;;   "Safely connect to remote with error handling.
;; METHOD, USER, HOST, and PORT are connection parameters."
;;   (condition-case err
;;       (etm-remote-connect method user host port)
;;     (error
;;      (etm-remote-handle-error err)
;;      (signal 'etm-remote-connection-error
;;              (list :method method :user user :host host :port port
;;                    :original-error err)))))
;; 
;; (defun etm-remote-safe-find-file (filename)
;;   "Safely find remote file with FILENAME, with fallback strategies."
;;   (condition-case err
;;       (find-file filename)
;;     (error
;;      (etm-remote-handle-error err)
;;      ;; Try fallback strategies
;;      (cond
;;       ;; Strategy 1: Try without method if it's a TRAMP filename
;;       ((and (file-remote-p filename)
;;             (string-match "^/[^:]+:" filename))
;;        (let ((simplified (replace-match "/ssh:" nil nil filename)))
;;          (etm-remote-notify-warning "Trying simplified connection method...")
;;          (condition-case nil
;;              (find-file simplified)
;;            (error
;;             (etm-remote-fallback-local filename)))))
;;       ;; Strategy 2: Offer to open locally
;;       (t
;;        (etm-remote-fallback-local filename))))))
;; 
;; (defun etm-remote-fallback-local (remote-filename)
;;   "Provide fallback options for failed REMOTE-FILENAME access."
;;   (let ((base-name (file-name-nondirectory remote-filename)))
;;     (if (yes-or-no-p (format "Cannot access remote file. Create local buffer '%s'? " base-name))
;;         (progn
;;           (etm-remote-notify-warning "Opening local buffer as fallback")
;;           (get-buffer-create base-name))
;;       (error "Remote file access failed"))))
;; 
;; (defun etm-remote-recover-connection (host)
;;   "Attempt to recover connection to HOST."
;;   (etm-remote-notify-warning (format "Recovering connection to %s..." host))
;;   (let ((conn (etm-remote-get-connection host)))
;;     (when conn
;;       ;; Reset retry counter
;;       (setf (etm-remote-connection-retries conn) 0)
;;       ;; Attempt reconnection
;;       (etm-remote-retry-operation
;;        (lambda ()
;;          (etm-remote-check-connection host))))))
;; 
;; (defun etm-remote-monitor-errors ()
;;   "Monitor and report on error patterns."
;;   (interactive)
;;   (let ((recent-errors (cl-subseq etm-remote-error-history 
;;                                   0 (min 10 (length etm-remote-error-history)))))
;;     (with-current-buffer (get-buffer-create "*ETM Remote Error Report*")
;;       (erase-buffer)
;;       (insert "ETM Remote Error Report\n")
;;       (insert "======================\n\n")
;;       (if recent-errors
;;           (dolist (error recent-errors)
;;             (insert (format "[%s] %s: %s\n"
;;                             (format-time-string "%Y-%m-%d %H:%M:%S" (plist-get error :time))
;;                             (plist-get error :type)
;;                             (error-message-string (plist-get error :data)))))
;;         (insert "No recent errors.\n"))
;;       (goto-char (point-min))
;;       (display-buffer (current-buffer)))))
;; 
;; ;; Install advice for automatic error handling
;; (defun etm-remote-errors-init ()
;;   "Initialize error handling for remote operations."
;;   ;; Advice for connection operations
;;   (advice-add 'etm-remote-connect :around #'etm-remote--with-error-handling)
;;   (advice-add 'etm-remote-check-connection :around #'etm-remote--with-error-handling))
;; 
;; (defun etm-remote-errors-cleanup ()
;;   "Clean up error handling advice."
;;   (advice-remove 'etm-remote-connect #'etm-remote--with-error-handling)
;;   (advice-remove 'etm-remote-check-connection #'etm-remote--with-error-handling))
;; 
;; (defun etm-remote--with-error-handling (orig-fun &rest args)
;;   "Wrap ORIG-FUN with error handling, passing ARGS."
;;   (condition-case err
;;       (apply orig-fun args)
;;     (error
;;      (etm-remote-handle-error err)
;;      nil)))
;; 
;; (provide 'etm-remote-errors)
;; ;;; etm-remote-errors.el ends here
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-remote/etm-remote-errors.el
;; --------------------------------------------------------------------------------

;;; test-etm-remote-errors.el ends here
