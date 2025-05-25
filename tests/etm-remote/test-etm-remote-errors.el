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
(when (and (boundp 'load-file-name) load-file-name)
  (ert-run-tests-batch-and-exit))

(provide 'test-etm-remote-errors)
;;; test-etm-remote-errors.el ends here