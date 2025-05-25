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
          (should (string-match "^\\[example\\.com\\]" 
                                (etm-remote-prefix-buffer-name "remote-file.txt")))))
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
(when (and (boundp 'load-file-name) load-file-name)
  (ert-run-tests-batch-and-exit))

(provide 'test-etm-remote-indicators)
;;; test-etm-remote-indicators.el ends here