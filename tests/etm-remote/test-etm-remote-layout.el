;;; test-etm-remote-layout.el --- Tests for ETM remote layout integration -*- coding: utf-8; lexical-binding: t -*-

;; Author: Yuki Watanabe
;; Date: 2025-01-13
;; Version: 1.0.0

;;; Commentary:
;; Test suite for ETM remote layout integration functionality.
;; Tests layout save/load with remote connections, offline mode, and migration.

;;; Code:

(add-to-list 'load-path (expand-file-name "../.." (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../etm-core" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../etm-layout" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../etm-remote" (file-name-directory load-file-name)))

(require 'ert)
(require 'cl-lib)
(require 'etm-core-variables)
(require 'etm-layout-save)
(require 'etm-layout-open)
(require 'etm-remote-connection)

;; Mock variables for testing
(defvar test-etm-remote-layout-saved-content nil
  "Content saved during test.")

(defvar test-etm-remote-layout-offline-mode nil
  "Whether to simulate offline mode.")

(defvar test-etm-remote-layout-connections nil
  "Mock connection list.")

(defun test-etm-remote-layout-setup ()
  "Set up test environment."
  ;; Initialize test variables
  (setq test-etm-remote-layout-saved-content nil)
  (setq test-etm-remote-layout-offline-mode nil)
  (setq test-etm-remote-layout-connections '())
  (setq etm-remote-connections (make-hash-table :test 'equal))
  (setq etm-remote-global-connections (make-hash-table :test 'equal))
  (puthash "test-tab" (make-hash-table :test 'equal) etm-remote-connections)
  
  ;; Mock functions
  (fset 'tab-bar--current-tab
        (lambda () '((name . "test-tab"))))
  
  ;; Mock file operations
  (fset 'find-file-noselect
        (lambda (filename)
          (get-buffer-create "*test-layout-buffer*")))
  
  (fset 'with-current-buffer
        (lambda (buffer &rest body)
          (eval `(progn ,@body))))
  
  ;; Mock save operations
  (advice-add 'insert :override
              (lambda (content)
                (setq test-etm-remote-layout-saved-content
                      (concat test-etm-remote-layout-saved-content content))))
  
  ;; Mock user input functions
  (fset 'read-string (lambda (_prompt) ""))
  
  ;; Mock layout capture to avoid complex window operations
  (fset '--etm-layout-capture-current-layout
        (lambda (_name _host)
          "(--etm-layout-create-from-positions \"test\" '() nil)"))
  
  ;; Mock file save operations
  (fset 'save-buffer (lambda () nil))
  (fset 'kill-buffer (lambda (&optional _buffer) nil))
  (fset 'load-file (lambda (_file) nil))
  (fset 'erase-buffer (lambda () nil))
  
  ;; Mock save-current-buffer properly
  (defmacro save-current-buffer (&rest body)
    `(progn ,@body)))

(defun test-etm-remote-layout-teardown ()
  "Tear down test environment."
  (advice-remove 'insert (lambda (content)
                           (setq test-etm-remote-layout-saved-content
                                 (concat test-etm-remote-layout-saved-content content)))))

(ert-deftest test-etm-remote-layout-functions-exist ()
  "Test that remote layout functions are defined."
  (test-etm-remote-layout-setup)
  (unwind-protect
      (progn
        (require 'etm-remote-layout)
        (should (fboundp 'etm-remote-layout-save-connections))
        (should (fboundp 'etm-remote-layout-restore-connections))
        (should (fboundp 'etm-remote-layout-offline-mode))
        (should (fboundp 'etm-remote-layout-migrate)))
    (test-etm-remote-layout-teardown)))

(ert-deftest test-etm-remote-layout-save-with-connections ()
  "Test saving layout with remote connection information."
  (test-etm-remote-layout-setup)
  (unwind-protect
      (progn
        (require 'etm-remote-layout)
        
        ;; Create some remote connections
        (let ((conn1 (etm-remote-connect "ssh" "user" "host1.com"))
              (conn2 (etm-remote-connect "ssh" "user" "host2.com")))
          (setf (etm-remote-connection-status conn1) :connected)
          (setf (etm-remote-connection-status conn2) :connected)
          
          ;; Save layout with connections
          (let ((saved-data (etm-remote-layout-save-connections)))
            (should saved-data)
            (should (= (length saved-data) 2))
            (should (cl-some (lambda (conn)
                               (string= (plist-get conn :host) "host1.com"))
                             saved-data))
            (should (cl-some (lambda (conn)
                               (string= (plist-get conn :host) "host2.com"))
                             saved-data)))))
    (test-etm-remote-layout-teardown)))

(ert-deftest test-etm-remote-layout-restore-connections ()
  "Test restoring connections when loading a layout."
  (test-etm-remote-layout-setup)
  (unwind-protect
      (progn
        (require 'etm-remote-layout)
        
        ;; Mock connection data
        (let ((connection-data
               '((:method "ssh" :user "user" :host "host1.com" :port nil)
                 (:method "ssh" :user "user" :host "host2.com" :port 2222))))
          
          ;; Restore connections
          (etm-remote-layout-restore-connections connection-data)
          
          ;; Check connections were created
          (let ((tab-connections (etm-remote--get-tab-connections)))
            (should (gethash "host1.com" tab-connections))
            (should (gethash "host2.com" tab-connections))
            
            ;; Check connection properties
            (let ((conn1 (gethash "host1.com" tab-connections)))
              (should (equal (etm-remote-connection-method conn1) "ssh"))
              (should (equal (etm-remote-connection-user conn1) "user"))))))
    (test-etm-remote-layout-teardown)))

(ert-deftest test-etm-remote-layout-offline-mode ()
  "Test offline mode behavior."
  (test-etm-remote-layout-setup)
  (unwind-protect
      (progn
        (require 'etm-remote-layout)
        
        ;; Enable offline mode
        (etm-remote-layout-offline-mode t)
        (should etm-remote-layout-offline-enabled)
        
        ;; Test that connections are not attempted in offline mode
        (let ((connection-data
               '((:method "ssh" :user "user" :host "remote.com" :port nil))))
          
          ;; This should not create actual connections
          (etm-remote-layout-restore-connections connection-data)
          
          ;; Connections should be marked as offline
          (let* ((tab-connections (etm-remote--get-tab-connections))
                 (conn (gethash "remote.com" tab-connections)))
            (should conn)
            (should (eq (etm-remote-connection-status conn) :offline)))))
    (test-etm-remote-layout-teardown)))

(ert-deftest test-etm-remote-layout-migration ()
  "Test migration of old layout formats."
  (test-etm-remote-layout-setup)
  (unwind-protect
      (progn
        (require 'etm-remote-layout)
        
        ;; Test old format detection
        (let ((old-layout-content
               "(--etm-layout-create-from-positions \"test\"
  '((file \"/home/user/file.txt\" 0 0 80 40 nil)
    (shell \"/ssh:user@host1.com:/home/user\" 80 0 80 40 nil))
  nil)"))
          
          (should (etm-remote-layout-needs-migration-p old-layout-content))
          
          ;; Test migration
          (let ((migrated (etm-remote-layout-migrate old-layout-content)))
            (should migrated)
            (should (string-match "etm-remote-layout-connections" migrated)))))
    (test-etm-remote-layout-teardown)))

(ert-deftest test-etm-remote-layout-integration ()
  "Test full integration with layout save/load."
  (test-etm-remote-layout-setup)
  (unwind-protect
      (progn
        (require 'etm-remote-layout)
        
        ;; Create a connection
        (let ((conn (etm-remote-connect "ssh" "user" "test.com")))
          (setf (etm-remote-connection-status conn) :connected)
          
          ;; Test connection formatting
          (let ((formatted (etm-remote-layout-format-connections
                            (etm-remote-layout-save-connections))))
            (should formatted)
            (should (string-match "etm-remote-layout-connections" formatted))
            (should (string-match "test.com" formatted)))))
    (test-etm-remote-layout-teardown)))

;; Run tests if executed directly
;; (when (and (boundp 'load-file-name) load-file-name)
;;   (ert-run-tests-batch-and-exit))

(provide 'test-etm-remote-layout)
;;; test-etm-remote-layout.el ends here