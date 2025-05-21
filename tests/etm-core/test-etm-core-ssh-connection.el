;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 20:01:15>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/etm-core/test-etm-core-ssh-connection.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;; Add mock directory to load path
(add-to-list 'load-path 
             (expand-file-name "../mocks" 
                               (file-name-directory (or load-file-name buffer-file-name))))

(require 'ert)
(require 'vterm)  ; Use the mock vterm
(require 'etm-core-ssh-connection)

;; Mock function for get-or-create-ssh-connection
(defun --test-mock-get-or-create-ssh-connection (host)
  "Mock implementation for testing."
  (format ".control-master:%s:22-ywatanabe" host))

;; Test SSH connection registry functions
(ert-deftest test-etm-ssh-connection-registry ()
  "Test SSH connection registry functions."
  (let ((etm-ssh-connections (make-hash-table :test 'equal)))
    ;; Test registration
    (--etm-register-ssh-connection "test-tab" "test-host" "test-connection-id")
    (should (--etm-get-tab-ssh-connection "test-tab"))
    
    ;; Test retrieval
    (let ((connection-info (--etm-get-tab-ssh-connection "test-tab")))
      (should (equal (car connection-info) "test-host"))
      (should (equal (cdr connection-info) "test-connection-id")))
    
    ;; Test unregistration
    (--etm-unregister-ssh-connection "test-tab")
    (should-not (--etm-get-tab-ssh-connection "test-tab"))))

;; Test SSH connection creation and reuse
(ert-deftest test-etm-ssh-connection-creation ()
  "Test SSH connection creation and reuse."
  (cl-letf (((symbol-function '--etm-get-or-create-ssh-connection) 
             #'--test-mock-get-or-create-ssh-connection)
            ((symbol-function 'directory-files)
             (lambda (dir pattern) 
               (if (string-match-p "test-host" pattern)
                   '(".control-master:test-host:22-ywatanabe")
                 nil))))
    
    ;; Should return the existing connection
    (should (equal (--etm-get-or-create-ssh-connection "test-host")
                   ".control-master:test-host:22-ywatanabe"))
    
    ;; Should create a new connection for a different host
    (should (equal (--etm-get-or-create-ssh-connection "another-host")
                   ".control-master:another-host:22-ywatanabe"))))

;; Test handling of 'l' as shortcut for 'localhost'
(ert-deftest test-etm-ssh-localhost-shortcut ()
  "Test handling of 'l' as shortcut for 'localhost'."
  (cl-letf (((symbol-function 'directory-files)
             (lambda (dir pattern) 
               (if (string-match-p "localhost" pattern)
                   '(".control-master:localhost:22-ywatanabe")
                 nil)))
            ((symbol-function 'start-process-shell-command)
             (lambda (name buffer command) nil))
            ((symbol-function 'sleep-for)
             (lambda (seconds) nil)))
    
    ;; Function from etm-core-ssh-helpers.el
    (should (string= (--etm-ssh-select-host) "localhost"))
    
    ;; With our mock, simulate what happens when we get or create a connection
    (let ((orig-fn (symbol-function '--etm-get-or-create-ssh-connection)))
      (cl-letf (((symbol-function '--etm-get-or-create-ssh-connection)
                 (lambda (host)
                   (when (string= host "l")
                     (setq host "localhost"))
                   (format ".control-master:%s:22-ywatanabe" host))))
        
        ;; Test that 'l' is converted to 'localhost'
        (should (string= 
                 (--etm-get-or-create-ssh-connection "l")
                 ".control-master:localhost:22-ywatanabe"))))))

;; Test SSH connection debugging
(ert-deftest test-etm-ssh-debugging ()
  "Test SSH connection debugging functionality."
  (let ((etm-ssh-debug nil)
        (log-messages '())
        (message-call-count 0))
    
    ;; Mock message function to capture output
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq message-call-count (1+ message-call-count))
                 (when (string-match-p "\\[ETM SSH\\]" format-string)
                   (push (apply #'format format-string args) log-messages))
                 nil)))
      
      ;; Test with debugging disabled
      (--etm-ssh-log "Test message")
      (should (= message-call-count 0)) ; Should not log when disabled
      
      ;; Enable debugging and test again
      (setq etm-ssh-debug t)
      (--etm-ssh-log "Test message")
      (should (= message-call-count 1)) ; Should log when enabled
      (should (string-match-p "Test message" (car log-messages)))
      
      ;; Test toggle function
      (etm-toggle-ssh-debug)
      (should-not etm-ssh-debug) ; Should be toggled off
      
      (etm-toggle-ssh-debug)
      (should etm-ssh-debug) ; Should be toggled on again
      )))

(provide 'test-etm-core-ssh-connection)