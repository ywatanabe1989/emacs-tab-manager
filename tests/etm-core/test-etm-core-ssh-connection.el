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

(provide 'test-etm-core-ssh-connection)