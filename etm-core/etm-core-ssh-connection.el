;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 19:55:56>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-core/etm-core-ssh-connection.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; Commentary:
;; SSH connection management for ETM
;; Provides functions for creating, tracking, and reusing SSH connections
;; using SSH ControlMaster functionality

(require 'etm-core-variables)
(require 'etm-core-ssh-helpers)

;; 1. SSH connection variables
;; ----------------------------------------

(defvar etm-ssh-connections (make-hash-table :test 'equal)
  "Hash table mapping tab names to their SSH connections.")

;; 2. SSH connection functions
;; ----------------------------------------

(defun --etm-get-or-create-ssh-connection (host)
  "Get existing SSH connection to HOST or create a new one if needed.
Returns the connection identifier that can be used by terminal sessions."
  (let* ((connection-pattern (format "^\\.control.*:%s:" host))
         (existing-connections (directory-files "~/.ssh" nil connection-pattern))
         (connection-id nil))
    
    ;; Check if we have a valid existing connection
    (if existing-connections
        (progn
          (setq connection-id (car existing-connections))
          (message "Reusing existing SSH connection to %s" host))
      
      ;; No valid connection exists, create a new one
      (message "Creating new SSH connection to %s" host)
      (let ((connection-process 
             (start-process-shell-command 
              (format "ssh-connect-%s" host)
              nil
              (format "ssh -o ControlMaster=auto -o ControlPersist=1h %s true" host))))
        ;; Wait briefly for connection to establish
        (sleep-for 0.5)
        ;; Find the newly created connection
        (setq connection-id 
              (car (directory-files "~/.ssh" nil connection-pattern)))))
    
    connection-id))

(defun etm-cleanup-unused-connections ()
  "Cleanup SSH connections that are no longer needed.
This respects the ControlPersist setting in SSH."
  (interactive)
  (message "SSH connections will be automatically closed based on ControlPersist settings"))

;; 3. Connection tracking functions
;; ----------------------------------------

(defun --etm-register-ssh-connection (tab-name host connection-id)
  "Register CONNECTION-ID for HOST with TAB-NAME."
  (puthash tab-name (cons host connection-id) etm-ssh-connections)
  (message "Tab '%s' using SSH connection %s to %s" 
           tab-name connection-id host))

(defun --etm-unregister-ssh-connection (tab-name)
  "Unregister SSH connection for TAB-NAME."
  (remhash tab-name etm-ssh-connections))

(defun --etm-get-tab-ssh-connection (tab-name)
  "Get SSH connection for TAB-NAME.
Returns (host . connection-id) or nil if no connection exists."
  (gethash tab-name etm-ssh-connections nil))

(provide 'etm-core-ssh-connection)

(when (not load-file-name)
  (message "etm-core-ssh-connection.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))