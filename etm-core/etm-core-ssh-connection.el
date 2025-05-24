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

(defvar etm-ssh-debug t
  "When non-nil, enable verbose SSH connection debugging messages.")

(defun etm-toggle-ssh-debug ()
  "Toggle SSH connection debugging."
  (interactive)
  (setq etm-ssh-debug (not etm-ssh-debug))
  (message "SSH connection debugging: %s" 
           (if etm-ssh-debug "ENABLED" "DISABLED")))

(defun --etm-ssh-log (format-string &rest args)
  "Log SSH connection debugging info if enabled.
FORMAT-STRING and ARGS are passed to `message'."
  (when etm-ssh-debug
    (apply #'message (concat "[ETM SSH] " format-string) args)))

;; 2. SSH connection functions
;; ----------------------------------------

(defun --etm-get-or-create-ssh-connection (host)
  "Get existing SSH connection to HOST or create a new one if needed.
Returns the connection identifier that can be used by terminal sessions."
  ;; Convert 'l' to 'localhost' if specified
  (when (string= host "l")
    (setq host "localhost"))
    
  (--etm-ssh-log "Attempting to get or create SSH connection to %s" host)
  (let* ((connection-pattern (format "^\\.control.*:%s:" host))
         (existing-connections (directory-files "~/.ssh" nil connection-pattern))
         (connection-id nil))
    
    ;; Check if we have a valid existing connection
    (if existing-connections
        (progn
          (setq connection-id (car existing-connections))
          (--etm-ssh-log "Found existing connection: %s" connection-id)
          (message "Reusing existing SSH connection to %s" host))
      
      ;; No valid connection exists, create a new one
      (--etm-ssh-log "No existing connection found, creating new one")
      (message "Creating new SSH connection to %s" host)
      (let ((connection-process 
             (start-process-shell-command 
              (format "ssh-connect-%s" host)
              nil
              (format "ssh -o ControlMaster=auto -o ControlPersist=1h %s true" host))))
        ;; Wait briefly for connection to establish
        (--etm-ssh-log "Waiting for connection to establish...")
        (sleep-for 0.5)
        ;; Find the newly created connection
        (setq connection-id 
              (car (directory-files "~/.ssh" nil connection-pattern)))
        (if connection-id
            (--etm-ssh-log "Created new connection: %s" connection-id)
          (--etm-ssh-log "WARNING: Failed to create connection to %s" host))))
    
    connection-id))

(defun etm-cleanup-unused-connections ()
  "Cleanup SSH connections that are no longer needed.
This actively terminates idle connections and respects the ControlPersist setting."
  (interactive)
  (let ((active-connections '())
        (all-connections '())
        (closed-count 0))
    
    (--etm-ssh-log "Starting SSH connection cleanup")
    
    ;; Build list of active connections from tabs
    (maphash (lambda (_tab-name connection-info) 
               (when connection-info
                 (push (cdr connection-info) active-connections)
                 (--etm-ssh-log "Active connection: %s for host %s" 
                               (cdr connection-info)
                               (car connection-info))))
             etm-ssh-connections)
    
    (--etm-ssh-log "Found %d active connections" (length active-connections))
    
    ;; Find all control socket files in ~/.ssh
    (setq all-connections (directory-files "~/.ssh" t "^\\.control.*:[0-9]+-"))
    (--etm-ssh-log "Found %d total connection files" (length all-connections))
    
    ;; Close connections that aren't actively in use
    (dolist (connection all-connections)
      (let ((connection-id (file-name-nondirectory connection)))
        (--etm-ssh-log "Checking connection: %s" connection-id)
        (unless (member connection-id active-connections)
          ;; Not in use by any tab, close it
          (--etm-ssh-log "Closing unused connection: %s" connection-id)
          (message "Closing unused SSH connection: %s" connection-id)
          (call-process "ssh" nil nil nil "-O" "exit" "-o" (format "ControlPath=%s" connection))
          (setq closed-count (1+ closed-count)))))
    
    (--etm-ssh-log "Cleanup complete. Closed %d connections" closed-count)
    (if (> closed-count 0)
        (message "Closed %d unused SSH connection(s)" closed-count)
      (message "No unused SSH connections found"))))

;; 3. Connection tracking functions
;; ----------------------------------------

(defun --etm-register-ssh-connection (tab-name host connection-id)
  "Register CONNECTION-ID for HOST with TAB-NAME."
  (if (not (and tab-name host connection-id))
      (progn
        (--etm-ssh-log "WARNING: Incomplete SSH connection info - tab:%s host:%s conn:%s"
                      (or tab-name "nil")
                      (or host "nil")
                      (or connection-id "nil"))
        nil)  ; Return nil on error
    
    (--etm-ssh-log "Registering SSH connection for tab '%s': %s@%s" 
                  tab-name connection-id host)
    (puthash tab-name (cons host connection-id) etm-ssh-connections)
    (message "Tab '%s' using SSH connection %s to %s" 
             tab-name connection-id host)))

(defun --etm-unregister-ssh-connection (tab-name)
  "Unregister SSH connection for TAB-NAME."
  (let ((connection (gethash tab-name etm-ssh-connections nil)))
    (when connection
      (--etm-ssh-log "Unregistering SSH connection for tab '%s': %s@%s" 
                    tab-name 
                    (cdr connection)
                    (car connection)))
    (remhash tab-name etm-ssh-connections)))

(defun --etm-get-tab-ssh-connection (tab-name)
  "Get SSH connection for TAB-NAME.
Returns (host . connection-id) or nil if no connection exists."
  (let ((connection (gethash tab-name etm-ssh-connections nil)))
    (when connection
      (--etm-ssh-log "Retrieved SSH connection for tab '%s': %s@%s" 
                    tab-name 
                    (cdr connection)
                    (car connection)))
    connection))

(provide 'etm-core-ssh-connection)

(when (not load-file-name)
  (message "etm-core-ssh-connection.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))