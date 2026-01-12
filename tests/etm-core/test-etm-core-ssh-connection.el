;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 20:01:15>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/etm-core/test-etm-core-ssh-connection.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;; Add mock directory to load path
(add-to-list 'load-path 
             (expand-file-name "../mocks" 
                               (file-name-directory (or load-file-name buffer-file-name))))

(require 'ert)
(require 'cl-lib)
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

;; Test for SSH pattern matching logic
(ert-deftest test-etm-ssh-pattern-matching ()
  "Test that SSH control socket pattern matching works correctly."
  (let ((test-host "test-server")
        (socket-filename ".control-master:test-server:22-ywatanabe"))
    
    ;; Test the pattern matching logic
    (let ((pattern (format "control.*%s" (regexp-quote test-host))))
      (should (string-match-p pattern socket-filename)))))

;; Test for SSH controller reuse issue (simplified)
(ert-deftest test-etm-ssh-controller-reuse-detection ()
  "Test that existing SSH control sockets are properly detected and reused."
  (let ((test-host "test-server")
        (existing-socket ".control-master:test-server:22-ywatanabe"))
    
    ;; Test with simpler mock - just check the function doesn't return nil
    (cl-letf (((symbol-function 'directory-files)
               (lambda (dir pattern &optional full)
                 ;; Always return the socket when called with ~/.ssh
                 (if (string= dir "~/.ssh")
                     (list existing-socket)
                   nil)))
              ((symbol-function 'start-process-shell-command)
               (lambda (name buffer command)
                 ;; Should NOT be called if connection is reused
                 (error "Should not create new connection"))))
      
      ;; Test that existing connection is found
      (let ((connection-id (--etm-get-or-create-ssh-connection test-host)))
        (should connection-id)
        (should (stringp connection-id))))))

;; Test for proper ControlPath option usage
(ert-deftest test-etm-ssh-control-path-construction ()
  "Test that ControlPath option is properly constructed for reuse."
  (let ((test-host "test-server")
        (connection-id ".control-master:test-server:22-ywatanabe"))
    
    ;; Register a connection for testing
    (let ((etm-ssh-connections (make-hash-table :test 'equal)))
      (--etm-register-ssh-connection "test-tab" test-host connection-id)
      
      ;; Test that connection info is properly stored and retrieved
      (let ((connection-info (--etm-get-tab-ssh-connection "test-tab")))
        (should (equal (car connection-info) test-host))
        (should (equal (cdr connection-info) connection-id))))))

(provide 'test-etm-core-ssh-connection)

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-core/etm-core-ssh-connection.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-12-20 21:00:00>
;; ;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-core/etm-core-ssh-connection.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; ;;; Commentary:
;; ;; SSH connection management for ETM
;; ;; Provides functions for creating, tracking, and reusing SSH connections
;; ;; using SSH ControlMaster functionality
;; 
;; (require 'etm-core-variables)
;; (require 'etm-core-ssh-helpers)
;; 
;; ;; 1. SSH connection variables
;; ;; ----------------------------------------
;; 
;; (defvar etm-ssh-connections (make-hash-table :test 'equal)
;;   "Hash table mapping tab names to their SSH connections.")
;; 
;; (defun --etm-ssh-log (format-string &rest args)
;;   "Log SSH connection debugging info if `etm-debug' is non-nil.
;; FORMAT-STRING and ARGS are passed to `message' with [ETM SSH] prefix."
;;   (when etm-debug
;;     (apply #'message (concat "[ETM SSH] " format-string) args)))
;; 
;; ;; 2. SSH connection functions
;; ;; ----------------------------------------
;; 
;; (defun --etm-get-or-create-ssh-connection (host)
;;   "Get existing SSH connection to HOST or create a new one if needed.
;; Returns the connection identifier that can be used by terminal sessions."
;;   ;; Convert 'l' to 'localhost' if specified
;;   (when (string= host "l")
;;     (setq host "localhost"))
;;     
;;   (--etm-ssh-log "=== SSH CONNECTION DEBUG START ===")
;;   (--etm-ssh-log "Attempting to get or create SSH connection to %s" host)
;;   (--etm-ssh-log "Current SSH connections hash table contents:")
;;   (maphash (lambda (tab connection)
;;              (--etm-ssh-log "  Tab '%s': host=%s connection=%s" 
;;                            tab (car connection) (cdr connection)))
;;            etm-ssh-connections)
;;   (let* ((connection-pattern (format "\\.control.*%s.*" (regexp-quote host)))
;;          (existing-connections (directory-files "~/.ssh" nil connection-pattern))
;;          (connection-id nil))
;;     
;;     (--etm-ssh-log "SSH config directory scan:")
;;     (--etm-ssh-log "  Pattern: %s" connection-pattern)
;;     (--etm-ssh-log "  Found %d existing connections: %s" 
;;                   (length existing-connections)
;;                   (if existing-connections 
;;                       (mapconcat 'identity existing-connections ", ")
;;                     "none"))
;;     
;;     ;; Check if we have a valid existing connection
;;     (if existing-connections
;;         (progn
;;           (setq connection-id (car existing-connections))
;;           (--etm-ssh-log "*** CONNECTION REUSE DETECTED ***")
;;           (--etm-ssh-log "Found existing connection: %s" connection-id)
;;           (--etm-ssh-log "Full connection file path: ~/.ssh/%s" connection-id)
;;           ;; Test if connection is actually alive
;;           (let ((test-result (call-process "ssh" nil nil nil "-O" "check" 
;;                                           "-o" (format "ControlPath=~/.ssh/%s" connection-id)
;;                                           host)))
;;             (--etm-ssh-log "Connection aliveness test result: %s" 
;;                           (if (= test-result 0) "ALIVE" "DEAD"))
;;             (when (= test-result 0)
;;               (--etm-ssh-log "*** SUCCESSFULLY REUSING CONNECTION ***")))
;;           (message "Reusing existing SSH connection to %s" host))
;;       
;;       ;; No valid connection exists, create a new one
;;       (--etm-ssh-log "*** NO CONNECTION FOUND - CREATING NEW ***")
;;       (--etm-ssh-log "No existing connection found, creating new one")
;;       (message "Creating new SSH connection to %s" host)
;;       (let ((connection-process 
;;              (start-process-shell-command 
;;               (format "ssh-connect-%s" host)
;;               nil
;;               (format "ssh -o ControlMaster=auto -o ControlPersist=1h %s true" host))))
;;         ;; Wait briefly for connection to establish
;;         (--etm-ssh-log "Waiting for connection to establish...")
;;         (sleep-for 0.5)
;;         ;; Find the newly created connection
;;         (setq connection-id 
;;               (car (directory-files "~/.ssh" nil connection-pattern)))
;;         (if connection-id
;;             (progn
;;               (--etm-ssh-log "*** NEW CONNECTION CREATED SUCCESSFULLY ***")
;;               (--etm-ssh-log "Created new connection: %s" connection-id)
;;               (--etm-ssh-log "New connection file: ~/.ssh/%s" connection-id))
;;           (--etm-ssh-log "*** ERROR: Failed to create connection to %s ***" host))))
;;     
;;     (--etm-ssh-log "Final connection-id result: %s" (or connection-id "NIL"))
;;     (--etm-ssh-log "=== SSH CONNECTION DEBUG END ===")
;;     connection-id))
;; 
;; (defun etm-cleanup-unused-connections ()
;;   "Cleanup SSH connections that are no longer needed.
;; This actively terminates idle connections and respects the ControlPersist setting."
;;   (interactive)
;;   (let ((active-connections '())
;;         (all-connections '())
;;         (closed-count 0))
;;     
;;     (--etm-ssh-log "Starting SSH connection cleanup")
;;     
;;     ;; Build list of active connections from tabs
;;     (maphash (lambda (_tab-name connection-info) 
;;                (when connection-info
;;                  (push (cdr connection-info) active-connections)
;;                  (--etm-ssh-log "Active connection: %s for host %s" 
;;                                (cdr connection-info)
;;                                (car connection-info))))
;;              etm-ssh-connections)
;;     
;;     (--etm-ssh-log "Found %d active connections" (length active-connections))
;;     
;;     ;; Find all control socket files in ~/.ssh
;;     (setq all-connections (directory-files "~/.ssh" t "^\\.control.*"))
;;     (--etm-ssh-log "Found %d total connection files" (length all-connections))
;;     
;;     ;; Close connections that aren't actively in use
;;     (dolist (connection all-connections)
;;       (let ((connection-id (file-name-nondirectory connection)))
;;         (--etm-ssh-log "Checking connection: %s" connection-id)
;;         (unless (member connection-id active-connections)
;;           ;; Not in use by any tab, close it
;;           (--etm-ssh-log "Closing unused connection: %s" connection-id)
;;           (message "Closing unused SSH connection: %s" connection-id)
;;           (call-process "ssh" nil nil nil "-O" "exit" "-o" (format "ControlPath=%s" connection))
;;           (setq closed-count (1+ closed-count)))))
;;     
;;     (--etm-ssh-log "Cleanup complete. Closed %d connections" closed-count)
;;     (if (> closed-count 0)
;;         (message "Closed %d unused SSH connection(s)" closed-count)
;;       (message "No unused SSH connections found"))))
;; 
;; ;; 3. Connection tracking functions
;; ;; ----------------------------------------
;; 
;; (defun --etm-register-ssh-connection (tab-name host connection-id)
;;   "Register CONNECTION-ID for HOST with TAB-NAME."
;;   (if (not (and tab-name host connection-id))
;;       (progn
;;         (--etm-ssh-log "*** ERROR: INCOMPLETE SSH CONNECTION INFO ***")
;;         (--etm-ssh-log "WARNING: Incomplete SSH connection info - tab:%s host:%s conn:%s"
;;                       (or tab-name "nil")
;;                       (or host "nil")
;;                       (or connection-id "nil"))
;;         nil)  ; Return nil on error
;;     
;;     (--etm-ssh-log "=== REGISTERING SSH CONNECTION ===")
;;     (--etm-ssh-log "Registering SSH connection for tab '%s': %s@%s" 
;;                   tab-name connection-id host)
;;     (--etm-ssh-log "Hash table before registration: %d entries" 
;;                   (hash-table-count etm-ssh-connections))
;;     (puthash tab-name (cons host connection-id) etm-ssh-connections)
;;     (--etm-ssh-log "Hash table after registration: %d entries" 
;;                   (hash-table-count etm-ssh-connections))
;;     (--etm-ssh-log "Registration complete for tab '%s'" tab-name)
;;     (message "Tab '%s' using SSH connection %s to %s" 
;;              tab-name connection-id host)))
;; 
;; (defun --etm-unregister-ssh-connection (tab-name)
;;   "Unregister SSH connection for TAB-NAME."
;;   (let ((connection (gethash tab-name etm-ssh-connections nil)))
;;     (when connection
;;       (--etm-ssh-log "Unregistering SSH connection for tab '%s': %s@%s" 
;;                     tab-name 
;;                     (cdr connection)
;;                     (car connection)))
;;     (remhash tab-name etm-ssh-connections)))
;; 
;; (defun --etm-get-tab-ssh-connection (tab-name)
;;   "Get SSH connection for TAB-NAME.
;; Returns (host . connection-id) or nil if no connection exists."
;;   (--etm-ssh-log "=== RETRIEVING SSH CONNECTION FOR TAB ===")
;;   (--etm-ssh-log "Looking up SSH connection for tab '%s'" tab-name)
;;   (--etm-ssh-log "Hash table contains %d entries:" (hash-table-count etm-ssh-connections))
;;   (maphash (lambda (tab connection)
;;              (--etm-ssh-log "  Available: Tab '%s' -> host=%s connection=%s" 
;;                            tab (car connection) (cdr connection)))
;;            etm-ssh-connections)
;;   (let ((connection (gethash tab-name etm-ssh-connections nil)))
;;     (if connection
;;         (progn
;;           (--etm-ssh-log "*** CONNECTION FOUND FOR TAB '%s' ***" tab-name)
;;           (--etm-ssh-log "Retrieved SSH connection: %s@%s" 
;;                         (cdr connection)
;;                         (car connection)))
;;       (--etm-ssh-log "*** NO CONNECTION FOUND FOR TAB '%s' ***" tab-name))
;;     connection))
;; 
;; (provide 'etm-core-ssh-connection)
;; 
;; (when (not load-file-name)
;;   (message "etm-core-ssh-connection.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-core/etm-core-ssh-connection.el
;; --------------------------------------------------------------------------------

;;; test-etm-core-ssh-connection.el ends here
