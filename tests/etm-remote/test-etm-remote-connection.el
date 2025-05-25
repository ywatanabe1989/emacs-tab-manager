;;; test-etm-remote-connection.el --- Tests for ETM remote connection management -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Time-stamp: <2025-05-25 16:02:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/etm-remote/test-etm-remote-connection.el

;;; Commentary:
;; Tests for remote connection management functionality

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Mock TRAMP functions for testing
(defvar test-etm-remote--tramp-connections nil
  "Mock storage for TRAMP connections.")

(defun test-etm-remote--mock-file-remote-p (filename &optional identification connected)
  "Mock version of `file-remote-p'."
  (when (string-match "^/\\([^:/]+\\):\\([^@]+\\)@\\([^:#/]+\\)\\(?:#[0-9]+\\)?:" filename)
    (if identification
        (cond
         ((eq identification 'method)
          (match-string 1 filename))
         ((eq identification 'user)
          (match-string 2 filename))
         ((eq identification 'host)
          (match-string 3 filename))
         (t filename))
      filename)))

(defun test-etm-remote--mock-tramp-dissect-file-name (filename)
  "Mock version of `tramp-dissect-file-name'."
  (when (string-match "^/\\([^:/]+\\):\\([^@/]+\\)@\\([^:#/]+\\)\\(?:#\\([0-9]+\\)\\)?:\\(.*\\)$" filename)
    (let ((method (match-string 1 filename))
          (user (match-string 2 filename))
          (host (match-string 3 filename))
          (port (match-string 4 filename))
          (localname (match-string 5 filename)))
      (make-tramp-file-name :method method
                            :user user
                            :host host
                            :port port
                            :localname localname))))

(defun test-etm-remote--mock-tramp-get-connection-property (vec property &optional default)
  "Mock version of `tramp-get-connection-property'."
  (let* ((method (tramp-file-name-method vec))
         (user (tramp-file-name-user vec))
         (host (tramp-file-name-host vec))
         (key (format "%s:%s@%s" method user host)))
    (let ((conn-props (alist-get key test-etm-remote--tramp-connections nil nil #'string=)))
      (if conn-props
          (or (alist-get property conn-props) default)
        ;; If no connection exists and we're checking last-ping, throw error
        (if (eq property 'last-ping)
            (error "No connection")
          default)))))

(defun test-etm-remote--mock-tramp-cleanup-connection (vec)
  "Mock version of `tramp-cleanup-connection'."
  (let* ((method (tramp-file-name-method vec))
         (user (tramp-file-name-user vec))
         (host (tramp-file-name-host vec))
         (key (format "%s:%s@%s" method user host)))
    (setq test-etm-remote--tramp-connections
          (assoc-delete-all key test-etm-remote--tramp-connections))))

(defun test-etm-remote--mock-tramp-make-tramp-file-name (&rest args)
  "Mock version of `tramp-make-tramp-file-name'."
  (if (= (length args) 1)
      ;; New style with plist
      (let* ((plist (car args))
             (method (plist-get plist :method))
             (user (plist-get plist :user))
             (host (plist-get plist :host)))
        (make-tramp-file-name :method method :user user :host host))
    ;; Handle other cases
    (make-tramp-file-name)))

;; Test setup
(defun test-etm-remote--setup ()
  "Set up test environment."
  (setq test-etm-remote--tramp-connections nil)
  ;; Mock TRAMP functions
  (advice-add 'file-remote-p :override #'test-etm-remote--mock-file-remote-p)
  (advice-add 'tramp-dissect-file-name :override #'test-etm-remote--mock-tramp-dissect-file-name)
  (advice-add 'tramp-get-connection-property :override #'test-etm-remote--mock-tramp-get-connection-property)
  (advice-add 'tramp-cleanup-connection :override #'test-etm-remote--mock-tramp-cleanup-connection)
  (advice-add 'tramp-make-tramp-file-name :override #'test-etm-remote--mock-tramp-make-tramp-file-name)
  ;; Load module - it should be in the load path already
  (require 'etm-remote-connection))

(defun test-etm-remote--teardown ()
  "Clean up test environment."
  (advice-remove 'file-remote-p #'test-etm-remote--mock-file-remote-p)
  (advice-remove 'tramp-dissect-file-name #'test-etm-remote--mock-tramp-dissect-file-name)
  (advice-remove 'tramp-get-connection-property #'test-etm-remote--mock-tramp-get-connection-property)
  (advice-remove 'tramp-cleanup-connection #'test-etm-remote--mock-tramp-cleanup-connection)
  (advice-remove 'tramp-make-tramp-file-name #'test-etm-remote--mock-tramp-make-tramp-file-name))

;; Tests
(ert-deftest test-etm-remote-parse-remote-path ()
  "Test parsing remote file paths."
  (test-etm-remote--setup)
  (unwind-protect
      (progn
        (require 'etm-remote-connection)
        ;; Test SSH path
        (let ((info (etm-remote-parse-path "/ssh:user@host:/path/to/file")))
          (should (equal (alist-get 'method info) "ssh"))
          (should (equal (alist-get 'user info) "user"))
          (should (equal (alist-get 'host info) "host"))
          (should (equal (alist-get 'path info) "/path/to/file")))
        
        ;; Test SCP path with port
        (let ((info (etm-remote-parse-path "/scp:user@host#2222:/path/to/file")))
          (should (equal (alist-get 'method info) "scp"))
          (should (equal (alist-get 'user info) "user"))
          (should (equal (alist-get 'host info) "host"))
          (should (equal (alist-get 'port info) "2222"))
          (should (equal (alist-get 'path info) "/path/to/file")))
        
        ;; Test sudo path
        (let ((info (etm-remote-parse-path "/sudo:root@localhost:/etc/hosts")))
          (should (equal (alist-get 'method info) "sudo"))
          (should (equal (alist-get 'user info) "root"))
          (should (equal (alist-get 'host info) "localhost")))
        
        ;; Test local path returns nil
        (should-not (etm-remote-parse-path "/home/user/file.el")))
    (test-etm-remote--teardown)))

(ert-deftest test-etm-remote-connection-management ()
  "Test connection creation and management."
  (test-etm-remote--setup)
  (unwind-protect
      (progn
        (require 'etm-remote-connection)
        ;; Mock tab-bar--current-tab to return a consistent value
        (cl-letf (((symbol-function 'tab-bar--current-tab) (lambda () '((name . "test-tab")))))
          ;; Test connection creation
          (let ((conn (etm-remote-connect "ssh" "user" "host")))
            (should conn)
            (should (equal (etm-remote-connection-method conn) "ssh"))
            (should (equal (etm-remote-connection-user conn) "user"))
            (should (equal (etm-remote-connection-host conn) "host"))
            ;; Status could be either :connecting or :connected depending on mock behavior
            (should (memq (etm-remote-connection-status conn) '(:connecting :connected))))
          
          ;; Test connection with port
          (let ((conn (etm-remote-connect "scp" "user" "host" "2222")))
            (should (equal (etm-remote-connection-port conn) "2222")))
          
          ;; Test get existing connection
          (etm-remote-connect "ssh" "user" "host")
          (let ((conn (etm-remote-get-connection "host")))
            (should conn)
            (should (equal (etm-remote-connection-host conn) "host")))))
    (test-etm-remote--teardown)))

(ert-deftest test-etm-remote-connection-health-check ()
  "Test connection health checking."
  (test-etm-remote--setup)
  (unwind-protect
      (progn
        (require 'etm-remote-connection)
        (cl-letf (((symbol-function 'tab-bar--current-tab) (lambda () '((name . "test-tab")))))
          ;; Create connection
          (let ((conn (etm-remote-connect "ssh" "user" "host")))
            ;; Initially could be connecting or connected
            (should (memq (etm-remote-connection-status conn) '(:connecting :connected)))
            
            ;; Mock successful connection
            (setq test-etm-remote--tramp-connections
                  `(("ssh:user@host" . ((last-ping . ,(float-time))))))
            
            ;; Check health - should succeed
            (should (etm-remote-check-connection "host"))
            (should (equal (etm-remote-connection-status conn) :connected))
            
            ;; Test reconnection functionality
            (should (etm-remote-reconnect "host"))
            ;; Status should still be connected or connecting after reconnect
            (should (memq (etm-remote-connection-status conn) '(:connecting :connected))))))
    (test-etm-remote--teardown)))

(ert-deftest test-etm-remote-cleanup ()
  "Test connection cleanup."
  (test-etm-remote--setup)
  (unwind-protect
      (progn
        (require 'etm-remote-connection)
        (cl-letf (((symbol-function 'tab-bar--current-tab) (lambda () '((name . "test-tab")))))
          ;; Create multiple connections
          (etm-remote-connect "ssh" "user1" "host1")
          (etm-remote-connect "ssh" "user2" "host2")
          (etm-remote-connect "scp" "user3" "host3")
          
          ;; Verify connections exist
          (should (etm-remote-get-connection "host1"))
          (should (etm-remote-get-connection "host2"))
          (should (etm-remote-get-connection "host3"))
          
          ;; Disconnect one
          (etm-remote-disconnect "host2")
          (should (etm-remote-get-connection "host1"))
          (should-not (etm-remote-get-connection "host2"))
          (should (etm-remote-get-connection "host3"))
          
          ;; Cleanup all
          (etm-remote-cleanup-all)
          (should-not (etm-remote-get-connection "host1"))
          (should-not (etm-remote-get-connection "host3"))))
    (test-etm-remote--teardown)))

(ert-deftest test-etm-remote-buffer-detection ()
  "Test remote buffer detection."
  (test-etm-remote--setup)
  (unwind-protect
      (progn
        (require 'etm-remote-connection)
        ;; Test remote file buffer
        (with-temp-buffer
          (setq buffer-file-name "/ssh:user@host:/path/to/file.el")
          (should (etm-remote-buffer-p (current-buffer)))
          (should (equal (etm-remote-buffer-host (current-buffer)) "host")))
        
        ;; Test local file buffer
        (with-temp-buffer
          (setq buffer-file-name "/home/user/file.el")
          (should-not (etm-remote-buffer-p (current-buffer)))
          (should-not (etm-remote-buffer-host (current-buffer))))
        
        ;; Test non-file buffer
        (with-temp-buffer
          (should-not (etm-remote-buffer-p (current-buffer)))))
    (test-etm-remote--teardown)))

(provide 'test-etm-remote-connection)