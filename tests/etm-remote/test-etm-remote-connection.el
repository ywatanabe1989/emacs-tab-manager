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

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-remote/etm-remote-connection.el
;; --------------------------------------------------------------------------------
;; ;;; etm-remote-connection.el --- ETM remote connection management -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Time-stamp: <2025-05-25 15:54:00>
;; ;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-remote/etm-remote-connection.el
;; 
;; ;;; Commentary:
;; ;; This module provides enhanced remote connection management for ETM,
;; ;; supporting multiple TRAMP methods with health monitoring and automatic
;; ;; reconnection capabilities.
;; 
;; ;;; Code:
;; 
;; (require 'tramp)
;; (require 'etm-core-variables)
;; (require 'cl-lib)
;; 
;; ;; Connection structure
;; (cl-defstruct etm-remote-connection
;;   method     ; TRAMP method (ssh, scp, sudo, etc.)
;;   user       ; Username
;;   host       ; Hostname or IP
;;   port       ; Port number (optional)
;;   status     ; :connected, :disconnected, :connecting
;;   last-check ; Timestamp of last health check
;;   retries    ; Number of reconnection attempts
;;   properties ; Additional TRAMP properties
;;   tab-id     ; Associated tab ID
;;   )
;; 
;; ;; Global connection storage
;; (defvar etm-remote-connections
;;   (make-hash-table :test 'equal)
;;   "Hash table mapping tab-id to connection alist.
;; Keys are tab IDs, values are alists of (host . connection-struct).")
;; 
;; (defvar etm-remote-global-connections
;;   (make-hash-table :test 'equal)
;;   "Global hash table mapping host to connection struct for cross-tab access.")
;; 
;; ;; Configuration
;; (defcustom etm-remote-connection-timeout 30
;;   "Timeout in seconds for remote connection attempts."
;;   :type 'integer
;;   :group 'etm)
;; 
;; (defcustom etm-remote-max-retries 3
;;   "Maximum number of reconnection attempts."
;;   :type 'integer
;;   :group 'etm)
;; 
;; (defcustom etm-remote-health-check-interval 60
;;   "Interval in seconds between connection health checks."
;;   :type 'integer
;;   :group 'etm)
;; 
;; (defvar etm-remote-connection-change-hook nil
;;   "Hook run when connection status changes.
;; Functions are called with two arguments: HOST and STATUS.")
;; 
;; ;; Utility functions
;; (defun etm-remote-parse-path (path)
;;   "Parse remote PATH and return connection info alist.
;; Returns nil for local paths."
;;   (when (file-remote-p path)
;;     (let* ((dissected (tramp-dissect-file-name path))
;;            (method (tramp-file-name-method dissected))
;;            (user (tramp-file-name-user dissected))
;;            (host (tramp-file-name-host dissected))
;;            (port (tramp-file-name-port dissected))
;;            (localname (tramp-file-name-localname dissected)))
;;       `((method . ,method)
;;         (user . ,user)
;;         (host . ,host)
;;         (port . ,port)
;;         (path . ,localname)))))
;; 
;; (defun etm-remote--get-tab-connections ()
;;   "Get connections hash for current tab."
;;   (let ((tab-name (or (alist-get 'name (tab-bar--current-tab)) "default")))
;;     (or (gethash tab-name etm-remote-connections)
;;         (puthash tab-name (make-hash-table :test 'equal) etm-remote-connections))))
;; 
;; ;; Connection management
;; (defun etm-remote-connect (method user host &optional port)
;;   "Create or retrieve connection for METHOD USER@HOST:PORT."
;;   (let* ((tab-connections (etm-remote--get-tab-connections))
;;          (existing (gethash host tab-connections)))
;;     (if (and existing
;;              (equal (etm-remote-connection-method existing) method)
;;              (equal (etm-remote-connection-user existing) user)
;;              (equal (etm-remote-connection-port existing) (or port nil)))
;;         existing
;;       ;; Create new connection
;;       (let ((conn (make-etm-remote-connection
;;                    :method method
;;                    :user user
;;                    :host host
;;                    :port port
;;                    :status :connecting
;;                    :last-check nil
;;                    :retries 0
;;                    :properties nil
;;                    :tab-id (or (alist-get 'name (tab-bar--current-tab)) "default"))))
;;         (puthash host conn tab-connections)
;;         (puthash host conn etm-remote-global-connections)
;;         conn))))
;; 
;; (defun etm-remote-get-connection (host)
;;   "Get connection struct for HOST in current tab."
;;   (let ((tab-connections (etm-remote--get-tab-connections)))
;;     (gethash host tab-connections)))
;; 
;; (defun etm-remote-check-connection (host)
;;   "Check health of connection to HOST.
;; Returns t if connected, nil otherwise."
;;   (let ((conn (etm-remote-get-connection host)))
;;     (when conn
;;       (let* ((method (etm-remote-connection-method conn))
;;              (user (etm-remote-connection-user conn))
;;              (vec (tramp-make-tramp-file-name
;;                    :method method
;;                    :user user
;;                    :host host))
;;              (now (float-time)))
;;         ;; Check if connection is alive
;;         (condition-case nil
;;             (progn
;;               ;; Try to get connection property
;;               (tramp-get-connection-property vec "last-ping" nil)
;;               ;; Update status
;;               (setf (etm-remote-connection-status conn) :connected)
;;               (setf (etm-remote-connection-last-check conn) now)
;;               (setf (etm-remote-connection-retries conn) 0)
;;               (run-hook-with-args 'etm-remote-connection-change-hook host :connected)
;;               t)
;;           (error
;;            ;; Connection failed
;;            (setf (etm-remote-connection-status conn) :disconnected)
;;            (setf (etm-remote-connection-last-check conn) now)
;;            (run-hook-with-args 'etm-remote-connection-change-hook host :disconnected)
;;            nil))))))
;; 
;; (defun etm-remote-disconnect (host)
;;   "Disconnect from HOST and cleanup connection."
;;   (let ((conn (etm-remote-get-connection host))
;;         (tab-connections (etm-remote--get-tab-connections)))
;;     (when conn
;;       ;; Cleanup TRAMP connection
;;       (let* ((method (etm-remote-connection-method conn))
;;              (user (etm-remote-connection-user conn))
;;              (vec (tramp-make-tramp-file-name
;;                    :method method
;;                    :user user
;;                    :host host)))
;;         (ignore-errors
;;           (tramp-cleanup-connection vec)))
;;       ;; Remove from storage
;;       (remhash host tab-connections)
;;       (remhash host etm-remote-global-connections))))
;; 
;; (defun etm-remote-cleanup-all ()
;;   "Cleanup all connections for current tab."
;;   (let ((tab-connections (etm-remote--get-tab-connections)))
;;     (maphash
;;      (lambda (host _conn)
;;        (etm-remote-disconnect host))
;;      tab-connections)))
;; 
;; ;; Buffer detection
;; (defun etm-remote-buffer-p (buffer)
;;   "Return t if BUFFER is visiting a remote file."
;;   (with-current-buffer buffer
;;     (and buffer-file-name
;;          (file-remote-p buffer-file-name))))
;; 
;; (defun etm-remote-buffer-host (buffer)
;;   "Return hostname of remote BUFFER, or nil if local."
;;   (with-current-buffer buffer
;;     (when (and buffer-file-name
;;                (file-remote-p buffer-file-name))
;;       (let ((info (etm-remote-parse-path buffer-file-name)))
;;         (when info
;;           (alist-get 'host info))))))
;; 
;; ;; Auto-reconnection
;; (defun etm-remote-reconnect (host)
;;   "Attempt to reconnect to HOST."
;;   (let ((conn (etm-remote-get-connection host)))
;;     (when (and conn
;;                (< (etm-remote-connection-retries conn) etm-remote-max-retries))
;;       (setf (etm-remote-connection-status conn) :connecting)
;;       (cl-incf (etm-remote-connection-retries conn))
;;       (run-hook-with-args 'etm-remote-connection-change-hook host :connecting)
;;       ;; Force TRAMP to reconnect
;;       (let* ((method (etm-remote-connection-method conn))
;;              (user (etm-remote-connection-user conn))
;;              (vec (tramp-make-tramp-file-name
;;                    :method method
;;                    :user user
;;                    :host host)))
;;         (ignore-errors
;;           (tramp-cleanup-connection vec))
;;         ;; Check if reconnection succeeded
;;         (etm-remote-check-connection host)))))
;; 
;; ;; Health monitoring timer
;; (defvar etm-remote--health-check-timer nil
;;   "Timer for periodic connection health checks.")
;; 
;; (defun etm-remote--health-check-all ()
;;   "Check health of all connections."
;;   (maphash
;;    (lambda (_tab-id tab-connections)
;;      (maphash
;;       (lambda (host conn)
;;         (when (and conn
;;                    (eq (etm-remote-connection-status conn) :connected))
;;           (let ((last-check (etm-remote-connection-last-check conn))
;;                 (now (float-time)))
;;             (when (or (null last-check)
;;                       (> (- now last-check) etm-remote-health-check-interval))
;;               (unless (etm-remote-check-connection host)
;;                 ;; Try to reconnect
;;                 (etm-remote-reconnect host))))))
;;       tab-connections))
;;    etm-remote-connections))
;; 
;; (defun etm-remote-start-health-monitoring ()
;;   "Start periodic health monitoring of connections."
;;   (etm-remote-stop-health-monitoring)
;;   (setq etm-remote--health-check-timer
;;         (run-with-timer etm-remote-health-check-interval
;;                         etm-remote-health-check-interval
;;                         #'etm-remote--health-check-all)))
;; 
;; (defun etm-remote-stop-health-monitoring ()
;;   "Stop health monitoring timer."
;;   (when etm-remote--health-check-timer
;;     (cancel-timer etm-remote--health-check-timer)
;;     (setq etm-remote--health-check-timer nil)))
;; 
;; (provide 'etm-remote-connection)
;; 
;; ;;; etm-remote-connection.el ends here
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-remote/etm-remote-connection.el
;; --------------------------------------------------------------------------------

;;; test-etm-remote-connection.el ends here
