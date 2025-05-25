;;; etm-remote-connection.el --- ETM remote connection management -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Time-stamp: <2025-05-25 15:54:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-remote/etm-remote-connection.el

;;; Commentary:
;; This module provides enhanced remote connection management for ETM,
;; supporting multiple TRAMP methods with health monitoring and automatic
;; reconnection capabilities.

;;; Code:

(require 'tramp)
(require 'etm-core-variables)
(require 'cl-lib)

;; Connection structure
(cl-defstruct etm-remote-connection
  method     ; TRAMP method (ssh, scp, sudo, etc.)
  user       ; Username
  host       ; Hostname or IP
  port       ; Port number (optional)
  status     ; :connected, :disconnected, :connecting
  last-check ; Timestamp of last health check
  retries    ; Number of reconnection attempts
  properties ; Additional TRAMP properties
  tab-id     ; Associated tab ID
  )

;; Global connection storage
(defvar etm-remote-connections
  (make-hash-table :test 'equal)
  "Hash table mapping tab-id to connection alist.
Keys are tab IDs, values are alists of (host . connection-struct).")

(defvar etm-remote-global-connections
  (make-hash-table :test 'equal)
  "Global hash table mapping host to connection struct for cross-tab access.")

;; Configuration
(defcustom etm-remote-connection-timeout 30
  "Timeout in seconds for remote connection attempts."
  :type 'integer
  :group 'etm)

(defcustom etm-remote-max-retries 3
  "Maximum number of reconnection attempts."
  :type 'integer
  :group 'etm)

(defcustom etm-remote-health-check-interval 60
  "Interval in seconds between connection health checks."
  :type 'integer
  :group 'etm)

(defvar etm-remote-connection-change-hook nil
  "Hook run when connection status changes.
Functions are called with two arguments: HOST and STATUS.")

;; Utility functions
(defun etm-remote-parse-path (path)
  "Parse remote PATH and return connection info alist.
Returns nil for local paths."
  (when (file-remote-p path)
    (let* ((dissected (tramp-dissect-file-name path))
           (method (tramp-file-name-method dissected))
           (user (tramp-file-name-user dissected))
           (host (tramp-file-name-host dissected))
           (port (tramp-file-name-port dissected))
           (localname (tramp-file-name-localname dissected)))
      `((method . ,method)
        (user . ,user)
        (host . ,host)
        (port . ,port)
        (path . ,localname)))))

(defun etm-remote--get-tab-connections ()
  "Get connections hash for current tab."
  (let ((tab-name (or (alist-get 'name (tab-bar--current-tab)) "default")))
    (or (gethash tab-name etm-remote-connections)
        (puthash tab-name (make-hash-table :test 'equal) etm-remote-connections))))

;; Connection management
(defun etm-remote-connect (method user host &optional port)
  "Create or retrieve connection for METHOD USER@HOST:PORT."
  (let* ((tab-connections (etm-remote--get-tab-connections))
         (existing (gethash host tab-connections)))
    (if (and existing
             (equal (etm-remote-connection-method existing) method)
             (equal (etm-remote-connection-user existing) user)
             (equal (etm-remote-connection-port existing) (or port nil)))
        existing
      ;; Create new connection
      (let ((conn (make-etm-remote-connection
                   :method method
                   :user user
                   :host host
                   :port port
                   :status :connecting
                   :last-check nil
                   :retries 0
                   :properties nil
                   :tab-id (or (alist-get 'name (tab-bar--current-tab)) "default"))))
        (puthash host conn tab-connections)
        (puthash host conn etm-remote-global-connections)
        conn))))

(defun etm-remote-get-connection (host)
  "Get connection struct for HOST in current tab."
  (let ((tab-connections (etm-remote--get-tab-connections)))
    (gethash host tab-connections)))

(defun etm-remote-check-connection (host)
  "Check health of connection to HOST.
Returns t if connected, nil otherwise."
  (let ((conn (etm-remote-get-connection host)))
    (when conn
      (let* ((method (etm-remote-connection-method conn))
             (user (etm-remote-connection-user conn))
             (vec (tramp-make-tramp-file-name
                   :method method
                   :user user
                   :host host))
             (now (float-time)))
        ;; Check if connection is alive
        (condition-case nil
            (progn
              ;; Try to get connection property
              (tramp-get-connection-property vec "last-ping" nil)
              ;; Update status
              (setf (etm-remote-connection-status conn) :connected)
              (setf (etm-remote-connection-last-check conn) now)
              (setf (etm-remote-connection-retries conn) 0)
              (run-hook-with-args 'etm-remote-connection-change-hook host :connected)
              t)
          (error
           ;; Connection failed
           (setf (etm-remote-connection-status conn) :disconnected)
           (setf (etm-remote-connection-last-check conn) now)
           (run-hook-with-args 'etm-remote-connection-change-hook host :disconnected)
           nil))))))

(defun etm-remote-disconnect (host)
  "Disconnect from HOST and cleanup connection."
  (let ((conn (etm-remote-get-connection host))
        (tab-connections (etm-remote--get-tab-connections)))
    (when conn
      ;; Cleanup TRAMP connection
      (let* ((method (etm-remote-connection-method conn))
             (user (etm-remote-connection-user conn))
             (vec (tramp-make-tramp-file-name
                   :method method
                   :user user
                   :host host)))
        (ignore-errors
          (tramp-cleanup-connection vec)))
      ;; Remove from storage
      (remhash host tab-connections)
      (remhash host etm-remote-global-connections))))

(defun etm-remote-cleanup-all ()
  "Cleanup all connections for current tab."
  (let ((tab-connections (etm-remote--get-tab-connections)))
    (maphash
     (lambda (host _conn)
       (etm-remote-disconnect host))
     tab-connections)))

;; Buffer detection
(defun etm-remote-buffer-p (buffer)
  "Return t if BUFFER is visiting a remote file."
  (with-current-buffer buffer
    (and buffer-file-name
         (file-remote-p buffer-file-name))))

(defun etm-remote-buffer-host (buffer)
  "Return hostname of remote BUFFER, or nil if local."
  (with-current-buffer buffer
    (when (and buffer-file-name
               (file-remote-p buffer-file-name))
      (let ((info (etm-remote-parse-path buffer-file-name)))
        (when info
          (alist-get 'host info))))))

;; Auto-reconnection
(defun etm-remote-reconnect (host)
  "Attempt to reconnect to HOST."
  (let ((conn (etm-remote-get-connection host)))
    (when (and conn
               (< (etm-remote-connection-retries conn) etm-remote-max-retries))
      (setf (etm-remote-connection-status conn) :connecting)
      (cl-incf (etm-remote-connection-retries conn))
      (run-hook-with-args 'etm-remote-connection-change-hook host :connecting)
      ;; Force TRAMP to reconnect
      (let* ((method (etm-remote-connection-method conn))
             (user (etm-remote-connection-user conn))
             (vec (tramp-make-tramp-file-name
                   :method method
                   :user user
                   :host host)))
        (ignore-errors
          (tramp-cleanup-connection vec))
        ;; Check if reconnection succeeded
        (etm-remote-check-connection host)))))

;; Health monitoring timer
(defvar etm-remote--health-check-timer nil
  "Timer for periodic connection health checks.")

(defun etm-remote--health-check-all ()
  "Check health of all connections."
  (maphash
   (lambda (_tab-id tab-connections)
     (maphash
      (lambda (host conn)
        (when (and conn
                   (eq (etm-remote-connection-status conn) :connected))
          (let ((last-check (etm-remote-connection-last-check conn))
                (now (float-time)))
            (when (or (null last-check)
                      (> (- now last-check) etm-remote-health-check-interval))
              (unless (etm-remote-check-connection host)
                ;; Try to reconnect
                (etm-remote-reconnect host))))))
      tab-connections))
   etm-remote-connections))

(defun etm-remote-start-health-monitoring ()
  "Start periodic health monitoring of connections."
  (etm-remote-stop-health-monitoring)
  (setq etm-remote--health-check-timer
        (run-with-timer etm-remote-health-check-interval
                        etm-remote-health-check-interval
                        #'etm-remote--health-check-all)))

(defun etm-remote-stop-health-monitoring ()
  "Stop health monitoring timer."
  (when etm-remote--health-check-timer
    (cancel-timer etm-remote--health-check-timer)
    (setq etm-remote--health-check-timer nil)))

(provide 'etm-remote-connection)

;;; etm-remote-connection.el ends here