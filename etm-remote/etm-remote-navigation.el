;;; etm-remote-navigation.el --- Remote-aware navigation for ETM -*- coding: utf-8; lexical-binding: t -*-

;; Author: Yuki Watanabe
;; Date: 2025-01-13
;; Version: 1.0.0

;;; Commentary:
;; This module provides remote-aware navigation commands for ETM.
;; It allows jumping between remote buffers on the same host,
;; selecting hosts interactively, and switching between local/remote contexts.

;;; Code:

(require 'cl-lib)
(require 'etm-remote-connection)
(require 'etm-buffer)

(defvar etm-remote-navigation-history nil
  "History of remote hosts for navigation.")

(defun etm-remote-get-buffer-host (buffer)
  "Get the remote host for BUFFER, or nil if local."
  (with-current-buffer buffer
    (when (and buffer-file-name
               (file-remote-p buffer-file-name))
      (file-remote-p buffer-file-name 'host))))

(defun etm-remote-list-buffers (&optional host)
  "List all buffers visiting remote files on HOST.
If HOST is nil, list all remote buffers."
  (let ((remote-buffers '()))
    (dolist (buffer (buffer-list))
      (let ((buffer-host (etm-remote-get-buffer-host buffer)))
        (when (and buffer-host
                   (or (null host)
                       (string= host buffer-host)))
          (push buffer remote-buffers))))
    (nreverse remote-buffers)))

(defun etm-remote-jump-to-host (host)
  "Jump to a buffer on remote HOST.
If multiple buffers exist on HOST, prompt for selection."
  (interactive
   (list (completing-read "Remote host: "
                          (delete-dups
                           (delq nil
                                 (mapcar #'etm-remote-get-buffer-host
                                         (buffer-list))))
                          nil t nil 'etm-remote-navigation-history)))
  (let ((host-buffers (etm-remote-list-buffers host)))
    (cond
     ((null host-buffers)
      (message "No buffers found on host: %s" host))
     ((= (length host-buffers) 1)
      (switch-to-buffer (car host-buffers)))
     (t
      (let ((buffer (completing-read
                     (format "Buffer on %s: " host)
                     (mapcar (lambda (buf)
                               (cons (buffer-name buf) buf))
                             host-buffers)
                     nil t)))
        (switch-to-buffer (cdr (assoc buffer
                                      (mapcar (lambda (buf)
                                                (cons (buffer-name buf) buf))
                                              host-buffers)))))))))

(defun etm-remote-next-buffer ()
  "Switch to the next remote buffer on the same host."
  (interactive)
  (let* ((current-host (etm-remote-get-buffer-host (current-buffer)))
         (host-buffers (if current-host
                           (etm-remote-list-buffers current-host)
                         (etm-remote-list-buffers))))
    (when host-buffers
      (let* ((current-pos (cl-position (current-buffer) host-buffers))
             (next-pos (if current-pos
                           (mod (1+ current-pos) (length host-buffers))
                         0)))
        (switch-to-buffer (nth next-pos host-buffers))))))

(defun etm-remote-prev-buffer ()
  "Switch to the previous remote buffer on the same host."
  (interactive)
  (let* ((current-host (etm-remote-get-buffer-host (current-buffer)))
         (host-buffers (if current-host
                           (etm-remote-list-buffers current-host)
                         (etm-remote-list-buffers))))
    (when host-buffers
      (let* ((current-pos (cl-position (current-buffer) host-buffers))
             (prev-pos (if current-pos
                           (mod (1- current-pos) (length host-buffers))
                         (1- (length host-buffers)))))
        (switch-to-buffer (nth prev-pos host-buffers))))))

(defun etm-remote-switch-to-local ()
  "Switch from a remote buffer to a local buffer."
  (interactive)
  (let ((local-buffers (cl-remove-if #'etm-remote-get-buffer-host
                                     (buffer-list))))
    (when local-buffers
      (if (= (length local-buffers) 1)
          (switch-to-buffer (car local-buffers))
        (let ((buffer (completing-read
                       "Local buffer: "
                       (mapcar (lambda (buf)
                                 (cons (buffer-name buf) buf))
                               local-buffers)
                       nil t)))
          (switch-to-buffer (cdr (assoc buffer
                                        (mapcar (lambda (buf)
                                                  (cons (buffer-name buf) buf))
                                                local-buffers)))))))))

(defun etm-remote-toggle-local-remote ()
  "Toggle between local and remote buffers."
  (interactive)
  (if (etm-remote-get-buffer-host (current-buffer))
      (etm-remote-switch-to-local)
    (let ((remote-buffers (etm-remote-list-buffers)))
      (when remote-buffers
        (switch-to-buffer (car remote-buffers))))))

;; Integration with ETM buffer system
(defun etm-remote-jump-home ()
  "Jump to home buffer, preferring remote if on remote host."
  (interactive)
  (let* ((current-host (etm-remote-get-buffer-host (current-buffer)))
         (home-buffers (etm-buffer-get-registered-buffers 'home)))
    (if current-host
        ;; Try to find a remote home buffer on the same host
        (let ((remote-homes (cl-remove-if-not
                             (lambda (buf)
                               (string= current-host
                                        (etm-remote-get-buffer-host buf)))
                             home-buffers)))
          (if remote-homes
              (switch-to-buffer (car remote-homes))
            ;; Fall back to regular home jump
            (etm-buffer-jump-home)))
      ;; Local context, use regular jump
      (etm-buffer-jump-home))))

(provide 'etm-remote-navigation)
;;; etm-remote-navigation.el ends here