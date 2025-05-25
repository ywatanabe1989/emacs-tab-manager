;;; etm-remote-indicators.el --- Visual indicators for ETM remote connections -*- coding: utf-8; lexical-binding: t -*-

;; Author: Yuki Watanabe
;; Date: 2025-01-13
;; Version: 1.0.0

;;; Commentary:
;; This module provides visual indicators for remote connections in ETM.
;; It enhances tab names, prefixes buffer names, and provides mode line
;; indicators to show remote connection status.

;;; Code:

(require 'cl-lib)
(require 'etm-remote-connection)

(defvar etm-remote-show-host-in-tab t
  "Whether to show remote host information in tab names.")

(defvar etm-remote-show-host-in-buffer t
  "Whether to prefix buffer names with remote host information.")

(defvar etm-remote-show-mode-line t
  "Whether to show remote connection status in mode line.")

(defvar etm-remote-status-colors
  '((:connected . "green")
    (:connecting . "yellow")
    (:error . "red")
    (:disconnected . "gray"))
  "Color mapping for connection status.")

(defun etm-remote-get-status-color (status)
  "Get color for connection STATUS."
  (or (cdr (assq status etm-remote-status-colors)) "default"))

(defun etm-remote-enhance-tab-name ()
  "Enhance current tab name with remote host information."
  (when etm-remote-show-host-in-tab
    (let ((connections (etm-remote--get-tab-connections))
          (active-hosts '()))
      ;; Collect active hosts
      (maphash (lambda (host conn)
                 (when (eq (etm-remote-connection-status conn) :connected)
                   (push host active-hosts)))
               connections)
      ;; Update tab name if there are active connections
      (when active-hosts
        (let* ((current-tab (tab-bar--current-tab))
               (current-name (or (alist-get 'name current-tab) ""))
               (base-name (replace-regexp-in-string 
                           " \\[@[^]]+\\]\\|\\[[0-9]+ hosts\\]" "" current-name))
               (suffix (if (= (length active-hosts) 1)
                           (format " [@%s]" (car active-hosts))
                         (format " [%d hosts]" (length active-hosts)))))
          (tab-bar-rename-tab (concat base-name suffix)))))))

(defun etm-remote-prefix-buffer-name (buffer-name)
  "Prefix BUFFER-NAME with remote host information if applicable."
  (if (and etm-remote-show-host-in-buffer
           (boundp 'default-directory)
           default-directory)
      (let ((remote-id (file-remote-p default-directory)))
        (if remote-id
            (let ((host (file-remote-p default-directory 'host)))
              (if host
                  (format "[%s] %s" host buffer-name)
                buffer-name))
          buffer-name))
    buffer-name))

(defun etm-remote-mode-line-indicator ()
  "Generate mode line indicator for remote connections."
  (if etm-remote-show-mode-line
      (let ((connections (etm-remote--get-tab-connections))
            (indicators '()))
        (maphash (lambda (host conn)
                   (let* ((status (etm-remote-connection-status conn))
                          (color (etm-remote-get-status-color status))
                          (symbol (cond ((eq status :connected) "●")
                                        ((eq status :connecting) "○")
                                        ((eq status :error) "!")
                                        (t "◌"))))
                     (push (format "%s%s" symbol host) indicators)))
                 connections)
        (if indicators
            (format " R:%s" (mapconcat 'identity (nreverse indicators) ","))
          ""))
    ""))

(defvar etm-remote-mode-line-format
  '(:eval (etm-remote-mode-line-indicator))
  "Mode line format for remote indicators.")

(defun etm-remote-indicators-init ()
  "Initialize remote visual indicators."
  (interactive)
  ;; Add to mode line
  (unless (memq 'etm-remote-mode-line-indicator mode-line-misc-info)
    (push etm-remote-mode-line-format mode-line-misc-info))
  
  ;; Set up hooks for tab name enhancement
  (add-hook 'etm-remote-connection-change-hook 'etm-remote-enhance-tab-name)
  
  ;; Set up advice for buffer naming
  (advice-add 'rename-buffer :filter-args 'etm-remote--rename-buffer-advice)
  (advice-add 'set-visited-file-name :after 'etm-remote--update-buffer-name)
  
  (message "ETM remote indicators initialized"))

(defun etm-remote-indicators-cleanup ()
  "Clean up remote visual indicators."
  (interactive)
  ;; Remove from mode line
  (setq mode-line-misc-info 
        (delq etm-remote-mode-line-format mode-line-misc-info))
  
  ;; Remove hooks
  (remove-hook 'etm-remote-connection-change-hook 'etm-remote-enhance-tab-name)
  
  ;; Remove advice
  (advice-remove 'rename-buffer 'etm-remote--rename-buffer-advice)
  (advice-remove 'set-visited-file-name 'etm-remote--update-buffer-name)
  
  (message "ETM remote indicators cleaned up"))

(defun etm-remote--rename-buffer-advice (args)
  "Advice for `rename-buffer' to add remote prefix.
ARGS are the original arguments to rename-buffer."
  (let ((newname (car args)))
    (cons (etm-remote-prefix-buffer-name newname) (cdr args))))

(defun etm-remote--update-buffer-name (&rest _)
  "Update current buffer name with remote prefix if needed."
  (when (and buffer-file-name
             (file-remote-p buffer-file-name))
    (let ((base-name (file-name-nondirectory buffer-file-name)))
      (rename-buffer (etm-remote-prefix-buffer-name base-name) t))))

(provide 'etm-remote-indicators)
;;; etm-remote-indicators.el ends here