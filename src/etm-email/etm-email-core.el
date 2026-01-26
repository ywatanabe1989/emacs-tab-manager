;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-01-08 07:21:24>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-email/etm-email-core.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Special email/mu4e integration for ETM (Emacs Tab Manager)
;; Provides dedicated email tab management and quick access to mu4e

(require 'etm-core-variables)

;;; Code:

;; Customization
;; ----------------------------------------

(defgroup etm-email nil
  "Email integration for Emacs Tab Manager."
  :prefix "etm-email-"
  :group 'etm)

(defcustom etm-email-tab-name "email"
  "Name for the dedicated email tab."
  :type 'string
  :group 'etm-email)

(defcustom etm-email-buffer-patterns
  '("\\*mu4e-main\\*"
    "\\*mu4e-headers\\*"
    "\\*mu4e-view\\*"
    "\\*mu4e-compose\\*"
    "\\*mu4e-.*\\*")
  "List of regex patterns matching email buffers."
  :type '(repeat string)
  :group 'etm-email)

(defcustom etm-email-preferred-main-buffer "*mu4e-main*"
  "Preferred buffer to switch to when jumping to email."
  :type 'string
  :group 'etm-email)

(defcustom etm-email-project-dir "~/proj/email"
  "Directory for email-related project files."
  :type 'directory
  :group 'etm-email)

;; Helper functions
;; ----------------------------------------

(defun etm-email-buffer-p (buffer)
  "Return non-nil if BUFFER is an email buffer."
  (let ((name (if (bufferp buffer)
                  (buffer-name buffer)
                buffer)))
    (cl-some (lambda (pattern)
               (string-match-p pattern name))
             etm-email-buffer-patterns)))

(defun etm-email-get-buffers ()
  "Return list of all email buffers."
  (cl-remove-if-not #'etm-email-buffer-p (buffer-list)))

(defun etm-email-tab-exists-p ()
  "Return non-nil if the email tab exists."
  (cl-some (lambda (tab)
             (string= (alist-get 'name tab) etm-email-tab-name))
           (tab-bar-tabs)))

(defun etm-email-get-tab-index ()
  "Return index of email tab, or nil if not found."
  (let ((tabs (tab-bar-tabs))
        (index nil))
    (cl-loop for tab in tabs
             for i from 1
             when (string= (alist-get 'name tab) etm-email-tab-name)
             do (setq index i))
    index))

;; Interactive commands
;; ----------------------------------------

;;;###autoload

(defun etm-email-jump ()
  "Jump to email.
If email tab exists, switch to it.
Otherwise, open mu4e in current tab."
  (interactive)
  (let ((email-tab-index (etm-email-get-tab-index)))
    (if email-tab-index
        (progn
          (tab-bar-select-tab email-tab-index)
          (etm-email--ensure-main-buffer))
      ;; No email tab, just open mu4e
      (etm-email--start-mu4e))))

;;;###autoload

(defun etm-email-open-tab ()
  "Open email in a dedicated tab.
If email tab exists, switch to it.
Otherwise, create new tab and open mu4e."
  (interactive)
  (let ((email-tab-index (etm-email-get-tab-index)))
    (if email-tab-index
        (progn
          (tab-bar-select-tab email-tab-index)
          (etm-email--ensure-main-buffer))
      ;; Create new email tab
      (tab-bar-new-tab)
      (tab-bar-rename-tab etm-email-tab-name)
      (etm-email--start-mu4e))))

;;;###autoload

(defalias 'email 'etm-email-open-tab
  "Alias for `etm-email-open-tab' for quick access.")

;;;###autoload

(defun etm-email-close-tab ()
  "Close the email tab if it exists."
  (interactive)
  (let ((email-tab-index (etm-email-get-tab-index)))
    (if email-tab-index
        (progn
          (tab-bar-select-tab email-tab-index)
          (tab-bar-close-tab))
      (message "No email tab to close"))))

;; Internal helpers
;; ----------------------------------------

(defun etm-email--start-mu4e ()
  "Start mu4e if available, handling database lock gracefully."
  (if (fboundp 'mu4e)
      (condition-case err
          (progn
            ;; Quit existing broken session if any
            (when (and (boundp 'mu4e--server-process)
                       mu4e--server-process)
              (ignore-errors (mu4e-quit)))
            ;; Start fresh
            (mu4e)
            ;; Set up 3-pane layout after mu4e initializes
            (run-with-timer 0.5 nil #'etm-email--setup-layout))
        (error
         (let ((err-msg (error-message-string err)))
           (if (string-match-p "locked\\|another process" err-msg)
               (message
		"mu4e: Database locked by another process, please wait...")
             (message "mu4e error: %s" err-msg)))))
    (message
     "mu4e is not available. Please install and configure mu4e.")))

(defun etm-email--ensure-main-buffer ()
  "Switch to mu4e main buffer if it exists."
  (let ((main-buf (get-buffer etm-email-preferred-main-buffer)))
    (when main-buf
      (switch-to-buffer main-buf))))

(defun etm-email--setup-layout ()
  "Set up 3-pane email layout: dired | mu4e | vterm+cld."
  (delete-other-windows)
  (let ((email-dir (expand-file-name etm-email-project-dir))
        (mu4e-buf (get-buffer etm-email-preferred-main-buffer))
        (vterm-buf-name "*email-cld*"))
    ;; Start in left window: dired ~/proj/email
    (dired email-dir)
    ;; Split for center
    (split-window-right)
    (other-window 1)
    ;; Center: mu4e
    (if mu4e-buf
        (switch-to-buffer mu4e-buf)
      (switch-to-buffer "*scratch*"))
    ;; Split for right
    (split-window-right)
    (other-window 1)
    ;; Right: vterm with cld command
    (let ((default-directory email-dir)
          (right-window (selected-window)))
      (if (require 'vterm nil t)
          (progn
            ;; Kill old buffer if exists
            (when (get-buffer vterm-buf-name)
              (kill-buffer vterm-buf-name))
            ;; Create new vterm
            (vterm vterm-buf-name)
            ;; Send cld command after vterm is ready
            (let ((buf (current-buffer)))
              (run-with-timer
               0.5 nil
               (lambda ()
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (vterm-send-string "cld")
                     (vterm-send-return)))))))
        ;; Fallback to eshell
        (eshell)))
    ;; Balance windows
    (balance-windows)
    ;; Return focus to center (mu4e)
    (other-window -1)))

(provide 'etm-email-core)

(when (not load-file-name)
  (message "%s loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
