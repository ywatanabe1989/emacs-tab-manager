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
          (mu4e)
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

(provide 'etm-email-core)

(when
    (not load-file-name)
  (message "etm-email-core.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
