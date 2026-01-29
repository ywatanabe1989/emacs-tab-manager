;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-19 07:26:56>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-close/etm-close-core.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

(require 'etm-core-tab-id)
(require 'etm-core-ssh-connection)
(require 'etm-buffer-auto-track)
(require 'etm-buffer-numeric)

(defun etm-reset
    ()
  "Close all tabs and call `etm-startup'."
  (interactive)
  (etm-close-all)
  (sleep-for 1)
  (etm-startup))

(defun etm-close (&optional kill-buffers)
  "Close current tab and move to next, with proper connection management.
When KILL-BUFFERS is non-nil or `etm-close-kills-tracked-buffers' is t,
also kill all buffers tracked to this tab."
  (interactive)
  (let* ((current-tab (tab-bar--current-tab))
         (tab-name (alist-get 'name current-tab))
         (killed-count 0))
    (etm-message "close: Closing tab '%s'" tab-name)
    (etm-message
     "close: kill-buffers=%s, etm-close-kills-tracked-buffers=%s"
     kill-buffers etm-close-kills-tracked-buffers)
    ;; Kill tracked buffers if enabled
    (if (or kill-buffers etm-close-kills-tracked-buffers)
        (progn
          (etm-message "close: Will kill tracked buffers")
          (setq killed-count (etm-kill-tracked-buffers tab-name)))
      (etm-message "close: NOT killing tracked buffers (disabled)"))
    ;; Clear numeric buffer entries for this tab
    (etm-numeric-clear-tab tab-name)
    ;; Don't terminate the connection, just remove it from the registry
    (--etm-unregister-ssh-connection tab-name)
    (tab-close)
    (tab-next)
    (if (etm-tab-first-tab-p)
        (tab-previous))
    (etm-message "close: Tab '%s' closed, killed %d buffers" tab-name
		 killed-count)
    (when (> killed-count 0)
      (message "Closed tab '%s' and killed %d tracked buffers"
	       tab-name killed-count))))

(defun etm-close-keep-buffers ()
  "Close current tab without killing tracked buffers."
  (interactive)
  (let ((etm-close-kills-tracked-buffers nil))
    (etm-close)))

(defun etm-close-all
    ()
  "Close all tabs with proper connection management."
  (interactive)
  (if
      (fboundp 'tab-bar-mode)
      (progn
        (tab-bar-mode 1)
        ;; Clear SSH connection registry
        (clrhash etm-ssh-connections)
        ;; Close all tabs except current
        (while
            (>
             (length
              (tab-bar-tabs))
             1)
          (tab-bar-close-tab))
        (tab-bar-rename-tab nil)
        (message "All tabs closed except the current one."))
    (message "Tab functionality not available in this Emacs version.")))

(defun etm-close-by-name (tab-name)
  "Close the specified tab if it exists and is not the only tab; otherwise, return nil."
  (interactive "sTab name to close: ")
  (let* ((tabs (tab-bar-tabs))
         (tab-index (cl-position tab-name tabs
                                 :test
                                 (lambda (name tab)
                                   (string= name
                                            (alist-get 'name tab))))))
    (when (and tab-index
               (> (length tabs) 1))
      ;; Clear numeric buffer entries for this tab
      (etm-numeric-clear-tab tab-name)
      ;; Clean up SSH connection registry
      (--etm-unregister-ssh-connection tab-name)
      (tab-bar-close-tab (1+ tab-index))
      t)))

(defun etm-close-with-connection-management ()
  "Close current tab with proper connection management."
  (interactive)
  (let* ((current-tab (tab-bar--current-tab))
         (tab-name (alist-get 'name current-tab)))
    ;; Don't terminate the connection, just remove it from the registry
    (--etm-unregister-ssh-connection tab-name)
    (etm-close)))

(defun etm-cleanup-unused-connections ()
  "Cleanup SSH connections that are no longer needed.
This respects the ControlPersist setting in SSH."
  (interactive)
  (message
   "SSH connections will be automatically closed based on ControlPersist settings"))

(provide 'etm-close-core)

(when
    (not load-file-name)
  (message "etm-close-core.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
