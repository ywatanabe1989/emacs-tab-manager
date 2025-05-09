;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-09 21:25:10>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-core/etm-core-ssh-helpers.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;; SSH helper functions for ETM

(require 'etm-core-variables)

(defvar --etm-local-host-names
  '("" "ywata-note-win" "localhost")
  "List of host names considered as local machines.")

(defvar --etm-ssh-hostname-username nil
  "Cache for SSH configuration cache: ((hostname . username) ...).")

(defun --etm-ssh-parse-dot-ssh ()
  "Update the SSH configuration cache from config files."
  (interactive)
  (let* ((command
          "awk '/^Host / {host=$2} /^[[:space:]]*User / {printf \"%s %s\\n\", host, $2}' ~/.ssh/config ~/.ssh/conf.d/*.conf 2>/dev/null")
         (output
          (shell-command-to-string command))
         (pairs
          (mapcar
           (lambda (line)
             (let ((parts (split-string line)))
               (cons (nth 0 parts) (nth 1 parts))))
           (split-string output "\n" t))))
    (setq --etm-ssh-hostname-username
          (cons '("localhost" . "ywatanabe")
                pairs))))

(defun --etm-ssh-select-host ()
  "Select an SSH host from cached config."
  (interactive)
  (--etm-ssh-parse-dot-ssh)
  (completing-read "Choose host: "
                   (mapcar #'car --etm-ssh-hostname-username)))

(defun --etm-ssh-rename-username (path &optional host)
  "Handle path transformations for remote connections.
PATH is the local path to transform.
HOST is the remote hostname."
  (let ((clean-path (replace-regexp-in-string "^/home/[^/]+" 
                                             "" 
                                             path)))
    (concat "/home/ywatanabe" clean-path)))

(defun --etm-vterm-new (name)
  "Create a new vterm buffer with NAME.
Falls back to term if vterm is not available."
  (if (fboundp 'vterm)
      (let ((vterm-buffer-name name))
        (vterm))
    (let ((term-buffer (get-buffer-create (concat "*" name "*"))))
      (with-current-buffer term-buffer
        (term-mode)
        (term-exec term-buffer name "/bin/bash" nil nil))
      (switch-to-buffer term-buffer))))

(provide 'etm-core-ssh-helpers)

(when (not load-file-name)
  (message "etm-core-ssh-helpers.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))