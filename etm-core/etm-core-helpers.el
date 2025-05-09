;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-09 19:40:15>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-core/etm-core-helpers.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;; Core utility functions for ETM (Emacs Tab Manager)
;; These are general-purpose helper functions used throughout the package

(require 'etm-core-variables)

;; 1. SSH-related helpers
;; ----------------------------------------

(defun --etm-ssh-select-host ()
  "Select a host from predefined list for remote connections."
  (let ((default-host (gethash (tab-bar-tab-name-current) 
                              etm-layout-default-hosts)))
    (if default-host
        default-host
      (or (completing-read "Select host: " 
                          '("localhost" "ywata-note-win"))
          "localhost"))))

(defun --etm-ssh-rename-username (path &optional _host)
  "Handle path transformations for remote connections.
PATH is the local path to transform.
_HOST is the remote hostname (unused in this implementation)."
  (let ((clean-path (replace-regexp-in-string "^/home/[^/]+" 
                                             "" 
                                             path)))
    (concat "/home/ywatanabe" clean-path)))

;; 2. Terminal helpers
;; ----------------------------------------

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

(provide 'etm-core-helpers)

(when (not load-file-name)
  (message "etm-core-helpers.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))