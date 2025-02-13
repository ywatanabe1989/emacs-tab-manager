;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-23 07:19:07
;;; Timestamp: <2025-01-23 07:19:07>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/inits/03-visu-070-tab-01-open.el

(defun my/tab-new (arg)
  (interactive
   (list
    (read-string "Enter string: ")))
  (tab-new)
  (tab-rename (message "%s" arg)))

(when (not load-file-name)
  (message "%s loaded." (file-name-nondirectory (or load-file-name buffer-file-name))))