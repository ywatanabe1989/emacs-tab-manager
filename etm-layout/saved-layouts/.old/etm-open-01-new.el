;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 18:47:24>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/inits/--remove-this-prefix-03-visu-070-tab-01-new.el

(defun my/tab-new
    (arg)
  (interactive
   (list
    (read-string "Enter string: ")))
  (tab-new)
  (tab-rename
   (message "%s" arg)))

(when
    (not load-file-name)
  (message "%s loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

(provide '--remove-this-prefix-03-visu-070-tab-01-new)

(when
    (not load-file-name)
  (message "--remove-this-prefix-03-visu-070-tab-01-new.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))