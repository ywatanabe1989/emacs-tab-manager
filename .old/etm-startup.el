;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 23:22:46>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-startup.el

(provide 'etm-startup)

(when
    (not load-file-name)
  (message "etm-startup.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))