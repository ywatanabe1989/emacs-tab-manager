;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 20:01:04>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/config.el

(add-hook 'after-init-hook #'etm-init)

(provide 'config)

(when
    (not load-file-name)
  (message "config.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))