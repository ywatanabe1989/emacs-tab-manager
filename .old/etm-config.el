;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 23:09:12>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-config.el

(add-hook 'after-init-hook #'etm-init)

(provide 'etm-config)

(when
    (not load-file-name)
  (message "etm-config.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))