;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 14:44:53>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-keys/etm-keys-command-map.el

(define-prefix-command 'etm-command-map)

(global-set-key
 (kbd "M-t")
 'etm-command-map)

(provide 'etm-keys-command-map)

(when
    (not load-file-name)
  (message "etm-keys-command-map.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))