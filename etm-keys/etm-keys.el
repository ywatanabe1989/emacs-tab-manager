;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 23:32:27>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-keys/etm-keys.el

(require 'etm-keys-command-map)
(require 'etm-keys-buffer)
(require 'etm-keys-layout)
(require 'etm-keys-navigation)

(provide 'etm-keys)

(when
    (not load-file-name)
  (message "etm-keys.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))