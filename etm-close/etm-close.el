;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 23:12:02>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-close.el

(require 'etm-close-core)
(require 'etm-close-utils)

(provide 'etm-close)

(when
    (not load-file-name)
  (message "etm-close.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))