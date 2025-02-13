;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 00:04:05>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-layout/etm-layout.el

(require 'etm-layout-core)
(require 'etm-layout-save)
(require 'etm-layout-window)
(require 'etm-layout-load)

(provide 'etm-layout)

(when
    (not load-file-name)
  (message "etm-layout.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))