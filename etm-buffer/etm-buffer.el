;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 14:23:21>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-buffer/etm-buffer.el

(require 'etm-buffer-setters)
(require 'etm-buffer-getters)
(require 'etm-buffer-kill-or-bury)
(require 'etm-buffer-jumpers)
(require 'etm-buffer-checkers)

(provide 'etm-buffer)

(when
    (not load-file-name)
  (message "etm-buffer.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))