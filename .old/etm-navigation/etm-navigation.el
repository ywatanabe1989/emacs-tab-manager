;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 23:17:17>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-navigation/etm-navigation.el
;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'etm-nav-core)
(require 'etm-nav-jump)
(require 'etm-nav-move)

(provide 'etm-navigation)

(when
    (not load-file-name)
  (message "etm-navigation.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))