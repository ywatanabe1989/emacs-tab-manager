;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 23:17:41>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-navigation/etm-navigation-core.el
;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'etm-variables)

(provide 'etm-navigation-core)

(when
    (not load-file-name)
  (message "etm-navigation-core.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))