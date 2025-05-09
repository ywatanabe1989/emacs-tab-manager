;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 08:50:15>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-buffer/etm-buffer.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;; Buffer management module for ETM (Emacs Tab Manager)
;; This file aggregates all buffer-related functionality

(require 'etm-buffer-setters)
(require 'etm-buffer-getters)
(require 'etm-buffer-kill-or-bury)
(require 'etm-buffer-jumpers)
(require 'etm-buffer-checkers)
(require 'etm-buffer-navigation)

(provide 'etm-buffer)

(when (not load-file-name)
  (message "etm-buffer.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))