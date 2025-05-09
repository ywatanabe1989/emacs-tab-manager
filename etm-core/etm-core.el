;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 08:49:30>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-core/etm-core.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;; Core module for ETM (Emacs Tab Manager)
;; This file aggregates all core functionality

(require 'etm-core-variables)
(require 'etm-core-variables-custom)
(require 'etm-core-helpers)
(require 'etm-core-tab-id)

(provide 'etm-core)

(when (not load-file-name)
  (message "etm-core.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))