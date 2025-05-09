;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-09 19:40:15>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-core/etm-core-helpers.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;; Core utility functions for ETM (Emacs Tab Manager)
;; These are general-purpose helper functions used throughout the package

(require 'etm-core-variables)
(require 'etm-core-ssh-helpers)

(provide 'etm-core-helpers)

(when (not load-file-name)
  (message "etm-core-helpers.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))