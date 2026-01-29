;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-09 19:40:15>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-core/etm-core-helpers.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Core utility functions for ETM (Emacs Tab Manager)
;; These are general-purpose helper functions used throughout the package

(require 'etm-core-variables)
(require 'etm-core-ssh-helpers)

;; This file provides core utility functions used throughout the ETM package.
;; SSH-related helpers and terminal helpers are now in etm-core-ssh-helpers.el

(defvar etm-debug nil
  "When non-nil, enable debug messages for ETM.")

(defun etm-message (format-string &rest args)
  "Log a debug message if `etm-debug' is non-nil.
FORMAT-STRING and ARGS are passed to `message'."
  (when etm-debug
    (apply #'message (concat "[ETM] " format-string) args)))

(provide 'etm-core-helpers)

(when (not load-file-name)
  (message "etm-core-helpers.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
