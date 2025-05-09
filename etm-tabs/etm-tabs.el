;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 08:45:05>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-tabs/etm-tabs.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;; Main module file for ETM tabs functionality
;; This aggregates all tabs-related modules

(require 'etm-tabs-new-and-rename)

(provide 'etm-tabs)

(when (not load-file-name)
  (message "etm-tabs.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))