;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-01-08 07:21:15>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-email/etm-email.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)


;; Load email components
(require 'etm-email-core)       ;; Basic variables
(require 'etm-email-keys) ;; User-customizable options


(provide 'etm-email)

(when
    (not load-file-name)
  (message "etm-email.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))