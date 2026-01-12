;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-01-08 07:29:57>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-email/etm-email-keys.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)


;;; Commentary:
;; Keybindings for ETM email integration

(require 'etm-keys-command-map)
(require 'etm-email-core)

;;; Code:

;; Email keybindings under etm-command-map
;; ----------------------------------------

;; M-t m - jump to email (mnemonic: mail)

(define-key etm-command-map (kbd "m") #'etm-email-jump)

;; M-t M - open email in dedicated tab (mnemonic: Mail tab)

(define-key etm-command-map (kbd "M") #'etm-email-open-tab)


(provide 'etm-email-keys)

(when
    (not load-file-name)
  (message "etm-email-keys.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))