;;; etm-remote-keys.el --- Keybindings for ETM remote support -*- coding: utf-8; lexical-binding: t -*-

;; Author: Yuki Watanabe
;; Date: 2025-01-13
;; Version: 1.0.0

;;; Commentary:
;; This module defines keybindings for ETM remote functionality.
;; All remote commands are prefixed with C-x t r (for "remote").

;;; Code:

(require 'etm-keys-command-map)
(require 'etm-remote-navigation)
(require 'etm-remote-errors)

(defvar etm-remote-command-map (make-sparse-keymap)
  "Keymap for ETM remote commands.")

;; Define the remote command prefix
(define-key etm-command-map "r" etm-remote-command-map)

;; Navigation commands
(define-key etm-remote-command-map "j" 'etm-remote-jump-to-host)
(define-key etm-remote-command-map "n" 'etm-remote-next-buffer)
(define-key etm-remote-command-map "p" 'etm-remote-prev-buffer)
(define-key etm-remote-command-map "l" 'etm-remote-switch-to-local)
(define-key etm-remote-command-map "t" 'etm-remote-toggle-local-remote)
(define-key etm-remote-command-map "h" 'etm-remote-jump-home)

;; Error monitoring
(define-key etm-remote-command-map "e" 'etm-remote-monitor-errors)

;; Help command
(defun etm-remote-help ()
  "Show help for ETM remote commands."
  (interactive)
  (message (concat "ETM Remote Commands:\n"
                   "C-x t r j - Jump to remote host\n"
                   "C-x t r n - Next remote buffer\n"
                   "C-x t r p - Previous remote buffer\n"
                   "C-x t r l - Switch to local buffer\n"
                   "C-x t r t - Toggle local/remote\n"
                   "C-x t r h - Jump to remote home\n"
                   "C-x t r e - Monitor errors")))

(define-key etm-remote-command-map "?" 'etm-remote-help)

(provide 'etm-remote-keys)
;;; etm-remote-keys.el ends here