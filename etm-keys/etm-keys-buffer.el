;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 16:42:11>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-keys/etm-keys-buffer.el

(require 'etm-navigation)
(require 'etm-buffer)
(require 'etm-keys-command-map)

;; Setters
;; ----------------------------------------

(define-key etm-command-map
            (kbd "H")
            #'etm-buffer-set-home)

(define-key etm-command-map
            (kbd "S")
            #'etm-buffer-set-semi-home)

(define-key etm-command-map
            (kbd "R")
            #'etm-buffer-set-results)

;; Jumpers
;; ----------------------------------------

(define-key etm-command-map
            (kbd "h")
            #'etm-navigation-jump-by-buffer-type-home)

(define-key etm-command-map
            (kbd "s")
            #'etm-navigation-jump-by-buffer-type-semi-home)

(define-key etm-command-map
            (kbd "r")
            #'etm-navigation-jump-by-buffer-type-results)

;; Killer
;; ----------------------------------------

(define-key etm-command-map
            (kbd "k")
            'etm-buffer-kill-or-bury)

(provide 'etm-keys-buffer)

(when
    (not load-file-name)
  (message "etm-keys-buffer.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))