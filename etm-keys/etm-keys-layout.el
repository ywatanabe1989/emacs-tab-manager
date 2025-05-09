;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 00:13:07>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-keys/etm-keys-layout.el

(require 'etm-keys-command-map)
(require 'etm-close-utils)
(require 'etm-tabs-new-and-rename)

(define-key
 etm-command-map
 (kbd "1")
 'etm-close-others)

(define-key
 etm-command-map
 (kbd "2")
 'etm-new)

(define-key
 etm-command-map
 (kbd "n")
 'etm-new)

(define-key
 etm-command-map
 (kbd "r")
 'etm-rename)

(global-set-key
 (kbd "M-w")
 (lambda
   ()
   (interactive)
   (etm-close)))

(provide 'etm-keys-layout)

(when
    (not load-file-name)
  (message "etm-keys-layout.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))