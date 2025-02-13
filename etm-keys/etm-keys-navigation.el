;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 00:12:42>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-keys/etm-keys-navigation.el

(require 'etm-navigation)
(require 'etm-keys-command-map)

(global-set-key
 (kbd "M-1")
 (lambda
   ()
   (interactive)
   (etm-navigation-jump-to 1)))

(global-set-key
 (kbd "M-2")
 (lambda
   ()
   (interactive)
   (etm-navigation-jump-to 2)))

(global-set-key
 (kbd "M-3")
 (lambda
   ()
   (interactive)
   (etm-navigation-jump-to 3)))

(global-set-key
 (kbd "M-4")
 (lambda
   ()
   (interactive)
   (etm-navigation-jump-to 4)))

(global-set-key
 (kbd "M-5")
 (lambda
   ()
   (interactive)
   (etm-navigation-jump-to 5)))

(global-set-key
 (kbd "M-6")
 (lambda
   ()
   (interactive)
   (etm-navigation-jump-to 6)))

(global-set-key
 (kbd "M-7")
 (lambda
   ()
   (interactive)
   (etm-navigation-jump-to 7)))

(global-set-key
 (kbd "M-8")
 (lambda
   ()
   (interactive)
   (etm-navigation-jump-to 8)))

(global-set-key
 (kbd "M-9")
 (lambda
   ()
   (interactive)
   (etm-navigation-jump-to 9)))

(provide 'etm-keys-navigation)

(when
    (not load-file-name)
  (message "etm-keys-navigation.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))