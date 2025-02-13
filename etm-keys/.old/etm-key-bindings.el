;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 13:31:37>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-keys/.old/etm-key-bindings.el

(require 'etm-variables)
(require 'etm-navigation)

(define-prefix-command 'etm-command-map)

(global-set-key
 (kbd "M-t")
 'etm-command-map)

(define-key
 etm-command-map
 (kbd "0")
 'etm-close)

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
 (kbd "m")
 'etm-navigation-move)

(define-key
 etm-command-map
 (kbd "r")
 'etm-rename)

;; Setters
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
(define-key etm-command-map
            (kbd "h")
            #'etm-buffer-jump-to-home)
(define-key etm-command-map
            (kbd "s")
            #'etm-buffer-jump-to-semi-home)
(define-key etm-command-map
            (kbd "r")
            #'etm-buffer-jump-to-results)

(define-key
 etm-command-map
 (kbd "k")
 'etm-buffer-kill-or-bury)

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

;; (global-set-key
;;  (kbd "M-h")
;;  (lambda
;;    ()
;;    (interactive)
;;    (etm-buffer-get "home")))

;; (global-set-key
;;  (kbd "M-H")
;;  (lambda
;;    ()
;;    (interactive)
;;    (etm-registry-set-buffer "home")))

;; (global-set-key
;;  (kbd "M-s")
;;  (lambda
;;    ()
;;    (interactive)
;;    (etm-buffer-get "semi-home")))

;; (global-set-key
;;  (kbd "M-S")
;;  (lambda
;;    ()
;;    (interactive)
;;    (etm-registry-set-buffer "semi-home")))

;; (global-set-key
;;  (kbd "M-r")
;;  (lambda
;;    ()
;;    (interactive)
;;    (etm-buffer-get "results")))

;; (global-set-key
;;  (kbd "M-R")
;;  (lambda
;;    ()
;;    (interactive)
;;    (etm-registry-set-buffer "results")))

(provide 'etm-key-bindings)

(when
    (not load-file-name)
  (message "etm-key-bindings.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))