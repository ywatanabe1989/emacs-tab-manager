;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-25 07:21:32>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-keys/etm-keys-navigation.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)


(require 'etm-buffer-navigation)
(require 'etm-keys-command-map)
(require 'tab-bar)

;; Tab bar mouse scroll navigation
;; Scroll on tab bar to switch tabs

(global-set-key [wheel-up] #'tab-previous)

(global-set-key [wheel-down] #'tab-next)

(global-set-key [wheel-left] #'tab-previous)

(global-set-key [wheel-right] #'tab-next)

(global-set-key [mouse-4] #'tab-previous)

(global-set-key [mouse-5] #'tab-next)

;; Buffer navigation keys

(global-set-key (kbd "M-1") #'etm-navigation-jump-to-1)

(global-set-key (kbd "M-2") #'etm-navigation-jump-to-2)

(global-set-key (kbd "M-3") #'etm-navigation-jump-to-3)

(global-set-key (kbd "M-4") #'etm-navigation-jump-to-4)

(global-set-key (kbd "M-5") #'etm-navigation-jump-to-5)

(global-set-key (kbd "M-6") #'etm-navigation-jump-to-6)

(global-set-key (kbd "M-7") #'etm-navigation-jump-to-7)

(global-set-key (kbd "M-8") #'etm-navigation-jump-to-8)

(global-set-key (kbd "M-9") #'etm-navigation-jump-to-9)

;; (global-set-key
;;  (kbd "M-1")
;;  (lambda
;;    ()
;;    (interactive)
;;    (etm-navigation-jump-by-index 1)))

;; (global-set-key
;;  (kbd "M-2")
;;  (lambda
;;    ()
;;    (interactive)
;;    (etm-navigation-jump-by-index 2)))

;; (global-set-key
;;  (kbd "M-3")
;;  (lambda
;;    ()
;;    (interactive)
;;    (etm-navigation-jump-by-index 3)))

;; (global-set-key
;;  (kbd "M-4")
;;  (lambda
;;    ()
;;    (interactive)
;;    (etm-navigation-jump-by-index 4)))

;; (global-set-key
;;  (kbd "M-5")
;;  (lambda
;;    ()
;;    (interactive)
;;    (etm-navigation-jump-by-index 5)))

;; (global-set-key
;;  (kbd "M-6")
;;  (lambda
;;    ()
;;    (interactive)
;;    (etm-navigation-jump-by-index 6)))

;; (global-set-key
;;  (kbd "M-7")
;;  (lambda
;;    ()
;;    (interactive)
;;    (etm-navigation-jump-by-index 7)))

;; (global-set-key
;;  (kbd "M-8")
;;  (lambda
;;    ()
;;    (interactive)
;;    (etm-navigation-jump-by-index 8)))

;; (global-set-key
;;  (kbd "M-9")
;;  (lambda
;;    ()
;;    (interactive)
;;    (etm-navigation-jump-by-index 9)))


(provide 'etm-keys-navigation)

(when
    (not load-file-name)
  (message "etm-keys-navigation.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))