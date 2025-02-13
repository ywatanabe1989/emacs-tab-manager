;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-20 06:25:32
;;; Timestamp: <2025-01-20 06:25:32>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/inits/200-interface/070-tab/999-tab-bindings.el

(global-set-key (kbd "C-M-c") 'my/tab-close-others)
(bind-key* (kbd "C-M-j") 'my/tab-jump-by-name)

;; Global
(global-unset-key (kbd "M-1"))
(global-unset-key (kbd "M-2"))
(global-unset-key (kbd "M-3"))
(global-unset-key (kbd "M-4"))
(global-unset-key (kbd "M-5"))
(global-unset-key (kbd "M-6"))
(global-unset-key (kbd "M-7"))
(global-unset-key (kbd "M-8"))
(global-unset-key (kbd "M-9"))

(bind-key* (kbd "M-1") (lambda () (interactive) (my/tab-jump-to 1)))
(bind-key* (kbd "M-2") (lambda () (interactive) (my/tab-jump-to 2)))
(bind-key* (kbd "M-3") (lambda () (interactive) (my/tab-jump-to 3)))
(bind-key* (kbd "M-4") (lambda () (interactive) (my/tab-jump-to 4)))
(bind-key* (kbd "M-5") (lambda () (interactive) (my/tab-jump-to 5)))
(bind-key* (kbd "M-6") (lambda () (interactive) (my/tab-jump-to 6)))
(bind-key* (kbd "M-7") (lambda () (interactive) (my/tab-jump-to 7)))
(bind-key* (kbd "M-8") (lambda () (interactive) (my/tab-jump-to 8)))
(bind-key* (kbd "M-9") (lambda () (interactive) (my/tab-jump-to 9)))
(bind-key* (kbd "M-h") (lambda () (interactive) (my/tab-jump-to-buffer "home")))
(bind-key* (kbd "M-H") (lambda () (interactive) (my/tab-set-buffer "home")))
(bind-key* (kbd "M-r") (lambda () (interactive) (my/tab-jump-to-buffer "results")))
(bind-key* (kbd "M-R") (lambda () (interactive) (my/tab-set-buffer "results")))
(bind-key* (kbd "M-s") (lambda () (interactive) (my/tab-jump-to-buffer "semi-home")))
(bind-key* (kbd "M-S") (lambda () (interactive) (my/tab-set-buffer "semi-home")))

(when (not load-file-name)
  (message "%s loaded." (file-name-nondirectory (or load-file-name buffer-file-name))))