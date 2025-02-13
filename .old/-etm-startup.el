;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-01-30 10:39:25>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/inits/03-visu-070-tab-01-startup.el
;;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(defun my/tab-startup
    ()
  (interactive)
  (tab-rename "default")

  ;; Main
  ;; (my/tab-neurovista "spartan")

  (my/tab-llemacs)

  ;; ;; Removes the first tab
  ;; (my/tab-remove-1)
  ;; (my/tab-jump-to 1)
  ;; (my/tab-close-by-tab-name "default")
  )

;; (add-hook 'after-init-hook #'my/tab-startup)


(when
    (not load-file-name)
  (message "03-visu-070-tab-01-startup.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))