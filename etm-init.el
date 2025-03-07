;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-14 01:42:41>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-init.el

(require 'etm-variables)
(require 'etm-layout)

(defun etm-init
    ()
  "Initialize Emacs Tab Manager."
  (interactive)
  (--etm-layout-load-all)
  (tab-bar-mode t)
  (setq tab-bar-show etm-show-tab-bar
        tab-bar-tab-hints t
        tab-bar-name-truncated t
        tab-bar-auto-width nil
        tab-bar-new-tab-to 'right
        tab-bar-close-button-show nil)
  (custom-set-faces
   '(tab-bar
     ((t
       (:background "gray20" :foreground "white"))))
   '(tab-bar-tab
     ((t
       (:inherit tab-bar :background "dark green" :foreground "gray60"))))
   '(tab-bar-tab-inactive
     ((t
       (:inherit tab-bar :background "gray20" :foreground "gray80"))))))

(provide 'etm-init)

(when
    (not load-file-name)
  (message "etm-init.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))