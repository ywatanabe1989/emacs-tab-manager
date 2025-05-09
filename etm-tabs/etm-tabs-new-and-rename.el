;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 08:44:10>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-tabs/etm-tabs-new-and-rename.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(defun etm-new
    (arg)
  (interactive
   (list
    (read-string "Enter string: ")))
  (tab-new)
  (tab-rename
   (message "%s" arg)))

(defun etm-rename
    (arg)
  (interactive
   (list
    (read-string "Enter string: ")))
  (tab-rename
   (message "%s" arg)))

;; (defun etm-startup
;;     ()
;;   (interactive)
;;   (tab-rename "default")

;;   ;; Main
;;   ;; (etm-neurovista "spartan")

;;   ;; ;; Removes the first tab
;;   ;; (etm-remove-1)
;;   ;; (etm-navigation-jump-by-index 1)
;;   ;; (etm-close-by-name "default")
;;   )

;; ;; (add-hook 'after-init-hook #'etm-startup)

(provide 'etm-tabs-new-and-rename)

(when
    (not load-file-name)
  (message "etm-tabs-new-and-rename.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))