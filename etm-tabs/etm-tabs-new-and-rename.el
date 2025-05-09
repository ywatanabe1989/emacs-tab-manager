;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 08:44:10>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-tabs/etm-tabs-new-and-rename.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;;###autoload
(defun etm-new
    (arg)
  "Create a new tab with name ARG."
  (interactive
   (list
    (read-string "Enter tab name: ")))
  (tab-new)
  (tab-rename
   (message "%s" arg)))

;;;###autoload
(defun etm-rename
    (arg)
  "Rename current tab to ARG."
  (interactive
   (list
    (read-string "Enter tab name: ")))
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