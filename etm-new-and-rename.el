;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 14:43:33>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-new-and-rename.el

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
;;   ;; (etm-navigation-jump-to 1)
;;   ;; (etm-close-by-name "default")
;;   )

;; ;; (add-hook 'after-init-hook #'etm-startup)

(provide 'etm-new-and-rename)

(when
    (not load-file-name)
  (message "etm-new-and-rename.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))