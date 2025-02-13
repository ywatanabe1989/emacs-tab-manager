;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 23:07:52>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-layout.el
;; etm-layout.el

(require 'etm-variables)

(defun etm-layout-set
    (name layout-func)
  "Register a layout with NAME and LAYOUT-FUNC.
LAYOUT-FUNC should be a function that creates the desired layout."
  (setq etm-saved-layouts
        (cons
         (cons name layout-func)
         (assq-delete-all name etm-saved-layouts)))
  (etm-layout-get name))

(defun etm-layout-get
    (name)
  "Get registered layout function by NAME."
  (cdr
   (assoc name etm-saved-layouts)))

(defun etm-layout-apply
    (name)
  "Apply registered layout by NAME."
  (interactive
   (list
    (completing-read "Layout: "
                     (mapcar #'car etm-saved-layouts))))
  (let
      ((layout-func
        (etm-layout-get name)))
    (when layout-func
      (funcall layout-func))))

(defun etm-layout-remove
    (name)
  "Remove layout by NAME from registry."
  (interactive
   (list
    (completing-read "Remove layout: "
                     (mapcar #'car etm-saved-layouts))))
  (setq etm-saved-layouts
        (assq-delete-all name etm-saved-layouts)))

(defun etm-layout-list
    ()
  "List all registered layouts."
  (interactive)
  (mapcar #'car etm-saved-layouts))

(provide 'etm-layout)

(when
    (not load-file-name)
  (message "etm-layout.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))