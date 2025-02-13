;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 16:44:27>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-buffer/etm-buffer-jumpers.el

(require 'etm-variables)
(require 'etm-buffer-checkers)
(require 'etm-navigation)

;; Define jump functions
(defun etm-buffer-define-buffer-type-jumper-function
    (type)
  "Define a buffer jump function for the given TYPE.
Example: For type 'home', creates `etm-navigation-jump-by-buffer-type-home'."
  (eval
   `(defun ,(intern
             (format "etm-navigation-jump-by-buffer-type-%s"
                     (if
                         (symbolp type)
                         (symbol-name type)
                       type)))
        ()
      ,(format "Jump to %s buffer of current tab." type)
      (interactive)
      (etm-navigation-jump-by-buffer-type ,type))))

(defun etm-buffer-define-buffer-type-jumper-functions
    ()
  "Define buffer jump functions for all registered buffer types.
Examples:
`etm-navigation-jump-by-buffer-type-home'
`etm-navigation-jump-by-buffer-type-semi-home'
`etm-navigation-jump-by-buffer-type-results'"
  (dolist
      (type etm-registered-buffer-types)
    (etm-buffer-define-buffer-type-jumper-function type)))

(etm-buffer-define-buffer-type-jumper-functions)

(provide 'etm-buffer-jumpers)

(when
    (not load-file-name)
  (message "etm-buffer-jumpers.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))