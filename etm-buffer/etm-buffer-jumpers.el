;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:00:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-buffer/etm-buffer-jumpers.el

(require 'etm-variables)
(require 'etm-buffer-checkers)

(defun etm-buffer-jump-to
    (type)
  "Jump to buffer of TYPE in current tab."
  (interactive
   (list
    (completing-read "Jump to buffer type: "
                     (append etm-registered-buffer-types
                             etm-custom-buffer-types))))
  (let
      ((buf
        (etm-buffer-get type)))
    (if buf
        (switch-to-buffer buf)
      (message "No %s buffer set for current tab" type))))

;; Define jump functions
(defun etm-define-buffer-jump-to-function
    (type)
  "Define a buffer jump function for the given TYPE.
Example: For type 'home', creates `etm-buffer-jump-to-home'."
  (eval
   `(defun ,(intern
             (format "etm-buffer-jump-to-%s"
                     (if
                         (symbolp type)
                         (symbol-name type)
                       type)))
        ()
      ,(format "Jump to %s buffer of current tab." type)
      (interactive)
      (etm-buffer-jump-to ,type))))

;; (defun etm-define-buffer-jump-to-function
;;     (type)
;;   "Define a buffer jump function for the given TYPE.
;; Example: For type 'home', creates `etm-buffer-jump-to-home'."
;;   (eval
;;    `(defun ,(intern
;;              (format "etm-buffer-jump-to-%s" type))
;;         ()
;;       ,(format "Jump to %s buffer of current tab." type)
;;       (interactive)
;;       (etm-buffer-jump-to ,type))))

(defun etm-define-buffer-jump-to-functions
    ()
  "Define buffer jump functions for all registered buffer types.
Examples:
`etm-buffer-jump-to-home'
`etm-buffer-jump-to-semi-home'
`etm-buffer-jump-to-results'"
  (dolist
      (type etm-registered-buffer-types)
    (etm-define-buffer-jump-to-function type)))

(etm-define-buffer-jump-to-functions)

(provide 'etm-buffer-jumpers)

(when
    (not load-file-name)
  (message "etm-buffer-jumpers.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))