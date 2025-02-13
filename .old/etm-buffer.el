;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 22:56:53>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-buffer.el

(require 'etm-variables)

;; Setter
;; ----------------------------------------

(defun etm-buffer-set
    (type &optional tab-name buffer-name)
  "Set buffer as TYPE for tab."
  (interactive
   (list
    (completing-read "Type: "
                     (append etm-registered-buffer-types
                             etm-custom-buffer-types))))
  (unless
      (member type
              (append etm-registered-buffer-types etm-custom-buffer-types))
    (error "Invalid buffer type"))
  (unless tab-name
    (setq tab-name
          (alist-get 'name
                     (tab-bar--current-tab))))
  (unless buffer-name
    (setq buffer-name
          (buffer-name)))
  (get-buffer-create buffer-name)
  (let
      ((tab-entry
        (assoc tab-name etm-registered-buffers)))
    (if tab-entry
        (setcdr tab-entry
                (cons
                 (cons type buffer-name)
                 (assoc-delete-all type
                                   (cdr tab-entry))))
      (push
       (cons tab-name
             (list
              (cons type buffer-name)))
       etm-registered-buffers)))
  (message "Set %s buffer for tab %s: %s" type tab-name buffer-name))

;; Define functions
(defun etm-define-buffer-set-functions
    ()
  "Define buffer setting functions for all registered buffer types.
Examples:
`etm-buffer-set-home'
`etm-buffer-set-semi-home'
`etm-buffer-set-results'
"
  (dolist
      (type etm-registered-buffer-types)
    (eval
     `(defun ,(intern
               (format "etm-buffer-set-%s" type))
          ()
        ,(format "Set current buffer as %s buffer for current tab." type)
        (interactive)
        (etm-buffer-set ,type)))))

;; Create buffer setting functions initially
(etm-define-buffer-set-functions)

;; Getter
;; ----------------------------------------

(defun etm-buffer-get
    (type &optional tab)
  "Get buffer of TYPE from TAB."
  (interactive
   (list
    (completing-read "Type: "
                     (append etm-registered-buffer-types
                             etm-custom-buffer-types))))
  (unless tab
    (setq tab
          (tab-bar--current-tab)))
  (let*
      ((tab-name
        (alist-get 'name tab))
       (tab-entry
        (assoc tab-name etm-registered-buffers))
       (buffer-entry
        (assoc type
               (cdr tab-entry))))
    (cdr buffer-entry)))

;; Jumper
;; ----------------------------------------

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
(defun etm-define-buffer-jump-to-functions
    ()
  "Define buffer jump functions for all registered buffer types.
Examples:
`etm-buffer-jump-to-home'
`etm-buffer-jump-to-semi-home'
`etm-buffer-jump-to-results'"
  (dolist
      (type etm-registered-buffer-types)
    (eval
     `(defun ,(intern
               (format "etm-buffer-jump-to-%s" type))
          ()
        ,(format "Jump to %s buffer of current tab." type)
        (interactive)
        (etm-buffer-jump-to ,type)))))

(etm-define-buffer-jump-to-functions)

;; Kill or Bury
;; ----------------------------------------

(defun etm-buffer-kill-or-bury
    ()
  "Kill buffer if not registered in any tab, otherwise bury it."
  (interactive)
  (let
      ((buf
        (current-buffer)))
    (if
        (--etm-buffer-registered-p
         (buffer-name buf))
        (progn
          (bury-buffer)
          (message "Buried"))
      (progn
        (kill-buffer)
        (message "Killed")))))

;; Helpers
;; ----------------------------------------

(defun --etm-buffer-registered-p
    (buffer-name &optional type tab)
  "Check if BUFFER-NAME is registered.
If TYPE is specified, check if registered as that type.
If TAB is specified, check only in that tab."
  (let
      ((found nil))
    (dolist
        (tab-entry etm-registered-buffers)
      (when
          (or
           (null tab)
           (string=
            (car tab-entry)
            (alist-get 'name tab)))
        (dolist
            (buffer-entry
             (cdr tab-entry))
          (when
              (and
               (string=
                (cdr buffer-entry)
                buffer-name)
               (or
                (null type)
                (string=
                 (car buffer-entry)
                 type)))
            (setq found t)))))
    found))

(provide 'etm-buffer)

(when
    (not load-file-name)
  (message "etm-buffer.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))