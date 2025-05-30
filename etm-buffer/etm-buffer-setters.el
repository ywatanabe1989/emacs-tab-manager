;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-14 13:53:23>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/etm-buffer/etm-buffer-setters.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'etm-core-variables)
(require 'etm-buffer-checkers)

(defun etm-buffer-set (type &optional tab-name buffer-obj)
  "Set buffer as TYPE for tab."
  (interactive
   (list
    (completing-read "Type: "
                     (append etm-registered-buffer-types
                             etm-custom-buffer-types))))
  (unless
      (member type
              (append etm-registered-buffer-types
                      etm-custom-buffer-types))
    (error "Invalid buffer type"))

  (unless tab-name
    (setq tab-name
          (alist-get 'name
                     (tab-bar--current-tab))))

  (unless buffer-obj
    (setq buffer-obj (current-buffer)))

  (let* ((buffer-name (if (stringp buffer-obj)
                          buffer-obj
                        (buffer-name buffer-obj)))
         (buffer-id (if (stringp buffer-obj)
                        buffer-obj
                      (with-current-buffer buffer-obj
                        (unless etm-buffer-id
                          (setq-local etm-buffer-id
                                      (format "etm-%s-%s"
                                              (format-time-string "%s")
                                              (random 10000))))
                        etm-buffer-id)))
         (tab-entry (assoc tab-name etm-registered-buffers)))

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
       etm-registered-buffers))

    (message "Set %s buffer for tab %s: %s"
             type tab-name buffer-name)))

;; Define functions

(defun etm-buffer-define-buffer-type-setter-function
    (type)
  "Define a buffer setting function for the given TYPE.
Example: For type 'home', creates `etm-buffer-set-home'."
  (eval
   `(defun ,(intern
             (format "etm-buffer-set-%s"
                     (if
                         (symbolp type)
                         (symbol-name type)
                       type)))
        ()
      ,(format "Set current buffer as %s buffer for current tab." type)
      (interactive)
      (etm-buffer-set ,type))))

(defun etm-buffer-define-buffer-type-setter-functions
    ()
  "Define buffer setting functions for all registered buffer types.
Examples:
`etm-buffer-set-home'
`etm-buffer-set-semi-home'
`etm-buffer-set-results'"
  (dolist
      (type etm-registered-buffer-types)
    (etm-buffer-define-buffer-type-setter-function type)))

;; Create buffer setting functions initially
(etm-buffer-define-buffer-type-setter-functions)


(provide 'etm-buffer-setters)

(when
    (not load-file-name)
  (message "etm-buffer-setters.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))