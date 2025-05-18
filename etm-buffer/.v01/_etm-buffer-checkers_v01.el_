;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-09 23:40:09>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/etm-buffer/etm-buffer-checkers.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'etm-core-variables)

;; Checker
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

(defun --etm-buffer-protected-p
    (buffer-name)
  (member
   buffer-name
   etm-protected-buffers))


(provide 'etm-buffer-checkers)

(when
    (not load-file-name)
  (message "etm-buffer-checkers.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))