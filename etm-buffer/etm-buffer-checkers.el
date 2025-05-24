;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-14 12:33:15>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/etm-buffer/etm-buffer-checkers.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'etm-core-variables)

;; Checker
;; ----------------------------------------

(defun --etm-buffer-registered-p (buffer &optional type tab)
  "Check if BUFFER is registered.
BUFFER can be a buffer name or buffer object.
If TYPE is specified, check if registered as that type.
If TAB is specified, check only in that tab."
  (let ((found nil)
        (buffer-name (if (bufferp buffer)
                         (buffer-name buffer)
                       buffer)))
    
    (dolist (tab-entry etm-registered-buffers)
      (when (or (null tab)
                (string= (car tab-entry)
                         (alist-get 'name tab)))
        (dolist (buffer-entry (cdr tab-entry))
          (let ((entry-type (car buffer-entry))
                (entry-buffer (cdr buffer-entry)))
            (when (and (string= entry-buffer buffer-name)
                       (or (null type)
                           (string= entry-type type)))
              (setq found t))))))
    
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