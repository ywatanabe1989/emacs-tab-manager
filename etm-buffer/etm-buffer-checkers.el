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
        (buffer-obj (if (bufferp buffer) buffer (get-buffer buffer)))
        (buffer-id (when (bufferp buffer)
                     (buffer-local-value 'etm-buffer-id buffer))))

    ;; If we can't get a buffer object, just use name-based checking
    (if (and buffer-obj buffer-id)
        ;; Check by buffer ID
        (dolist (tab-entry etm-registered-buffers)
          (when (or (null tab)
                    (string= (car tab-entry)
                             (alist-get 'name tab)))
            (dolist (buffer-entry (cdr tab-entry))
              (when (and (consp (cdr buffer-entry))
                         (string= (cadr buffer-entry) buffer-id)
                         (or (null type)
                             (string= (car buffer-entry) type)))
                (setq found t)))))

      ;; Fallback to name-based checking
      (let ((buffer-name (if (bufferp buffer)
                             (buffer-name buffer)
                           buffer)))
        (dolist (tab-entry etm-registered-buffers)
          (when (or (null tab)
                    (string= (car tab-entry)
                             (alist-get 'name tab)))
            (dolist (buffer-entry (cdr tab-entry))
              (when (and (consp (cdr buffer-entry))
                         (string= (cddr buffer-entry) buffer-name)
                         (or (null type)
                             (string= (car buffer-entry) type)))
                (setq found t)))))))

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