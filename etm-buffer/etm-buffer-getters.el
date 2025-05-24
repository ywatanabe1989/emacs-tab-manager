;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-14 12:42:55>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/etm-buffer/etm-buffer-getters.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'etm-core-variables)
(require 'etm-buffer-checkers)

(defun --etm-buffer-get (type &optional tab)
  "Get buffer of TYPE from TAB."
  (interactive
   (list
    (completing-read "Type: "
                     (append etm-registered-buffer-types
                             etm-custom-buffer-types))))
  (unless tab
    (setq tab
          (tab-bar--current-tab)))
  (let* ((tab-name
          (alist-get 'name tab))
         (tab-entry
          (assoc tab-name etm-registered-buffers))
         (buffer-name
          (cdr (assoc type (cdr tab-entry)))))

    ;; Return the buffer name
    buffer-name))


(provide 'etm-buffer-getters)

(when
    (not load-file-name)
  (message "etm-buffer-getters.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))