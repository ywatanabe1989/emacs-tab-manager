;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 14:23:21>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-buffer/etm-buffer-getters.el

(require 'etm-variables)
(require 'etm-buffer-checkers)

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

(provide 'etm-buffer-getters)

(when
    (not load-file-name)
  (message "etm-buffer-getters.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))