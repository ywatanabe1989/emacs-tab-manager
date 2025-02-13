;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 23:09:15>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-navigation.el

(require 'etm-variables)

(defun etm-navigation-jump-to
    (index)
  "Jump to tab at INDEX."
  (interactive "p")
  (tab-bar-select-tab index))

(defun etm-navigation-jump-by-name
    (name)
  "Jump to tab with NAME."
  (interactive
   (list
    (completing-read "Tab name: "
                     (mapcar
                      (lambda
                        (tab)
                        (alist-get 'name tab))
                      (tab-bar-tabs)))))
  (let*
      ((tabs
        (tab-bar-tabs))
       (tab-index
        (cl-position name tabs
                     :test
                     (lambda
                       (name tab)
                       (string= name
                                (alist-get 'name tab))))))
    (when tab-index
      (tab-bar-select-tab
       (1+ tab-index)))))

(defun etm-navigation-move
    (&optional step)
  "Move current tab STEP positions."
  (interactive
   (list
    (if current-prefix-arg
        (prefix-numeric-value current-prefix-arg)
      (read-number "Move steps: " 1))))
  (tab-move step))

(provide 'etm-navigation)

(when
    (not load-file-name)
  (message "etm-navigation.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))