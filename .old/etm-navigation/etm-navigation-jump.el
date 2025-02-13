;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 23:17:55>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-navigation/etm-navigation-jump.el
;;; -*- coding: utf-8; lexical-binding: t -*-

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

(provide 'etm-navigation-jump)

(when
    (not load-file-name)
  (message "etm-navigation-jump.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))