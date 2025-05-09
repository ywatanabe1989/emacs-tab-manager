;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-09 19:45:10>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-close/etm-close-utils.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(defun etm-close-by-id (tab-id)
  "Close the tab with ID TAB-ID."
  (interactive "nTab ID to close: ")
  (tab-bar-close-tab (- tab-id 1)))

(defun etm-close-1 ()
  "Close tab with index 1."
  (interactive)
  (tab-bar-select-tab 1)
  (tab-close))

(defun etm-close-and-next ()
  "Close the current tab and move to the next one."
  (interactive)
  (tab-close)
  (tab-next))

(defun etm-close-by-name-and-prev ()
  "Close the current tab and move to the previous one."
  (interactive)
  (let* ((tabs (tab-bar-tabs))
        (current-index (tab-bar--current-tab-index))
        (prev-tab-index (1- current-index)))
    (tab-close)
    (when (and (>= prev-tab-index 0) 
               (< prev-tab-index (length (tab-bar-tabs))))
      (tab-bar-select-tab (1+ prev-tab-index)))))

(defun etm-close-others ()
  "Close all tabs except the current one."
  (interactive)
  (let ((current-tab (tab-bar--current-tab))
        (tabs (tab-bar-tabs)))
    (when (> (length tabs) 1)  ;; Only proceed if there's more than one tab
      (dolist (tab tabs)
        (unless (eq tab current-tab)
          (tab-bar-close-tab-by-name (alist-get 'name tab)))))))

(provide 'etm-close-utils)

(when (not load-file-name)
  (message "etm-close-utils.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))