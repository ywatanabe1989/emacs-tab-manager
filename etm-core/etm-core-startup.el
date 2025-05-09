;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 08:46:45>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-core/etm-core-startup.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'etm-core-variables)
(require 'etm-close)
(require 'etm-buffer-navigation)

(defcustom etm-startup-layouts-list '("neurovista" "lisp" "genai")
  "List of layout names to automatically open at startup.
Each name should match an existing layout function `etm-open-LAYOUTNAME'."
  :type '(repeat string)
  :group 'etm)

(defun etm-startup-layouts ()
  "Open all layouts specified in `etm-startup-layouts-list'.
Closes the default tab after opening configured layouts."
  (interactive)
  (dolist (layout-name etm-startup-layouts-list)
    (let ((layout-func (intern (concat "etm-open-" layout-name))))
      (when (fboundp layout-func)
        (funcall layout-func))))

  ;; Clean up default tab if it exists
  (etm-close-by-name "default")

  ;; Jump to first tab
  (when (> (length (tab-bar-tabs)) 0)
    (etm-navigation-jump-by-index 1))

  (message "ETM startup layouts loaded"))

(defun etm-startup-edit-layouts ()
  "Edit the list of startup layouts through customize interface."
  (interactive)
  (customize-variable 'etm-startup-layouts-list))

(provide 'etm-core-startup)

(when
    (not load-file-name)
  (message "etm-core-startup.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))