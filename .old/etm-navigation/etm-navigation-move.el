;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 23:18:10>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-navigation/etm-navigation-move.el
;;; -*- coding: utf-8; lexical-binding: t -*-

(defun etm-navigation-move
    (&optional step)
  "Move current tab STEP positions."
  (interactive
   (list
    (if current-prefix-arg
        (prefix-numeric-value current-prefix-arg)
      (read-number "Move steps: " 1))))
  (tab-move step))

(provide 'etm-navigation-move)

(when
    (not load-file-name)
  (message "etm-navigation-move.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))