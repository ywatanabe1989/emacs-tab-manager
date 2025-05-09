;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 23:12:39>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-close-utils.el

;;; -*- coding: utf-8; lexical-binding: t -*-

(defun --etm-close-by-id
    (tab-id)
  (tab-bar-close-tab
   (- tab-id 1)))

(defun --etm-close-1
    ()
  (etm-navigation-jump-by-index 1)
  (tab-close))

(defun --etm-close-and-next
    ()
  "Close the current tab and move to the next one."
  (tab-close)
  (tab-next))

(defun --etm-close-by-name-and-prev
    ()
  "Close the current tab and move to the previous one."
  (let
      ((prev-tab-index
        (1-
         (tab-bar--current-tab-index))))
    (tab-close)
    (when
        (>= prev-tab-index 0)
      (tab-bar-select-tab
       (1+ prev-tab-index)))))

(defun etm-close-others ()
  "Close all tabs except the current one."
  (interactive)
  (let ((current-tab-index (tab-bar--current-tab-index)))
    (dotimes (i (length (tab-bar-tabs)))
      (unless (= i current-tab-index)
        (tab-bar-close-tab-by-position i)))
    (message "All other tabs closed.")))

(provide 'etm-close-utils)

(when
    (not load-file-name)
  (message "etm-close-utils.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))