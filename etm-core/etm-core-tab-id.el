;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 08:38:02>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-core/etm-core-tab-id.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(defun etm-tab-get-current-tab-index ()
  "Return the 0-based index of the currently active tab. Returns nil if tab-bar-mode is not active or functions are unavailable."
  (if (and (boundp 'tab-bar-mode) tab-bar-mode
           (fboundp 'tab-bar-tabs))
      (let* ((tabs (tab-bar-tabs))
             (current-tab-fn (cond
                              ((fboundp 'tab-bar-current-tab)
                               #'tab-bar-current-tab)
                              ((fboundp 'tab-bar--current-tab)
                               #'tab-bar--current-tab)
                              (t nil)))
             (current-tab
              (when current-tab-fn (funcall current-tab-fn))))
        (cond
         ;; Handle the single tab case where tabs is 't'
         ((and (eq tabs t) current-tab) 0)
         ;; Handle the list case
         ((and (listp tabs) current-tab)
          ;; Find the position of the current tab object in the list
          (cl-position current-tab tabs :test #'equal))
         ;; Default to nil if something is wrong
         (t nil)))
    nil))

(defun etm-tab-first-tab-p ()
  (eq (etm-tab-get-current-tab-index) 0))

(provide 'etm-core-tab-id)

(when
    (not load-file-name)
  (message "etm-core-tab-id.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))