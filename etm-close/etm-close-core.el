;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-19 07:26:56>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-close/etm-close-core.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'etm-core-tab-id)

(defun etm-reset
    ()
  "Close all tabs and call `etm-startup'."
  (interactive)
  (etm-close-all)
  (sleep-for 1)
  (etm-startup))

(defun etm-close
    ()
  "Close current tab and move to next."
  (interactive)
  (tab-close)
  (tab-next)
  (if (etm-tab-first-tab-p)
      (tab-previous)))

(defun etm-close-all
    ()
  "Close all tabs."
  (interactive)
  (if
      (fboundp 'tab-bar-mode)
      (progn
        (tab-bar-mode 1)
        (while
            (>
             (length
              (tab-bar-tabs))
             1)
          (tab-bar-close-tab))
        (tab-bar-rename-tab nil)
        (message "All tabs closed except the current one."))
    (message "Tab functionality not available in this Emacs version.")))

(defun etm-close-by-name (tab-name)
  "Close the specified tab if it exists and is not the only tab; otherwise, return nil."
  (interactive "sTab name to close: ")
  (let* ((tabs (tab-bar-tabs))
         (tab-index (cl-position tab-name tabs
                                 :test
                                 (lambda (name tab)
                                   (string= name
                                            (alist-get 'name tab))))))
    (when (and tab-index
               (> (length tabs) 1))
      (tab-bar-close-tab (1+ tab-index))
      t)))

;; (defun etm-close-by-name
;;     (tab-name)
;;   "Close the specified tab if it exists and is not the only tab; otherwise, return nil."
;;   (interactive "sTab name to close: ")
;;   (let*
;;       ((tabs
;;         (tab-bar-tabs))
;;        (tab-index
;;         (cl-position tab-name tabs
;;                      :test
;;                      (lambda
;;                        (name tab)
;;                        (string= name
;;                                 (alist-get 'name tab))))))
;;     (if
;;         (and tab-index
;;              (>
;;               (length tabs)
;;               1))
;;         (progn
;;           (tab-bar-close-tab
;;            (1+ tab-index))
;;           t)
;;       nil)))


(provide 'etm-close-core)

(when
    (not load-file-name)
  (message "etm-close-core.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))