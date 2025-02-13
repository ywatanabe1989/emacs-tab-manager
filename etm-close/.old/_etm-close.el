;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 23:14:03>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-close/_etm-close.el

;; 1. Main Tab Control Functions
;; ----------------------------------------

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
  (tab-next))

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

;; 2. Core Tab Operations
;; ----------------------------------------

(defun etm-close-by-name
    (tab-name)
  "Close the specified tab if it exists and is not the only tab; otherwise, return nil."
  (interactive "sTab name to close: ")
  (let*
      ((tabs
        (tab-bar-tabs))
       (tab-index
        (cl-position tab-name tabs
                     :test
                     (lambda
                       (name tab)
                       (string= name
                                (alist-get 'name tab))))))
    (if
        (and tab-index
             (>
              (length tabs)
              1))
        (progn
          (tab-bar-close-tab
           (1+ tab-index))
          t)
      nil)))

;; 3. Helper Functions
;; ----------------------------------------

;; not used
(defun --etm-close-by-id
    (tab-id)
  (tab-bar-close-tab
   (- tab-id 1)))

;; not used
(defun --etm-close-1
    ()
  (etm-navigation-jump-to 1)
  (tab-close))

;; not used
(defun --etm-close-and-next
    ()
  "Close the current tab and move to the next one."
  (tab-close)
  (tab-next))

;; not used
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

;; (defun etm-close-others
;;     ()
;;   "Close all tabs except current."
;;   (interactive)
;;   (let*
;;       ((current-tab-name
;;         (alist-get 'name
;;                    (tab-bar--current-tab)))
;;        (tabs
;;         (tab-bar-tabs)))
;;     (dolist
;;         (tab tabs)
;;       (let
;;           ((tab-name
;;             (alist-get 'name tab)))
;;         (unless
;;             (string= tab-name current-tab-name)
;;           (etm-close-by-name tab-name))))))

;; (defun etm-close-by-name
;;     (tab-name)
;;   "Close tab with TAB-NAME."
;;   (interactive "sTab name: ")
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
;;     (when
;;         (and tab-index
;;              (>
;;               (length tabs)
;;               1))
;;       (tab-bar-close-tab
;;        (1+ tab-index))
;;       t)))

;; (defun etm-close-others
;;     ()
;;   "Close all tabs except the current one."
;;   (interactive)
;;   (let*
;;       ((current-tab-name
;;         (alist-get 'name
;;                    (tab-bar--current-tab)))
;;        (tabs
;;         (tab-bar-tabs)))
;;     (dolist
;;         (tab tabs)
;;       (let
;;           ((tab-name
;;             (alist-get 'name tab)))
;;         (unless
;;             (string= tab-name current-tab-name)
;;           (etm-close-by-name tab-name))))))

(provide '_etm-close)

(when
    (not load-file-name)
  (message "_etm-close.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))