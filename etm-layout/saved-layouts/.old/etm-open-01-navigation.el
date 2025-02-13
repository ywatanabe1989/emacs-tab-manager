;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-22 22:17:47
;;; Timestamp: <2025-01-22 22:17:47>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/inits/03-visu-070-tab-01-navigation.el

;; 1. Main Tab Navigation
;; ----------------------------------------

(defun my/tab-jump-to (index)
  "Jump to the tab at the specified INDEX."
  (interactive "p")
  (tab-bar-select-tab index))

(defun my/tab-jump-by-name (name)
  "Jump to tab with specified NAME."
  (interactive
   (list
    (completing-read "Tab name: "
                     (mapcar (lambda (tab)
                               (alist-get 'name tab))
                             (tab-bar-tabs)))))
  (let* ((tabs (tab-bar-tabs))
         (tab-index (cl-position name tabs
                                 :test (lambda (name tab)
                                         (string= name (alist-get 'name tab))))))
    (if tab-index
        (tab-bar-select-tab (1+ tab-index))
      (message "No tab named '%s' found" name))))

;; 2. Core Tab Operations
;; ----------------------------------------

(defun my/tab-move (&optional step)
  "Move to the next tab, or move STEP tabs if specified."
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (read-number "Enter step: " 1))))
  (tab-move step))

;; 3. Buffer Navigation
;; ----------------------------------------

(defun my/tab-jump-to-buffer (type)
  "Jump to the buffer of a given type associated with the current tab."
  (interactive "sType (home/semi-home/results): ")
  (let ((buffer (my/tab-get-buffer type)))
    (if buffer
        (progn
          (switch-to-buffer buffer)
          (message "Switched to %s buffer." type))
      (message "No %s buffer set for this tab" type))))

;; (defun my/tab-jump-to-buffer (type)
;;   "Jump to the buffer of a given type associated with the current tab."
;;   (interactive "sType (home/semi-home/results): ")
;;   (let ((buffer (my/tab-get-buffer type)))
;;     (if buffer
;;         (progn
;;           (switch-to-buffer buffer)
;;           (message "Switched to %s buffer." type))
;;       (message "No %s buffer set for this tab" type))))

;; (defun my/tab-jump-to (index)
;;   "Jump to the tab at the specified INDEX."
;;   (interactive "p")
;;   (tab-bar-select-tab index))

;; (defun my/tab-jump-by-name (name)
;;   "Jump to tab with specified NAME."
;;   (interactive
;;    (list
;;     (completing-read "Tab name: "
;;                      (mapcar (lambda (tab)
;;                                (alist-get 'name tab))
;;                              (tab-bar-tabs)))))
;;   (let* ((tabs (tab-bar-tabs))
;;          (tab-index (cl-position name tabs
;;                                  :test (lambda (name tab)
;;                                          (string= name (alist-get 'name tab))))))
;;     (if tab-index
;;         (tab-bar-select-tab (1+ tab-index))
;;       (message "No tab named '%s' found" name))))

;; (defun my/tab-move (&optional step)
;;   "Move to the next tab, or move STEP tabs if specified."
;;   (interactive
;;    (list (if current-prefix-arg
;;              (prefix-numeric-value current-prefix-arg)
;;            (read-number "Enter step: " 1))))
;;   (tab-move step))


(when (not load-file-name)
  (message "%s loaded." (file-name-nondirectory (or load-file-name buffer-file-name))))