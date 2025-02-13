;;; -*- lexical-binding: t -*-
;;; Author: ywatanabe
;;; Time-stamp: <2024-10-30 08:59:41>
;;; File: .dotfiles/.emacs.d/inits/070-tabs/001-setter-getter.el

(defun my/tab-set-buffer (type &optional tab-name buffer-name)
  "Set a buffer as either home, semi-home, or results for a given tab."
  (interactive "sType (home/semi-home/results): ")
  ;; Validate type
  (unless (member type '("home" "semi-home" "results"))
    (error "Type must be either 'home', 'semi-home', or 'results'"))
  ;; Use current tab's name if TAB-NAME is not provided.
  (unless tab-name
    (setq tab-name (alist-get 'name (tab-bar--current-tab))))
  ;; (setq tab-name (alist-get 'name (car (last (tab-bar-tabs))))))
  ;; Use current buffer's name if BUFFER-NAME is not provided.
  (unless buffer-name
    (setq buffer-name (buffer-name)))
  ;; Ensure the buffer exists.
  (get-buffer-create buffer-name)
  ;; Find or create tab entry in alist.
  (let ((tab-entry (assoc tab-name my/tab-buffer-alist)))
    (if tab-entry
        ;; Update existing tab entry.
        (setcdr tab-entry (cons (cons type buffer-name) (assoc-delete-all type (cdr tab-entry))))
      ;; Add new tab entry.
      (push (cons tab-name (list (cons type buffer-name))) my/tab-buffer-alist)))
  (message "The %s buffer of tab \"%s\" is set as \"%s\"" type tab-name buffer-name))

(defun my/tab-get-buffer (type &optional tab)
  "Get the buffer of a given type (home, semi-home, or results) associated with a tab."
  (interactive "sType (home/semi-home/results): ")
  ;; Use the current tab if none is specified.
  (unless tab (setq tab (tab-bar--current-tab)))
  (let* ((tab-name (alist-get 'name tab))
         (tab-entry (assoc tab-name my/tab-buffer-alist))
         (buffer-entry (assoc type (cdr tab-entry))))
    (cdr buffer-entry)))


(when (not load-file-name)
  (message "%s loaded." (file-name-nondirectory (or load-file-name buffer-file-name))))
