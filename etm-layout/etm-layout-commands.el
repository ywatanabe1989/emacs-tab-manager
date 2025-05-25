;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-25 11:10:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-layout/etm-layout-commands.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;; Interactive commands for ETM layout management
;; Provides user-facing commands for layout preview and management

(require 'etm-layout-preview)

;; Interactive Commands
;; ----------------------------------------

(defun etm-layout-list ()
  "Display a list of all saved ETM layouts with preview capability."
  (interactive)
  (let ((layouts (etm-layout-scan-directory))
        (buffer (get-buffer-create "*ETM Layouts*")))
    (with-current-buffer buffer
      (etm-layout-list-mode)
      (setq tabulated-list-entries
            (mapcar (lambda (layout)
                      (list layout (etm-layout-format-list-entry layout)))
                    layouts))
      (tabulated-list-print)
      (local-set-key (kbd "RET") #'etm-layout-load-at-point)
      (local-set-key (kbd "SPC") #'etm-layout-preview-at-point)
      (local-set-key (kbd "p") #'etm-layout-preview-at-point)
      (local-set-key (kbd "q") #'quit-window))
    (switch-to-buffer buffer)
    (message "Press RET to load, SPC/p to preview, q to quit")))

(defun etm-layout-load-at-point ()
  "Load the layout at point in the layout list."
  (interactive)
  (let ((layout-info (etm-layout-get-at-point)))
    (when layout-info
      (let ((layout-name (plist-get layout-info :name)))
        (if (fboundp (intern (format "etm-open-%s" layout-name)))
            (progn
              (call-interactively (intern (format "etm-open-%s" layout-name)))
              (quit-window))
          (error "Layout function not found: etm-open-%s" layout-name))))))

(defun etm-layout-preview (layout-name)
  "Preview a specific LAYOUT-NAME without loading it."
  (interactive
   (list (completing-read "Layout to preview: "
                          (mapcar (lambda (l) (plist-get l :name))
                                  (etm-layout-scan-directory))
                          nil t)))
  (let ((layout-info (cl-find layout-name 
                              (etm-layout-scan-directory)
                              :key (lambda (l) (plist-get l :name))
                              :test #'string=)))
    (if layout-info
        (etm-layout-show-preview layout-info)
      (error "Layout not found: %s" layout-name))))

(defun etm-layout-preview-quit ()
  "Close the layout preview window."
  (interactive)
  (when-let ((window (get-buffer-window etm-layout-preview-buffer)))
    (delete-window window))
  (when-let ((buffer (get-buffer etm-layout-preview-buffer)))
    (kill-buffer buffer)))

;; Key Bindings
;; ----------------------------------------

(defun etm-layout-setup-preview-keys ()
  "Set up keybindings for layout preview in ETM command map."
  (define-key etm-command-map (kbd "p") #'etm-layout-preview)
  (define-key etm-command-map (kbd "P") #'etm-layout-list)
  (define-key etm-command-map (kbd "l") #'etm-layout-list))

;; Auto-setup keys when loaded with ETM
(with-eval-after-load 'etm-keys-command-map
  (etm-layout-setup-preview-keys))

(provide 'etm-layout-commands)