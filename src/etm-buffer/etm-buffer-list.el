;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-31 01:00:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-buffer/etm-buffer-list.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; List and display registered buffers in ETM

(require 'etm-core-variables)
(require 'etm-buffer-getters)
(require 'etm-buffer-checkers)
(require 'etm-buffer-numeric)

(defun etm-list-registered-buffers ()
  "List all numeric buffers registered for the current tab."
  (interactive)
  (let* ((tab-name (alist-get 'name (tab-bar--current-tab)))
         (buffer "*ETM Registered Buffers*"))
    
    ;; Create or clear the buffer
    (with-current-buffer (get-buffer-create buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        
        ;; Header
        (insert (format "ETM Numeric Buffers for Tab: %s\n" tab-name))
        (insert (make-string 50 ?=) "\n\n")
        
        ;; Numeric buffers
        (insert "NUMERIC BUFFERS:\n")
        (insert (make-string 20 ?-) "\n")
        (let ((numeric-buffers (--etm-numeric-get-tab-buffers tab-name))
              (has-numeric-buffers nil))
          (if numeric-buffers
              (dolist (entry (sort numeric-buffers (lambda (a b) (< (car a) (car b)))))
                (let* ((id (car entry))
                       (buffer-name (cdr entry))
                       (exists (get-buffer buffer-name)))
                  (setq has-numeric-buffers t)
                  (insert (format "  %d: %s%s\n" 
                                  id 
                                  buffer-name
                                  (if exists "" " (killed)")))))
            (insert "  No numeric buffers registered.\n"))
          
          ;; Add registration hint if no numeric buffers
          (unless has-numeric-buffers
            (insert "  Use M-t b r to register current buffer with a number.\n")))
        
        ;; Footer with keybinding help
        (insert "\n" (make-string 50 ?-) "\n")
        (insert "KEYBINDINGS:\n")
        (insert "  M-t b r   - Register with numeric ID\n")
        (insert "  M-t 1-9   - Jump to numeric buffer\n")
        (insert "  M-t b l   - List numeric buffers\n")
        (insert "  M-t b c   - Clean up dead buffers\n")
        (insert "  M-t b ?   - Show numeric buffer help\n")
        
        ;; Make buffer read-only
        (setq buffer-read-only t)
        (goto-char (point-min))))
    
    ;; Display the buffer
    (display-buffer buffer '((display-buffer-reuse-window
                             display-buffer-pop-up-window)
                            (window-height . 0.4)))))

(defun etm-list-all-tabs-buffers ()
  "List numeric buffers across all tabs."
  (interactive)
  (let ((buffer "*ETM All Numeric Buffers*"))
    
    ;; Create or clear the buffer
    (with-current-buffer (get-buffer-create buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        
        ;; Header
        (insert "ETM Numeric Buffers - All Tabs\n")
        (insert (make-string 50 ?=) "\n\n")
        
        ;; Get all tabs
        (let ((tabs (tab-bar-tabs))
              (total-buffers 0))
          (dolist (tab tabs)
            (let ((tab-name (alist-get 'name tab)))
              ;; Tab header
              (insert (format "\nTab: %s\n" tab-name))
              (insert (make-string 30 ?-) "\n")
              
              ;; Numeric buffers for this tab
              (let ((numeric-buffers (--etm-numeric-get-tab-buffers tab-name)))
                (if numeric-buffers
                    (progn
                      (dolist (entry (sort numeric-buffers (lambda (a b) (< (car a) (car b)))))
                        (let* ((id (car entry))
                               (buffer-name (cdr entry))
                               (exists (get-buffer buffer-name)))
                          (setq total-buffers (1+ total-buffers))
                          (insert (format "  %d: %s%s\n" 
                                          id 
                                          buffer-name
                                          (if exists "" " (killed)")))))
                      (insert "\n"))
                  (insert "  No numeric buffers registered.\n\n")))))
          
          ;; Summary
          (insert (make-string 50 ?-) "\n")
          (insert (format "Total tabs: %d\n" (length tabs)))
          (insert (format "Total numeric buffers: %d\n" total-buffers)))
        
        ;; Make buffer read-only
        (setq buffer-read-only t)
        (goto-char (point-min))))
    
    ;; Display the buffer
    (display-buffer buffer '((display-buffer-reuse-window
                             display-buffer-pop-up-window)
                            (window-height . 0.5)))))

(provide 'etm-buffer-list)

;;; etm-buffer-list.el ends here