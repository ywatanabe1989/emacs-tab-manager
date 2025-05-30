;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-31 01:00:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-buffer/etm-buffer-list.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;; List and display registered buffers in ETM

(require 'etm-core-variables)
(require 'etm-buffer-getters)
(require 'etm-buffer-checkers)
(require 'etm-buffer-numeric)

(defun etm-list-registered-buffers ()
  "List all registered buffers for the current tab.
Shows both type-based registrations (home, semi-home, etc.) 
and numeric registrations (1-9)."
  (interactive)
  (let* ((tab-name (alist-get 'name (tab-bar--current-tab)))
         (buffer "*ETM Registered Buffers*"))
    
    ;; Create or clear the buffer
    (with-current-buffer (get-buffer-create buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        
        ;; Header
        (insert (format "ETM Registered Buffers for Tab: %s\n" tab-name))
        (insert (make-string 50 ?=) "\n\n")
        
        ;; Type-based buffers
        (insert "TYPE-BASED BUFFERS:\n")
        (insert (make-string 20 ?-) "\n")
        (let ((tab-entry (assoc tab-name etm-registered-buffers))
              (has-typed-buffers nil))
          (if (cdr tab-entry)
              (dolist (type-entry (cdr tab-entry))
                (let* ((type (car type-entry))
                       (buffer-name (cdr type-entry))
                       (exists (get-buffer buffer-name)))
                  (setq has-typed-buffers t)
                  (insert (format "  %-12s: %s%s\n" 
                                  type 
                                  buffer-name
                                  (if exists "" " (killed)")))))
            (insert "  No type-based buffers registered.\n"))
          
          ;; Add registration hint if no typed buffers
          (unless has-typed-buffers
            (insert "  Use M-t h (home) or M-t s (semi-home) to register buffers.\n")))
        
        ;; Numeric buffers
        (insert "\n")
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
        (insert "  Type-based:\n")
        (insert "    M-t h     - Set as home buffer\n")
        (insert "    M-t s     - Set as semi-home buffer\n")
        (insert "    M-t r     - Set as results buffer\n")
        (insert "    C-c h/s/r - Jump to home/semi-home/results\n")
        (insert "  Numeric:\n")
        (insert "    M-t b r   - Register with numeric ID\n")
        (insert "    M-t 1-9   - Jump to numeric buffer\n")
        (insert "    M-t b l   - List numeric buffers only\n")
        (insert "    M-t b c   - Clean up dead buffers\n")
        
        ;; Make buffer read-only
        (setq buffer-read-only t)
        (goto-char (point-min))))
    
    ;; Display the buffer
    (display-buffer buffer '((display-buffer-reuse-window
                             display-buffer-pop-up-window)
                            (window-height . 0.4)))))

(defun etm-list-all-tabs-buffers ()
  "List registered buffers across all tabs."
  (interactive)
  (let ((buffer "*ETM All Registered Buffers*"))
    
    ;; Create or clear the buffer
    (with-current-buffer (get-buffer-create buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        
        ;; Header
        (insert "ETM Registered Buffers - All Tabs\n")
        (insert (make-string 50 ?=) "\n\n")
        
        ;; Get all tabs
        (let ((tabs (tab-bar-tabs)))
          (dolist (tab tabs)
            (let ((tab-name (alist-get 'name tab)))
              ;; Tab header
              (insert (format "\nTab: %s\n" tab-name))
              (insert (make-string 30 ?-) "\n")
              
              ;; Type-based buffers for this tab
              (let ((tab-entry (assoc tab-name etm-registered-buffers)))
                (when (cdr tab-entry)
                  (insert "  Type-based:\n")
                  (dolist (type-entry (cdr tab-entry))
                    (let* ((type (car type-entry))
                           (buffer-name (cdr type-entry))
                           (exists (get-buffer buffer-name)))
                      (insert (format "    %-10s: %s%s\n" 
                                      type 
                                      buffer-name
                                      (if exists "" " (killed)")))))))
              
              ;; Numeric buffers for this tab
              (let ((numeric-buffers (--etm-numeric-get-tab-buffers tab-name)))
                (when numeric-buffers
                  (insert "  Numeric:\n")
                  (dolist (entry (sort numeric-buffers (lambda (a b) (< (car a) (car b)))))
                    (let* ((id (car entry))
                           (buffer-name (cdr entry))
                           (exists (get-buffer buffer-name)))
                      (insert (format "    %d: %s%s\n" 
                                      id 
                                      buffer-name
                                      (if exists "" " (killed)")))))))))
          
          ;; Summary
          (insert "\n" (make-string 50 ?-) "\n")
          (insert (format "Total tabs: %d\n" (length tabs))))
        
        ;; Make buffer read-only
        (setq buffer-read-only t)
        (goto-char (point-min))))
    
    ;; Display the buffer
    (display-buffer buffer '((display-buffer-reuse-window
                             display-buffer-pop-up-window)
                            (window-height . 0.5)))))

(provide 'etm-buffer-list)

;;; etm-buffer-list.el ends here