;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-24 16:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-buffer/etm-buffer-numeric.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;; Numeric buffer ID system for ETM
;; Automatically assigns buffer IDs (1, 2, 3...) and provides keybindings

(require 'etm-core-variables)

;; Use variables from core
;; ----------------------------------------

;; etm-numeric-buffers and etm-max-numeric-buffers are defined in etm-core-variables

;; Core Functions
;; ----------------------------------------

(defun --etm-numeric-get-tab-buffers (tab-name)
  "Get numeric buffer configuration for TAB-NAME."
  (cdr (assoc tab-name etm-numeric-buffers)))

(defun --etm-numeric-get-next-id (tab-name)
  "Get next available numeric ID for TAB-NAME."
  (let* ((tab-buffers (--etm-numeric-get-tab-buffers tab-name))
         (used-ids (mapcar #'car tab-buffers))
         (next-id 1))
    (while (and (<= next-id etm-max-numeric-buffers)
                (member next-id used-ids))
      (setq next-id (1+ next-id)))
    (if (<= next-id etm-max-numeric-buffers)
        next-id
      nil)))

(defun --etm-numeric-register-buffer (buffer-name &optional tab-name)
  "Register BUFFER-NAME with next available numeric ID for TAB-NAME.
Returns the assigned ID or nil if no slots available."
  (unless tab-name
    (setq tab-name (alist-get 'name (tab-bar--current-tab))))
  
  (let ((next-id (--etm-numeric-get-next-id tab-name)))
    (when next-id
      (let* ((tab-entry (assoc tab-name etm-numeric-buffers))
             (new-entry (cons next-id buffer-name)))
        (if tab-entry
            ;; Add to existing tab
            (setcdr tab-entry (cons new-entry (cdr tab-entry)))
          ;; Create new tab entry
          (push (cons tab-name (list new-entry)) etm-numeric-buffers))
        
        (message "Buffer '%s' registered as ID %d in tab '%s'" 
                 buffer-name next-id tab-name)
        next-id))))

(defun --etm-numeric-unregister-buffer (id-or-buffer &optional tab-name)
  "Unregister buffer by ID or buffer name from TAB-NAME."
  (unless tab-name
    (setq tab-name (alist-get 'name (tab-bar--current-tab))))
  
  (let* ((tab-entry (assoc tab-name etm-numeric-buffers))
         (tab-buffers (cdr tab-entry)))
    (when tab-buffers
      (let ((updated-buffers
             (if (numberp id-or-buffer)
                 ;; Remove by ID
                 (assq-delete-all id-or-buffer tab-buffers)
               ;; Remove by buffer name
               (cl-remove-if (lambda (entry) 
                              (string= (cdr entry) id-or-buffer))
                            tab-buffers))))
        (setcdr tab-entry updated-buffers)))))

(defun --etm-numeric-get-buffer-by-id (id &optional tab-name)
  "Get buffer name for ID in TAB-NAME."
  (unless tab-name
    (setq tab-name (alist-get 'name (tab-bar--current-tab))))
  
  (let ((tab-buffers (--etm-numeric-get-tab-buffers tab-name)))
    (cdr (assq id tab-buffers))))

(defun --etm-numeric-get-id-by-buffer (buffer-name &optional tab-name)
  "Get numeric ID for BUFFER-NAME in TAB-NAME."
  (unless tab-name
    (setq tab-name (alist-get 'name (tab-bar--current-tab))))
  
  (let ((tab-buffers (--etm-numeric-get-tab-buffers tab-name)))
    (car (rassoc buffer-name tab-buffers))))

;; Interactive Functions
;; ----------------------------------------

(defun etm-numeric-register-current-buffer ()
  "Register current buffer with next available numeric ID."
  (interactive)
  (let ((buffer-name (buffer-name)))
    (if (--etm-numeric-register-buffer buffer-name)
        (message "Registered current buffer '%s'" buffer-name)
      (message "No available numeric slots (max %d)" etm-max-numeric-buffers))))

(defun etm-numeric-jump-to-buffer (id)
  "Jump to buffer with numeric ID."
  (interactive "nBuffer ID (1-9): ")
  (let ((buffer-name (--etm-numeric-get-buffer-by-id id)))
    (if buffer-name
        (if (get-buffer buffer-name)
            (switch-to-buffer buffer-name)
          (message "Buffer '%s' (ID %d) no longer exists" buffer-name id)
          (--etm-numeric-unregister-buffer id))
      (message "No buffer registered with ID %d" id))))

(defun etm-numeric-list-buffers ()
  "List all numeric buffers for current tab."
  (interactive)
  (let* ((tab-name (alist-get 'name (tab-bar--current-tab)))
         (tab-buffers (--etm-numeric-get-tab-buffers tab-name)))
    (if tab-buffers
        (progn
          (message "Numeric buffers in tab '%s':" tab-name)
          (dolist (entry (sort tab-buffers (lambda (a b) (< (car a) (car b)))))
            (let ((id (car entry))
                  (buffer-name (cdr entry)))
              (message "  %d: %s%s" id buffer-name
                      (if (get-buffer buffer-name) "" " (not found)")))))
      (message "No numeric buffers in tab '%s'" tab-name))))

(defun etm-numeric-auto-register-buffer ()
  "Auto-register current buffer if it's not already registered."
  (let* ((buffer-name (buffer-name))
         (tab-name (alist-get 'name (tab-bar--current-tab)))
         (existing-id (--etm-numeric-get-id-by-buffer buffer-name tab-name)))
    (unless existing-id
      (--etm-numeric-register-buffer buffer-name tab-name))))

;; Keybinding Generation
;; ----------------------------------------

(defun etm-numeric-define-keybindings (prefix-key)
  "Define keybindings for numeric buffer jumping.
PREFIX-KEY should be like 'M-t b' - this will create M-t b 1, M-t b 2, etc."
  (dotimes (i etm-numeric-max-buffers)
    (let ((id (1+ i))
          (key-sequence (concat prefix-key " " (number-to-string (1+ i)))))
      (global-set-key (kbd key-sequence)
                      `(lambda () 
                         (interactive)
                         (etm-numeric-jump-to-buffer ,id))))))

;; Cleanup function
;; ----------------------------------------

(defun etm-numeric-cleanup-dead-buffers ()
  "Remove numeric buffer entries for buffers that no longer exist."
  (interactive)
  (let ((cleaned-count 0))
    (dolist (tab-entry etm-numeric-buffers)
      (let* ((tab-name (car tab-entry))
             (tab-buffers (cdr tab-entry))
             (live-buffers (cl-remove-if 
                           (lambda (entry)
                             (let ((buffer-name (cdr entry)))
                               (unless (get-buffer buffer-name)
                                 (setq cleaned-count (1+ cleaned-count))
                                 t)))
                           tab-buffers)))
        (setcdr tab-entry live-buffers)))
    (message "Cleaned up %d dead buffer entries" cleaned-count)))

;; Hook Integration
;; ----------------------------------------

(defun etm-numeric-setup-hooks ()
  "Setup hooks for automatic buffer management."
  ;; Auto-register buffers when switching to them
  (add-hook 'buffer-list-update-hook #'etm-numeric-auto-register-buffer)
  
  ;; Clean up dead buffers periodically
  (run-with-timer 60 60 #'etm-numeric-cleanup-dead-buffers))

(provide 'etm-buffer-numeric)

;;; etm-buffer-numeric.el ends here