;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-24 16:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-buffer/etm-buffer-numeric.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Numeric buffer ID system for ETM
;; Stores buffer OBJECTS (not names) for reliable identification
;; Each buffer object has a unique internal ID in Emacs

(require 'etm-core-variables)

;; Use variables from core
;; ----------------------------------------

;; etm-numeric-buffers and etm-max-numeric-buffers are defined in etm-core-variables

;; Core Functions
;; ----------------------------------------

(defun --etm-get-current-tab-key ()
  "Get the unique key for the current tab (ID if available, else name)."
  (let ((current-tab (tab-bar--current-tab)))
    (or (alist-get 'etm-id current-tab)
        (alist-get 'name current-tab))))

(defun --etm-numeric-get-tab-buffers (tab-key)
  "Get numeric buffer configuration for TAB-KEY (ID or name)."
  (cdr (assoc tab-key etm-numeric-buffers)))

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

(defun --etm-numeric-register-buffer (buffer-name &optional tab-key)
  "Register buffer with BUFFER-NAME with next available numeric ID for TAB-KEY.
Stores the actual buffer OBJECT for reliable identification.
TAB-KEY is the unique tab identifier. Returns the assigned ID or nil if no slots available."
  (unless tab-key
    (setq tab-key (--etm-get-current-tab-key)))

  (let* ((buffer-obj (get-buffer buffer-name))
         (next-id (--etm-numeric-get-next-id tab-key)))
    (when (and next-id buffer-obj)
      (let* ((tab-entry (assoc tab-key etm-numeric-buffers))
             ;; Store buffer OBJECT, not name
             (new-entry (cons next-id buffer-obj)))
        (if tab-entry
            ;; Add to existing tab
            (setcdr tab-entry (cons new-entry (cdr tab-entry)))
          ;; Create new tab entry
          (push (cons tab-key (list new-entry)) etm-numeric-buffers))

        (etm-debug-message 'numeric
                           "Registered: ID=%d buf='%s' obj=#<%s> tab='%s'"
                           next-id buffer-name
                           (prin1-to-string buffer-obj)
                           tab-key)
        next-id))))

(defun --etm-numeric-unregister-buffer
    (id-or-buffer &optional tab-key)
  "Unregister buffer by ID, buffer name, or buffer object from TAB-KEY."
  (unless tab-key
    (setq tab-key (--etm-get-current-tab-key)))

  (let* ((tab-entry (assoc tab-key etm-numeric-buffers))
         (tab-buffers (cdr tab-entry)))
    (when tab-buffers
      (let ((updated-buffers
             (cond
              ;; Remove by ID
              ((numberp id-or-buffer)
               (assq-delete-all id-or-buffer tab-buffers))
              ;; Remove by buffer object (using eq)
              ((bufferp id-or-buffer)
               (cl-remove-if (lambda (entry)
                               (eq (cdr entry) id-or-buffer))
                             tab-buffers))
              ;; Remove by buffer name
              (t
               (cl-remove-if (lambda (entry)
                               (and (buffer-live-p (cdr entry))
                                    (string= (buffer-name (cdr entry))
                                             id-or-buffer)))
                             tab-buffers)))))
        (etm-debug-message 'numeric "Unregistered: %s from tab='%s'"
                           (cond
                            ((numberp id-or-buffer)
                             (format "ID=%d" id-or-buffer))
                            ((bufferp id-or-buffer)
                             (format "obj=#<%s>"
                                     (prin1-to-string id-or-buffer)))
                            (t (format "name='%s'" id-or-buffer)))
                           tab-key)
        (setcdr tab-entry updated-buffers)))))

(defun --etm-numeric-get-buffer-by-id (id &optional tab-key)
  "Get buffer name for ID in TAB-KEY.
Returns the buffer name string, or nil if not found or buffer is dead."
  (unless tab-key
    (setq tab-key (--etm-get-current-tab-key)))

  (let* ((tab-buffers (--etm-numeric-get-tab-buffers tab-key))
         (buffer-obj (cdr (assq id tab-buffers))))
    (when (and buffer-obj (buffer-live-p buffer-obj))
      (buffer-name buffer-obj))))

(defun --etm-numeric-get-buffer-object-by-id (id &optional tab-key)
  "Get buffer OBJECT for ID in TAB-KEY.
Returns the actual buffer object for reliable comparison."
  (unless tab-key
    (setq tab-key (--etm-get-current-tab-key)))

  (let ((tab-buffers (--etm-numeric-get-tab-buffers tab-key)))
    (cdr (assq id tab-buffers))))

(defun --etm-numeric-get-id-by-buffer
    (buffer-or-name &optional tab-key)
  "Get numeric ID for BUFFER-OR-NAME in TAB-KEY.
BUFFER-OR-NAME can be a buffer object or buffer name string."
  (unless tab-key
    (setq tab-key (--etm-get-current-tab-key)))

  (let ((tab-buffers (--etm-numeric-get-tab-buffers tab-key))
        (target-buffer (if (bufferp buffer-or-name)
                           buffer-or-name
                         (get-buffer buffer-or-name))))
    (when target-buffer
      (car (cl-find-if (lambda (entry)
                         (eq (cdr entry) target-buffer))
                       tab-buffers)))))

(defun --etm-numeric-buffer-in-other-tabs-p
    (buffer-obj current-tab-key)
  "Check if BUFFER-OBJ is registered in tabs other than CURRENT-TAB-KEY.
Uses buffer object identity (eq) for reliable comparison."
  (let ((found-in-other nil))
    (dolist (tab-entry etm-numeric-buffers)
      (let ((tab-key (car tab-entry)))
        (unless (string= tab-key current-tab-key)
          (dolist (buf-entry (cdr tab-entry))
            (when (eq (cdr buf-entry) buffer-obj)
              (setq found-in-other tab-key))))))
    found-in-other))

;; Interactive Functions
;; ----------------------------------------

(defun etm-numeric-register-current-buffer ()
  "Register current buffer with next available numeric ID."
  (interactive)
  (let ((buffer-name (buffer-name)))
    (if (--etm-numeric-register-buffer buffer-name)
        (message "Registered current buffer '%s'" buffer-name)
      (message "No available numeric slots"))))

(defun etm-numeric-jump-to-buffer (id)
  "Jump to buffer with numeric ID."
  (interactive "nBuffer ID (1-9): ")
  (let ((buffer-name (--etm-numeric-get-buffer-by-id id)))
    (if buffer-name
        (if (get-buffer buffer-name)
            (switch-to-buffer buffer-name)
          (message "Buffer '%s' no longer exists" buffer-name)
          (--etm-numeric-unregister-buffer id))
      (message
       "No buffer at position %d. Use M-t b r to register current buffer"
       id))))

(defun etm-numeric-list-buffers ()
  "List all numeric buffers for current tab."
  (interactive)
  (let* ((tab-key (--etm-get-current-tab-key))
         (tab-name (alist-get 'name (tab-bar--current-tab)))
         (tab-buffers (--etm-numeric-get-tab-buffers tab-key)))
    (if tab-buffers
        (progn
          (message "Registered buffers in tab '%s':" tab-name)
          (dolist
	      (entry
	       (sort tab-buffers (lambda (a b) (< (car a) (car b)))))
            (let ((buffer-name (cdr entry)))
              (message "  %s%s" buffer-name
                       (if (get-buffer buffer-name) "" " (not found)")))))
      (message
       "No registered buffers in tab '%s'. Use M-t b r to register current buffer"
       tab-name))))

(defun etm-numeric-auto-register-buffer ()
  "Auto-register current buffer if it's not already registered."
  (when (and (buffer-name)
             (not (string-prefix-p " " (buffer-name)))  ; Skip hidden buffers
             (not (minibufferp)))
					; Skip minibuffer
    (let* ((buffer-name (buffer-name))
           (tab-key (--etm-get-current-tab-key))
           (existing-id
	    (--etm-numeric-get-id-by-buffer buffer-name tab-key)))
      (unless existing-id
        (--etm-numeric-register-buffer buffer-name tab-key)))))

(defun etm-numeric-quick-start ()
  "Quick start guide for numeric buffer system."
  (interactive)
  (with-help-window "*ETM Numeric Buffers Help*"
    (princ "ETM Numeric Buffer System - Quick Start Guide\n")
    (princ "============================================\n\n")
    (princ
     "The numeric buffer system allows you to quickly jump to buffers using number keys.\n\n")
    (princ "GETTING STARTED:\n")
    (princ "1. First, register buffers you want quick access to:\n")
    (princ "   - Switch to a buffer you use frequently\n")
    (princ "   - Press M-t b r to register it\n")
    (princ
     "   - The buffer gets assigned the next available number (1-9)\n\n")
    (princ "2. Jump to registered buffers:\n")
    (princ "   - M-t 1 to jump to buffer #1\n")
    (princ "   - M-t 2 to jump to buffer #2\n")
    (princ "   - ... and so on\n\n")
    (princ "KEY BINDINGS:\n")
    (princ "  M-t b r - Register current buffer\n")
    (princ "  M-t b l - List all registered buffers\n")
    (princ "  M-t b 1-9 - Jump to buffer by number\n")
    (princ "  M-t 1-9 - Quick jump to buffer by number\n")
    (princ "  M-t b c - Clean up dead buffer entries\n")
    (princ "  M-t b ? - Show help\n\n")
    (princ "NOTES:\n")
    (princ "- Each tab has its own set of numeric buffers\n")
    (princ
     "- Buffers are NOT automatically registered - you must use M-t b r\n")
    (princ "- Maximum 9 buffers per tab by default\n")))

;; Keybinding Generation
;; ----------------------------------------

(defun etm-numeric-define-keybindings (prefix-key)
  "Define keybindings for numeric buffer jumping.
PREFIX-KEY should be like 'M-t b' - this will create M-t b 1, M-t b 2, etc."
  (dotimes (i etm-numeric-max-buffers)
    (let ((id (1+ i))
          (key-sequence
	   (concat prefix-key " " (number-to-string (1+ i)))))
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
                                  (setq cleaned-count
					(1+ cleaned-count))
                                  t)))
                            tab-buffers)))
        (setcdr tab-entry live-buffers)))
    (etm-debug-message 'numeric "Cleaned up %d dead buffer entries"
		       cleaned-count)))

(defun etm-numeric-clear-tab (tab-key)
  "Clear all numeric buffer entries for TAB-KEY.
TAB-KEY is the unique tab identifier. This should be called when a tab is closed."
  (setq etm-numeric-buffers
        (assoc-delete-all tab-key etm-numeric-buffers)))

;; Hook Integration
;; ----------------------------------------

(defun etm-numeric-setup-hooks ()
  "Setup hooks for automatic buffer management."
  ;; Don't use buffer-list-update-hook as it's too aggressive
  ;; Instead, users should manually register buffers with M-t b r
  ;; or use etm-numeric-register-current-buffer

  ;; Clean up dead buffers periodically
  (run-with-timer 60 60 #'etm-numeric-cleanup-dead-buffers))

;; Home Buffer Jump Functions
;; ----------------------------------------

(defun etm-jump-to-home-1 ()
  "Jump to home buffer 1 (numeric buffer 1)."
  (interactive)
  (etm-numeric-jump-to-buffer 1))

(defun etm-jump-to-home-2 ()
  "Jump to home buffer 2 (numeric buffer 2)."
  (interactive)
  (etm-numeric-jump-to-buffer 2))

(defun etm-jump-to-home-3 ()
  "Jump to home buffer 3 (numeric buffer 3)."
  (interactive)
  (etm-numeric-jump-to-buffer 3))

(defun etm-jump-to-home-4 ()
  "Jump to home buffer 4 (numeric buffer 4)."
  (interactive)
  (etm-numeric-jump-to-buffer 4))

(defun etm-jump-to-home-5 ()
  "Jump to home buffer 5 (numeric buffer 5)."
  (interactive)
  (etm-numeric-jump-to-buffer 5))

(defun etm-jump-to-home-6 ()
  "Jump to home buffer 6 (numeric buffer 6)."
  (interactive)
  (etm-numeric-jump-to-buffer 6))

(defun etm-jump-to-home-7 ()
  "Jump to home buffer 7 (numeric buffer 7)."
  (interactive)
  (etm-numeric-jump-to-buffer 7))

(defun etm-jump-to-home-8 ()
  "Jump to home buffer 8 (numeric buffer 8)."
  (interactive)
  (etm-numeric-jump-to-buffer 8))

(defun etm-jump-to-home-9 ()
  "Jump to home buffer 9 (numeric buffer 9)."
  (interactive)
  (etm-numeric-jump-to-buffer 9))

(provide 'etm-buffer-numeric)

;;; etm-buffer-numeric.el ends here
