;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-25 21:15:39>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-buffer/etm-buffer-ibuffer.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)


;;; Commentary:
;; Ibuffer integration for ETM - organize buffers by tab groups
;; Loads ibuffer on demand when needed

(require 'etm-buffer-auto-track)

;; Lazy loading of ibuffer
;; ----------------------------------------

(defvar etm-ibuffer--loaded nil
  "Non-nil if ibuffer integration has been initialized.")

(defun etm-ibuffer--ensure-loaded ()
  "Ensure ibuffer is loaded and ETM filter is defined."
  (unless etm-ibuffer--loaded
    (require 'ibuffer)
    (etm-ibuffer--define-filter)
    (setq etm-ibuffer--loaded t)))

(defun etm-ibuffer--define-filter ()
  "Define the ETM tab filter for ibuffer."
  (define-ibuffer-filter etm-tab
      "Filter buffers by their associated ETM tab name.
If QUALIFIER is a string, show only buffers from that tab.
If QUALIFIER is nil, show only untracked buffers."
    (:description "ETM tab"
		          :reader (completing-read "Tab name: "
					                       (etm-ibuffer-get-all-tabs)))
    ;; Use etm-ibuffer-get-buffer-tab which checks both systems
    (let ((tab-name (etm-ibuffer-get-buffer-tab buf)))
      (if qualifier
          (string= tab-name qualifier)
        (null tab-name)))))

;; Define column for tab name display

(define-ibuffer-column etm-tab
  (:name "Tab" :inline t)
  (or (etm-ibuffer-get-buffer-tab buffer) "-"))

;; Helper functions
;; ----------------------------------------

(defun etm-ibuffer-get-all-tabs ()
  "Get list of all tab names (from tab-bar + tracked buffers)."
  (let ((tabs '()))
    ;; Get all tabs from tab-bar
    (dolist (tab (tab-bar-tabs))
      (let ((name (alist-get 'name tab)))
        (when (and name (not (member name tabs)))
          (push name tabs))))
    ;; Also include tabs from buffer associations (in case of orphaned buffers)
    (when (hash-table-p etm-buffer-tab-association)
      (maphash (lambda (_buf tab)
                 (unless (member tab tabs)
                   (push tab tabs)))
               etm-buffer-tab-association))
    (sort tabs #'string<)))

(defun etm-ibuffer-get-buffer-tab (buf)
  "Get the ETM tab name for buffer BUF from explicit tracking.
Checks both etm-buffer-tab-association and etm-numeric-buffers.
Uses buffer OBJECT comparison (eq) for reliable identification."
  (let ((buffer-obj (if (bufferp buf) buf (get-buffer buf)))
        (found-tab nil))
    ;; Check etm-buffer-tab-association first (uses buffer names)
    (when (and buffer-obj
               (boundp 'etm-buffer-tab-association)
               (hash-table-p etm-buffer-tab-association))
      (setq found-tab (gethash (buffer-name buffer-obj)
                               etm-buffer-tab-association)))
    ;; Also check etm-numeric-buffers (now stores buffer objects)
    (unless found-tab
      (when (and buffer-obj (boundp 'etm-numeric-buffers))
        (dolist (tab-entry etm-numeric-buffers)
          (let ((tab-key (car tab-entry))
                (buffers (cdr tab-entry)))
            (dolist (buf-entry buffers)
              ;; Compare buffer OBJECTS with eq
              (when (eq (cdr buf-entry) buffer-obj)
                (setq found-tab tab-key)))))))
    found-tab))

;; Generate filter groups
;; ----------------------------------------

(defun etm-ibuffer-generate-filter-groups ()
  "Generate ibuffer filter groups based on ETM tabs.
Returns a list suitable for `ibuffer-filter-groups'."
  (let ((groups '())
        (tabs (etm-ibuffer-get-all-tabs)))
    ;; Add a group for each tab
    (dolist (tab tabs)
      (push (list (concat "[" tab "]")
                  `(etm-tab . ,tab))
            groups))
    ;; Add untracked group at the end
    (push '("Untracked" (etm-tab . nil)) groups)
    ;; Return in correct order
    (nreverse groups)))

(defun etm-ibuffer-generate-hybrid-filter-groups ()
  "Generate hybrid filter groups: ETM tabs + standard mode-based groups."
  (let ((groups '())
        (tabs (etm-ibuffer-get-all-tabs)))
    ;; Add ETM tab groups first
    (dolist (tab tabs)
      (push (list (concat "[" tab "]")
                  `(etm-tab . ,tab))
            groups))
    ;; Standard groups for untracked buffers
    (push '("Terminal (untracked)"
            (and (or (mode . vterm-mode)
                     (mode . term-mode)
                     (mode . shell-mode)
                     (mode . eshell-mode))
                 (etm-tab . nil)))
          groups)
    (push '("Dired (untracked)"
            (and (mode . dired-mode)
                 (etm-tab . nil)))
          groups)
    (push '("Emacs"
            (or (name . "^\\*scratch\\*$")
                (name . "^\\*Messages\\*$")
                (name . "^\\*Help\\*$")))
          groups)
    (push '("Other" (etm-tab . nil)) groups)
    (nreverse groups)))

;; Interactive commands (main API)
;; ----------------------------------------

;;;###autoload

(defun etm-ibuffer ()
  "Open ibuffer with ETM tab-based grouping."
  (interactive)
  (etm-ibuffer--ensure-loaded)
  (let ((tabs (etm-ibuffer-get-all-tabs)))
    (if (null tabs)
        (progn
          (ibuffer)
          (message "No ETM-tracked buffers. Using standard ibuffer."))
      (ibuffer)
      (setq ibuffer-filter-groups (etm-ibuffer-generate-filter-groups))
      (ibuffer-update nil t)
      (message "Ibuffer grouped by ETM tabs (%d tabs)" (length tabs)))))

;;;###autoload

(defun etm-ibuffer-hybrid ()
  "Open ibuffer with hybrid grouping: ETM tabs + mode-based fallback."
  (interactive)
  (etm-ibuffer--ensure-loaded)
  (ibuffer)
  (setq ibuffer-filter-groups
	    (etm-ibuffer-generate-hybrid-filter-groups))
  (ibuffer-update nil t)
  (message "Ibuffer: hybrid ETM/mode grouping"))

;;;###autoload

(defun etm-ibuffer-refresh-groups ()
  "Refresh ibuffer filter groups based on current ETM tracking."
  (interactive)
  (etm-ibuffer--ensure-loaded)
  (when (eq major-mode 'ibuffer-mode)
    (setq ibuffer-filter-groups (etm-ibuffer-generate-filter-groups))
    (ibuffer-update nil t)))

;;;###autoload

(defun etm-ibuffer-show-tab (tab-name)
  "Show only buffers from TAB-NAME in ibuffer."
  (interactive
   (list (completing-read "Show buffers from tab: "
                          (etm-ibuffer-get-all-tabs))))
  (etm-ibuffer--ensure-loaded)
  (ibuffer)
  (ibuffer-filter-by-etm-tab tab-name))

;;; etm-buffer-ibuffer.el ends here


(provide 'etm-buffer-ibuffer)

(when
    (not load-file-name)
  (message "etm-buffer-ibuffer.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))