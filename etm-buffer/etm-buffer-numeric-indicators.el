;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-25 10:00:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-buffer/etm-buffer-numeric-indicators.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;; Visual indicators for numeric buffer system
;; Shows occupied numeric slots in tab-bar and current buffer's slot in mode-line

(require 'etm-core-variables)
(require 'etm-buffer-numeric)

;; Core Functions
;; ----------------------------------------

(defun etm-numeric-get-occupied-slots (tab-name)
  "Get sorted list of occupied numeric slots for TAB-NAME."
  (let* ((tab-buffers (--etm-numeric-get-tab-buffers tab-name))
         (slots (mapcar #'car tab-buffers)))
    (sort slots #'<)))

(defun etm-numeric-format-indicator (slots)
  "Format SLOTS list into indicator string."
  (if (or (null slots) 
          (not etm-numeric-indicators-enabled))
      ""
    (let ((formatted (--etm-numeric-format-slot-ranges slots)))
      (format etm-numeric-indicator-format formatted))))

(defun --etm-numeric-format-slot-ranges (slots)
  "Format SLOTS into compact range notation."
  (if (null slots)
      ""
    ;; When separator is not space, don't use ranges
    (if (not (string= etm-numeric-indicator-separator " "))
        (mapconcat #'number-to-string slots etm-numeric-indicator-separator)
      ;; Use range notation only with space separator
      (let ((ranges '())
            (start (car slots))
            (end (car slots))
            (rest (cdr slots)))
        ;; Build ranges
        (while rest
          (if (= (car rest) (1+ end))
              ;; Continue range
              (setq end (car rest))
            ;; End range and start new
            (push (if (< (- end start) 2) ; Only use range for 3+ consecutive
                      (if (= start end)
                          (number-to-string start)
                        (format "%d %d" start end))
                    (format "%d-%d" start end))
                  ranges)
            (setq start (car rest)
                  end (car rest)))
          (setq rest (cdr rest)))
        ;; Add final range
        (push (if (< (- end start) 2) ; Only use range for 3+ consecutive
                  (if (= start end)
                      (number-to-string start)
                    (format "%d %d" start end))
                (format "%d-%d" start end))
              ranges)
        ;; Join with separator
        (mapconcat #'identity (nreverse ranges) etm-numeric-indicator-separator)))))

(defun etm-numeric-get-buffer-slot (buffer-name tab-name)
  "Get numeric slot for BUFFER-NAME in TAB-NAME, or nil if not assigned."
  (let ((tab-buffers (--etm-numeric-get-tab-buffers tab-name)))
    (car (rassoc buffer-name tab-buffers))))

(defun etm-numeric-set-buffer-id (buffer-name id tab-name)
  "Set BUFFER-NAME to specific numeric ID in TAB-NAME."
  (let* ((tab-entry (assoc tab-name etm-numeric-buffers))
         (tab-buffers (cdr tab-entry))
         (new-entry (cons id buffer-name)))
    ;; Remove any existing entry for this buffer
    (setq tab-buffers (cl-remove buffer-name tab-buffers :key #'cdr :test #'equal))
    ;; Remove any existing entry for this ID
    (setq tab-buffers (cl-remove id tab-buffers :key #'car))
    ;; Add new entry
    (push new-entry tab-buffers)
    ;; Update or create tab entry
    (if tab-entry
        (setcdr tab-entry tab-buffers)
      (push (cons tab-name tab-buffers) etm-numeric-buffers))
    ;; Refresh indicators
    (etm-numeric-refresh-indicators)
    id))

(defun etm-numeric-unregister-buffer (buffer-name tab-name)
  "Remove BUFFER-NAME from numeric slots in TAB-NAME."
  (let* ((tab-entry (assoc tab-name etm-numeric-buffers))
         (tab-buffers (cdr tab-entry)))
    (when tab-entry
      (setcdr tab-entry 
              (cl-remove buffer-name tab-buffers :key #'cdr :test #'equal))
      (etm-numeric-refresh-indicators))))

;; Tab-bar Integration
;; ----------------------------------------

(defun etm-numeric-format-tab-name (tab-name)
  "Format TAB-NAME with numeric indicators."
  (if (not etm-numeric-indicators-enabled)
      tab-name
    (let* ((slots (etm-numeric-get-occupied-slots tab-name))
           (indicator (etm-numeric-format-indicator slots)))
      (if (string-empty-p indicator)
          tab-name
        (format "%s %s" tab-name indicator)))))

;; Mode-line Integration
;; ----------------------------------------

(defun etm-numeric-mode-line-indicator (buffer-name tab-name)
  "Get mode-line indicator for BUFFER-NAME in TAB-NAME."
  (if (not etm-numeric-indicators-enabled)
      ""
    (let ((slot (etm-numeric-get-buffer-slot buffer-name tab-name)))
      (if slot
          (format " ETM[%d]" slot)
        ""))))

;; Update Functions
;; ----------------------------------------

(defun etm-numeric-refresh-indicators ()
  "Refresh all numeric indicators in tab-bar and mode-line."
  (when etm-numeric-indicators-enabled
    ;; Update tab-bar
    (when (fboundp 'tab-bar-rename-tab)
      (let ((current-tab (alist-get 'name (tab-bar--current-tab))))
        (tab-bar-rename-tab (etm-numeric-format-tab-name current-tab))))
    ;; Force mode-line update
    (force-mode-line-update t)))

;; Hook numeric buffer changes to update indicators
(advice-add '--etm-numeric-register-buffer :after
            (lambda (&rest _) (etm-numeric-refresh-indicators)))

(provide 'etm-buffer-numeric-indicators)