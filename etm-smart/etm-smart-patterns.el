;;; etm-smart-patterns.el --- Pattern tracking for ETM smart suggestions -*- coding: utf-8; lexical-binding: t -*-

;; Author: ywatanabe
;; Date: 2025-05-26
;; Version: 1.0.0

;;; Commentary:
;; This module tracks buffer switching patterns to enable intelligent
;; suggestions. It monitors user behavior, builds statistical models,
;; and provides pattern data for the suggestion engine.

;;; Code:

(require 'cl-lib)
(require 'etm-core-variables)

;;; Variables

(defvar etm-smart-patterns (make-hash-table :test 'equal)
  "Hash table storing switching patterns per tab.
Keys are tab names, values are lists of pattern structures.")

(defvar etm-smart-max-patterns-per-tab 100
  "Maximum number of patterns to store per tab.")

(defvar etm-smart-max-timestamps 10
  "Maximum number of timestamps to keep per pattern.")

(defvar etm-smart-blacklist-patterns '("\\*password" "\\*private" "\\*secret")
  "List of regexps for buffer names that should never be tracked.")

(defvar etm-smart-min-pattern-count 2
  "Minimum occurrences before a pattern is considered significant.")

;;; Data Structures

(cl-defstruct etm-smart-pattern
  "Structure representing a buffer switching pattern."
  from-buffer      ; Source buffer name
  to-buffer        ; Destination buffer name
  count            ; Number of occurrences
  timestamps       ; List of recent timestamps
  context          ; Context information
  score)           ; Calculated relevance score

(cl-defstruct etm-smart-context
  "Structure representing the context of a buffer switch."
  project-root     ; Current project root directory
  major-mode       ; Major mode of source buffer
  time-of-day      ; Morning/afternoon/evening/night
  day-of-week      ; Weekday/weekend
  remote-host)     ; Remote host if applicable

;;; Core Functions

(defun etm-smart-make-pattern (&rest args)
  "Create a new pattern structure with ARGS."
  (apply #'make-etm-smart-pattern args))

(defun etm-smart-track-switch (from-buffer to-buffer)
  "Track a buffer switch from FROM-BUFFER to TO-BUFFER."
  (cl-block etm-smart-track-switch
    ;; Check blacklist
    (when (or (etm-smart--buffer-blacklisted-p from-buffer)
              (etm-smart--buffer-blacklisted-p to-buffer))
      (cl-return-from etm-smart-track-switch))
    
    ;; Don't track self-switches
    (when (equal from-buffer to-buffer)
      (cl-return-from etm-smart-track-switch))
  
  (let* ((tab-name (etm-smart--current-tab-name))
         (patterns (gethash tab-name etm-smart-patterns))
         (existing (etm-smart--find-pattern patterns from-buffer to-buffer))
         (context (etm-smart--capture-context)))
    
    (if existing
        ;; Update existing pattern
        (progn
          (cl-incf (etm-smart-pattern-count existing))
          (etm-smart--add-timestamp existing))
      ;; Create new pattern
      (let ((new-pattern (make-etm-smart-pattern
                          :from-buffer from-buffer
                          :to-buffer to-buffer
                          :count 1
                          :timestamps (list (current-time))
                          :context context
                          :score 0.0)))
        (push new-pattern patterns)))
    
    ;; Update patterns in hash table
    (puthash tab-name (etm-smart--prune-patterns patterns) etm-smart-patterns))))

(defun etm-smart-get-patterns (buffer &optional context)
  "Get switching patterns for BUFFER with optional CONTEXT filtering."
  (let* ((tab-name (etm-smart--current-tab-name))
         (all-patterns (gethash tab-name etm-smart-patterns))
         (relevant-patterns (cl-remove-if-not
                             (lambda (p)
                               (equal (etm-smart-pattern-from-buffer p) buffer))
                             all-patterns)))
    
    ;; Filter by context if provided
    (when context
      (setq relevant-patterns
            (cl-remove-if-not
             (lambda (p)
               (etm-smart--context-matches-p 
                (etm-smart-pattern-context p) context))
             relevant-patterns)))
    
    ;; Sort by score
    (cl-sort relevant-patterns #'> :key #'etm-smart-pattern-score)))

(defun etm-smart-calculate-scores (patterns current-context)
  "Calculate relevance scores for PATTERNS given CURRENT-CONTEXT."
  (dolist (pattern patterns)
    (let* ((frequency-score (etm-smart--calculate-frequency-score pattern))
           (recency-score (etm-smart--calculate-recency-score pattern))
           (context-score (if current-context
                              (etm-smart--calculate-context-score 
                               pattern current-context)
                            0.5))
           ;; Weighted combination
           (total-score (+ (* 0.4 frequency-score)
                           (* 0.4 recency-score)
                           (* 0.2 context-score))))
      (setf (etm-smart-pattern-score pattern) total-score)))
  patterns)

;;; Helper Functions

(defun etm-smart--current-tab-name ()
  "Get the current tab name."
  (if (fboundp 'tab-bar--current-tab-name)
      (tab-bar--current-tab-name)
    "default"))

(defun etm-smart--buffer-blacklisted-p (buffer-name)
  "Check if BUFFER-NAME matches any blacklist pattern."
  (when buffer-name
    (cl-some (lambda (pattern)
               (string-match-p pattern buffer-name))
             etm-smart-blacklist-patterns)))

(defun etm-smart--find-pattern (patterns from to)
  "Find pattern in PATTERNS matching FROM and TO buffers."
  (cl-find-if (lambda (p)
                (and (equal (etm-smart-pattern-from-buffer p) from)
                     (equal (etm-smart-pattern-to-buffer p) to)))
              patterns))

(defun etm-smart--add-timestamp (pattern)
  "Add current timestamp to PATTERN, maintaining limit."
  (let ((timestamps (etm-smart-pattern-timestamps pattern)))
    (push (current-time) timestamps)
    ;; Keep only recent timestamps
    (when (> (length timestamps) etm-smart-max-timestamps)
      (setf (etm-smart-pattern-timestamps pattern)
            (seq-take timestamps etm-smart-max-timestamps)))))

(defun etm-smart--capture-context ()
  "Capture current context information."
  (make-etm-smart-context
   :project-root (when (fboundp 'project-current)
                   (when-let ((proj (project-current)))
                     (if (consp proj)
                         (cdr proj)
                       proj)))
   :major-mode (when (and (boundp 'major-mode) 
                          (symbolp major-mode))
                 major-mode)
   :time-of-day (etm-smart--time-of-day)
   :day-of-week (etm-smart--day-of-week)
   :remote-host (when (and buffer-file-name
                           (file-remote-p buffer-file-name))
                  (file-remote-p buffer-file-name 'host))))

(defun etm-smart--time-of-day ()
  "Get current time of day category."
  (let ((hour (string-to-number (format-time-string "%H"))))
    (cond ((< hour 6) 'night)
          ((< hour 12) 'morning)
          ((< hour 18) 'afternoon)
          (t 'evening))))

(defun etm-smart--day-of-week ()
  "Get current day category."
  (let ((dow (string-to-number (format-time-string "%u"))))
    (if (<= dow 5) 'weekday 'weekend)))

(defun etm-smart--prune-patterns (patterns)
  "Prune PATTERNS list to maintain size limits."
  (if (<= (length patterns) etm-smart-max-patterns-per-tab)
      patterns
    ;; Sort by score and keep top patterns
    (seq-take (cl-sort patterns #'> :key #'etm-smart-pattern-score)
              etm-smart-max-patterns-per-tab)))

(defun etm-smart--context-matches-p (context1 context2)
  "Check if CONTEXT1 matches CONTEXT2."
  (and (or (null (etm-smart-context-project-root context1))
           (null (etm-smart-context-project-root context2))
           (equal (etm-smart-context-project-root context1)
                  (etm-smart-context-project-root context2)))
       (or (null (etm-smart-context-major-mode context1))
           (null (etm-smart-context-major-mode context2))
           (eq (etm-smart-context-major-mode context1)
               (etm-smart-context-major-mode context2)))
       (or (null (etm-smart-context-remote-host context1))
           (null (etm-smart-context-remote-host context2))
           (equal (etm-smart-context-remote-host context1)
                  (etm-smart-context-remote-host context2)))))

(defun etm-smart--calculate-frequency-score (pattern)
  "Calculate frequency-based score for PATTERN."
  (let ((count (etm-smart-pattern-count pattern)))
    ;; Logarithmic scaling to prevent domination
    (min 1.0 (/ (log (1+ count)) (log 50)))))

(defun etm-smart--calculate-recency-score (pattern)
  "Calculate recency-based score for PATTERN."
  (let* ((timestamps (etm-smart-pattern-timestamps pattern))
         (latest (car timestamps)))
    (if latest
        (let* ((age-seconds (float-time (time-subtract (current-time) latest)))
               (age-days (/ age-seconds 86400)))
          ;; Exponential decay over days - stronger decay for older items
          (exp (- (* 0.5 age-days))))
      0.0)))

(defun etm-smart--calculate-context-score (pattern current-context)
  "Calculate context match score between PATTERN and CURRENT-CONTEXT."
  (let ((pattern-context (etm-smart-pattern-context pattern))
        (score 0.0)
        (factors 0))
    
    ;; Project match
    (when (and (etm-smart-context-project-root pattern-context)
               (etm-smart-context-project-root current-context))
      (cl-incf factors)
      (when (equal (etm-smart-context-project-root pattern-context)
                   (etm-smart-context-project-root current-context))
        (cl-incf score 1.0)))
    
    ;; Major mode match
    (when (and (etm-smart-context-major-mode pattern-context)
               (etm-smart-context-major-mode current-context))
      (cl-incf factors)
      (when (eq (etm-smart-context-major-mode pattern-context)
                (etm-smart-context-major-mode current-context))
        (cl-incf score 1.0)))
    
    ;; Time of day match
    (when (etm-smart-context-time-of-day pattern-context)
      (cl-incf factors 0.5)
      (when (eq (etm-smart-context-time-of-day pattern-context)
                (etm-smart-context-time-of-day current-context))
        (cl-incf score 0.5)))
    
    ;; Remote host match
    (when (and (etm-smart-context-remote-host pattern-context)
               (etm-smart-context-remote-host current-context))
      (cl-incf factors)
      (when (equal (etm-smart-context-remote-host pattern-context)
                   (etm-smart-context-remote-host current-context))
        (cl-incf score 1.0)))
    
    (if (> factors 0)
        (/ score factors)
      0.5)))

;;; Public API

(defun etm-smart-clear-patterns (&optional tab-name)
  "Clear patterns for TAB-NAME or current tab."
  (interactive)
  (let ((tab (or tab-name (etm-smart--current-tab-name))))
    (remhash tab etm-smart-patterns)
    (message "Cleared patterns for tab: %s" tab)))

(defun etm-smart-pattern-stats ()
  "Display statistics about tracked patterns."
  (interactive)
  (let ((total-patterns 0)
        (total-switches 0))
    (maphash (lambda (_tab patterns)
               (cl-incf total-patterns (length patterns))
               (dolist (p patterns)
                 (cl-incf total-switches (etm-smart-pattern-count p))))
             etm-smart-patterns)
    (message "ETM Smart: %d patterns tracking %d switches across %d tabs"
             total-patterns total-switches (hash-table-count etm-smart-patterns))))

(provide 'etm-smart-patterns)
;;; etm-smart-patterns.el ends here