;;; etm-smart-suggest.el --- Suggestion engine for ETM smart suggestions -*- coding: utf-8; lexical-binding: t -*-

;; Author: ywatanabe
;; Date: 2025-05-26
;; Version: 1.0.0

;;; Commentary:
;; This module provides the suggestion engine for ETM smart suggestions.
;; It analyzes tracked patterns and generates intelligent buffer suggestions
;; based on context, frequency, and recency.

;;; Code:

(require 'cl-lib)
(require 'etm-smart-patterns)

;;; Variables

(defvar etm-smart-last-suggestions nil
  "Cache of last generated suggestions for annotation.")

(defvar etm-smart-min-confidence 0.3
  "Minimum confidence score for suggestions.")

(defvar etm-smart-max-suggestions 5
  "Maximum number of suggestions to display.")

(defvar etm-smart-fallback-to-recent t
  "Whether to fall back to recent buffers when no patterns exist.")

(defvar etm-smart-show-scores t
  "Whether to show confidence scores in suggestions.")

;;; Core Functions

(defun etm-smart-suggest-buffers (&optional count)
  "Suggest COUNT buffers based on current context.
Returns a list of (buffer-name . score) pairs."
  (let* ((count (or count etm-smart-max-suggestions))
         (current-buf (buffer-name (current-buffer)))
         (context (etm-smart--capture-context))
         (patterns (etm-smart-get-patterns current-buf context))
         (suggestions '()))
    
    ;; Calculate scores for patterns
    (when patterns
      (etm-smart-calculate-scores patterns context))
    
    ;; Convert patterns to suggestions
    (dolist (pattern patterns)
      (when (>= (etm-smart-pattern-score pattern) etm-smart-min-confidence)
        (let ((to-buffer (etm-smart-pattern-to-buffer pattern)))
          ;; Check if buffer exists and isn't current
          (when (and (get-buffer to-buffer)
                     (not (equal to-buffer current-buf)))
            (push (cons to-buffer (etm-smart-pattern-score pattern))
                  suggestions)))))
    
    ;; Fall back to recent buffers if needed
    (when (and etm-smart-fallback-to-recent
               (< (length suggestions) count))
      (setq suggestions (etm-smart--add-fallback-suggestions 
                         suggestions current-buf count)))
    
    ;; Sort by score and limit count
    (setq suggestions (seq-take (cl-sort suggestions #'> :key #'cdr) count))
    
    ;; Cache for annotations
    (setq etm-smart-last-suggestions suggestions)
    
    suggestions))

(defun etm-smart-score-suggestion (pattern current-context)
  "Calculate suggestion score for PATTERN in CURRENT-CONTEXT."
  (let* ((frequency-score (etm-smart--calculate-frequency-score pattern))
         (recency-score (etm-smart--calculate-recency-score pattern))
         (context-score (if current-context
                            (etm-smart--calculate-context-score 
                             pattern current-context)
                          0.5)))
    ;; Weighted combination (reusing pattern scoring logic)
    (+ (* 0.4 frequency-score)
       (* 0.4 recency-score)
       (* 0.2 context-score))))

(defun etm-smart-filter-suggestions (suggestions)
  "Filter and sort SUGGESTIONS by relevance."
  (cl-remove-if (lambda (suggestion)
                  (< (cdr suggestion) etm-smart-min-confidence))
                suggestions))

;;; Fallback Functions

(defun etm-smart--add-fallback-suggestions (existing current-buf max-count)
  "Add fallback suggestions to EXISTING list.
Avoids CURRENT-BUF and respects MAX-COUNT."
  (let ((all-buffers (buffer-list))
        (added 0)
        (existing-names (mapcar #'car existing)))
    
    ;; Add recent buffers not in existing suggestions
    (dolist (buf all-buffers)
      (when (>= (+ (length existing) added) max-count)
        (cl-return))
      
      (let ((buf-name (buffer-name buf)))
        (when (and buf-name
                   (not (equal buf-name current-buf))
                   (not (member buf-name existing-names))
                   (not (string-prefix-p " " buf-name)) ; Skip hidden buffers
                   (not (etm-smart--buffer-blacklisted-p buf-name)))
          (push (cons buf-name 0.1) existing) ; Low confidence for fallback
          (cl-incf added))))
    
    existing))

;;; Completion Enhancement

(defun etm-smart-completing-read (prompt)
  "Enhanced completing-read with smart suggestions.
PROMPT is the prompt string for completion."
  (let* ((suggestions (etm-smart-suggest-buffers))
         (all-buffers (mapcar #'buffer-name (buffer-list)))
         (sorted-buffers (etm-smart--sort-buffers-by-suggestion 
                          all-buffers suggestions)))
    
    (completing-read prompt sorted-buffers nil t)))

(defun etm-smart--sort-buffers-by-suggestion (buffers suggestions)
  "Sort BUFFERS list based on SUGGESTIONS scores."
  (let ((suggestion-alist suggestions))
    (cl-sort buffers
             (lambda (a b)
               (let ((score-a (or (cdr (assoc a suggestion-alist)) 0))
                     (score-b (or (cdr (assoc b suggestion-alist)) 0)))
                 (> score-a score-b))))))

(defun etm-smart-annotate-completion (candidate)
  "Add annotation to completion CANDIDATE with suggestion metadata."
  (when etm-smart-show-scores
    (let ((suggestion (assoc candidate etm-smart-last-suggestions)))
      (when suggestion
        (format " [%d%%]" (round (* 100 (cdr suggestion))))))))

;;; Quick Switch Functions

(defun etm-smart-quick-switch (number)
  "Quickly switch to suggestion NUMBER (1-based)."
  (interactive "p")
  (let ((suggestions (etm-smart-suggest-buffers)))
    (when (and suggestions
               (> number 0)
               (<= number (length suggestions)))
      (let ((buffer-name (car (nth (1- number) suggestions))))
        (switch-to-buffer buffer-name)
        (message "Switched to %s (suggestion #%d)" buffer-name number)))))

;;; Buffer Display

(defun etm-smart-suggest-show-buffer ()
  "Show suggestions in a dedicated buffer."
  (interactive)
  (let ((suggestions (etm-smart-suggest-buffers))
        (buf (get-buffer-create "*ETM Smart Suggestions*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "ETM Smart Suggestions\n")
        (insert "=====================\n\n")
        
        (if suggestions
            (cl-loop for (buffer . score) in suggestions
                     for i from 1
                     do (insert (format "%d. %s (score: %.1f)\n"
                                        i buffer score)))
          (insert "No suggestions available\n"))
        
        (insert "\nPress 1-9 to switch to a suggestion\n")
        (insert "Press q to quit\n"))
      (goto-char (point-min))
      (setq buffer-read-only t)
      (use-local-map etm-smart-ui-suggestion-keymap))
    (switch-to-buffer buf)))

;;; Learning and Feedback

(defun etm-smart-learn-from-feedback (suggestion accepted-p)
  "Update patterns based on whether SUGGESTION was ACCEPTED-P."
  ;; Track the switch if accepted
  (when accepted-p
    (etm-smart-track-switch (buffer-name (current-buffer)) 
                            (car suggestion)))
  
  ;; Could implement negative feedback in the future
  ;; For now, just tracking positive signals
  )

;;; Mode Line Support

(defun etm-smart-mode-line-indicator ()
  "Generate mode line indicator showing suggestion availability."
  (if etm-smart-show-mode-line
      (let ((suggestions (etm-smart-suggest-buffers 1)))
        (if suggestions
            (format " →%s" (truncate-string-to-width 
                            (caar suggestions) 
                            10 nil nil "…"))
          ""))
    ""))

(defvar etm-smart-show-mode-line nil
  "Whether to show smart suggestions in mode line.")

;;; Interactive Commands

(defun etm-smart-show-suggestions ()
  "Display suggestions in a dedicated buffer."
  (interactive)
  (let ((suggestions (etm-smart-suggest-buffers))
        (source-buffer-name (buffer-name))
        (buffer (get-buffer-create "*ETM Suggestions*")))
    
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "ETM Smart Suggestions\n")
        (insert "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
        (insert (format "Based on your current buffer: %s\n\n" 
                        source-buffer-name))
        
        (if suggestions
            (cl-loop for suggestion in suggestions
                     for i from 1
                     do (insert (format "%d. %-30s [%d%%] %s\n"
                                        i
                                        (car suggestion)
                                        (round (* 100 (cdr suggestion)))
                                        (etm-smart--get-suggestion-reason suggestion))))
          (insert "No suggestions available yet.\n"))
        
        (insert "\nPress 1-5 for quick switch, q to quit\n"))
      
      (etm-smart-suggestions-mode)
      (goto-char (point-min)))
    
    (display-buffer buffer)))

(defun etm-smart--get-suggestion-reason (suggestion)
  "Get human-readable reason for SUGGESTION."
  ;; This could be enhanced to provide more specific reasons
  (cond ((>= (cdr suggestion) 0.8) "- Frequently used together")
        ((>= (cdr suggestion) 0.6) "- Often follows current buffer")
        ((>= (cdr suggestion) 0.4) "- Sometimes related")
        ((>= (cdr suggestion) 0.2) "- Occasionally used")
        (t "- Recent buffer")))

;;; Suggestion Buffer Mode

(define-derived-mode etm-smart-suggestions-mode special-mode "ETM-Suggestions"
  "Major mode for ETM smart suggestions buffer."
  (setq buffer-read-only t)
  (define-key etm-smart-suggestions-mode-map "1" 
    (lambda () (interactive) (etm-smart-quick-switch 1)))
  (define-key etm-smart-suggestions-mode-map "2" 
    (lambda () (interactive) (etm-smart-quick-switch 2)))
  (define-key etm-smart-suggestions-mode-map "3" 
    (lambda () (interactive) (etm-smart-quick-switch 3)))
  (define-key etm-smart-suggestions-mode-map "4" 
    (lambda () (interactive) (etm-smart-quick-switch 4)))
  (define-key etm-smart-suggestions-mode-map "5" 
    (lambda () (interactive) (etm-smart-quick-switch 5)))
  (define-key etm-smart-suggestions-mode-map "q" 'quit-window)
  (define-key etm-smart-suggestions-mode-map "g" 'etm-smart-show-suggestions))

(provide 'etm-smart-suggest)
;;; etm-smart-suggest.el ends here