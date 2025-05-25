;;; validate-smart-suggestions.el --- Basic validation for Smart Suggestions -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Time-stamp: <2025-01-26 04:50:00 ywatanabe>

;;; Commentary:
;; This script performs basic validation of Smart Suggestions functionality.
;; Run with: emacs -Q -l validate-smart-suggestions.el

;;; Code:

;; Add paths
(add-to-list 'load-path (expand-file-name "../.." (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../etm-core" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../etm-smart" (file-name-directory load-file-name)))

;; Load required modules
(require 'etm-core)
(require 'etm-smart)

(defvar etm-smart-validation-results nil
  "Results of validation tests.")

(defun etm-smart-validate-record (test result &optional message)
  "Record validation TEST RESULT with optional MESSAGE."
  (push (list test result message) etm-smart-validation-results))

(defun etm-smart-validate-basic-functionality ()
  "Validate basic Smart Suggestions functionality."
  
  ;; Test 1: Module loading
  (condition-case err
      (progn
        (require 'etm-smart-patterns)
        (require 'etm-smart-suggest)
        (require 'etm-smart-ui)
        (require 'etm-smart)
        (etm-smart-validate-record "Module Loading" 'pass))
    (error
     (etm-smart-validate-record "Module Loading" 'fail (error-message-string err))))
  
  ;; Test 2: Data structures
  (condition-case err
      (progn
        (unless (hash-table-p etm-smart-patterns)
          (error "etm-smart-patterns is not a hash table"))
        (etm-smart-validate-record "Data Structures" 'pass))
    (error
     (etm-smart-validate-record "Data Structures" 'fail (error-message-string err))))
  
  ;; Test 3: Pattern tracking
  (condition-case err
      (let ((test-tab "test-validation"))
        ;; Mock the tab name
        (cl-letf (((symbol-function 'etm-smart--current-tab-name)
                   (lambda () test-tab)))
          
          ;; Track some patterns
          (etm-smart-track-switch "buffer-a" "buffer-b")
          (etm-smart-track-switch "buffer-a" "buffer-b")
          (etm-smart-track-switch "buffer-a" "buffer-c")
          
          ;; Check if patterns were recorded
          (let ((tab-patterns (gethash test-tab etm-smart-patterns)))
            (unless tab-patterns
              (error "No patterns recorded for test tab"))
            
            (let ((pattern-count 0))
              (maphash (lambda (_key _val) (cl-incf pattern-count)) tab-patterns)
              (unless (>= pattern-count 2)
                (error "Expected at least 2 patterns, got %d" pattern-count))))
          
          (etm-smart-validate-record "Pattern Tracking" 'pass)))
    (error
     (etm-smart-validate-record "Pattern Tracking" 'fail (error-message-string err))))
  
  ;; Test 4: Scoring
  (condition-case err
      (let ((test-tab "test-validation"))
        (cl-letf (((symbol-function 'etm-smart--current-tab-name)
                   (lambda () test-tab)))
          
          ;; Get patterns and calculate scores
          (let* ((patterns (etm-smart-get-patterns "buffer-a"))
                 (context (etm-smart--capture-context)))
            
            (unless patterns
              (error "No patterns found for buffer-a"))
            
            (etm-smart-calculate-scores patterns context)
            
            ;; Check if scores were calculated
            (dolist (pattern patterns)
              (unless (numberp (etm-smart-pattern-score pattern))
                (error "Pattern score is not a number")))
            
            (etm-smart-validate-record "Scoring" 'pass))))
    (error
     (etm-smart-validate-record "Scoring" 'fail (error-message-string err))))
  
  ;; Test 5: Suggestions
  (condition-case err
      (let ((test-tab "test-validation"))
        (cl-letf (((symbol-function 'etm-smart--current-tab-name)
                   (lambda () test-tab))
                  ((symbol-function 'current-buffer)
                   (lambda () (get-buffer-create "buffer-a")))
                  ((symbol-function 'buffer-list)
                   (lambda () (list (get-buffer-create "buffer-a")
                                    (get-buffer-create "buffer-b")
                                    (get-buffer-create "buffer-c")))))
          
          ;; Get suggestions
          (let ((suggestions (etm-smart-suggest-buffers 3)))
            (unless (listp suggestions)
              (error "Suggestions is not a list"))
            
            ;; Should have at least one suggestion
            (when (> (length suggestions) 0)
              (let ((first (car suggestions)))
                (unless (and (consp first)
                             (stringp (car first))
                             (numberp (cdr first)))
                  (error "Invalid suggestion format"))))
            
            (etm-smart-validate-record "Suggestions" 'pass))))
    (error
     (etm-smart-validate-record "Suggestions" 'fail (error-message-string err))))
  
  ;; Test 6: Commands
  (condition-case err
      (progn
        ;; Check if commands are defined
        (unless (commandp 'etm-smart-mode)
          (error "etm-smart-mode is not a command"))
        (unless (commandp 'etm-smart-toggle)
          (error "etm-smart-toggle is not a command"))
        (unless (commandp 'etm-smart-switch-to-suggested)
          (error "etm-smart-switch-to-suggested is not a command"))
        
        (etm-smart-validate-record "Commands" 'pass))
    (error
     (etm-smart-validate-record "Commands" 'fail (error-message-string err))))
  
  ;; Clean up test buffers
  (dolist (buf '("buffer-a" "buffer-b" "buffer-c"))
    (when (get-buffer buf)
      (kill-buffer buf))))

(defun etm-smart-validate-report ()
  "Generate validation report."
  (let ((pass-count 0)
        (fail-count 0))
    
    (message "\n=== ETM Smart Suggestions Validation Report ===\n")
    
    (dolist (result (reverse etm-smart-validation-results))
      (let ((test (nth 0 result))
            (status (nth 1 result))
            (message (nth 2 result)))
        
        (if (eq status 'pass)
            (progn
              (cl-incf pass-count)
              (message "[PASS] %s" test))
          (cl-incf fail-count)
          (message "[FAIL] %s: %s" test (or message "Unknown error")))))
    
    (message "\n=== Summary ===")
    (message "Total tests: %d" (+ pass-count fail-count))
    (message "Passed: %d" pass-count)
    (message "Failed: %d" fail-count)
    (message "Success rate: %.1f%%\n" 
             (* 100.0 (/ (float pass-count) (+ pass-count fail-count))))
    
    (if (= fail-count 0)
        (message "✅ All validation tests passed!")
      (message "❌ Some validation tests failed. Please check the errors above."))))

;; Run validation
(etm-smart-validate-basic-functionality)
(etm-smart-validate-report)

(provide 'validate-smart-suggestions)
;;; validate-smart-suggestions.el ends here