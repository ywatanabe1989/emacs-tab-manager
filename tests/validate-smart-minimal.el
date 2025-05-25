;;; validate-smart-minimal.el --- Minimal validation for Smart Suggestions -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Time-stamp: <2025-01-26 04:52:00 ywatanabe>

;;; Commentary:
;; Minimal validation that tests Smart Suggestions in isolation.

;;; Code:

;; Add paths
(add-to-list 'load-path (expand-file-name "../.." (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../etm-smart" (file-name-directory load-file-name)))

;; Mock minimal ETM core functions needed
(defun etm-core-get-current-tab-id ()
  "Mock function for testing."
  "test-tab")

;; Load Smart modules directly
(require 'etm-smart-patterns)
(require 'etm-smart-suggest)

(message "\n=== Smart Suggestions Minimal Validation ===\n")

;; Test 1: Basic pattern tracking
(message "Test 1: Pattern Tracking...")
(let ((result "FAIL"))
  (condition-case err
      (progn
        ;; Initialize if needed
        (unless (bound-and-true-p etm-smart-patterns)
          (setq etm-smart-patterns (make-hash-table :test 'equal)))
        
        ;; Track a pattern
        (etm-smart-track-switch "test-buffer-1" "test-buffer-2")
        (etm-smart-track-switch "test-buffer-1" "test-buffer-2")
        
        ;; Verify pattern was tracked
        (let* ((patterns (gethash (etm-core-get-current-tab-id) etm-smart-patterns)))
          (if patterns
              (setq result "PASS")
            (error "No patterns found"))))
    (error
     (setq result (format "FAIL: %s" (error-message-string err)))))
  (message "  Pattern Tracking: %s" result))

;; Test 2: Pattern retrieval
(message "\nTest 2: Pattern Retrieval...")
(let ((result "FAIL"))
  (condition-case err
      (progn
        ;; Get patterns for a buffer
        (let ((patterns (etm-smart-get-patterns "test-buffer-1")))
          (if (and patterns (listp patterns))
              (setq result "PASS")
            (error "Pattern retrieval failed"))))
    (error
     (setq result (format "FAIL: %s" (error-message-string err)))))
  (message "  Pattern Retrieval: %s" result))

;; Test 3: Scoring
(message "\nTest 3: Pattern Scoring...")
(let ((result "FAIL"))
  (condition-case err
      (progn
        ;; Get and score patterns
        (let* ((patterns (etm-smart-get-patterns "test-buffer-1"))
               (context (etm-smart--capture-context)))
          (when patterns
            (etm-smart-calculate-scores patterns context)
            ;; Check if first pattern has a score
            (let ((first-pattern (car patterns)))
              (if (and first-pattern 
                       (numberp (etm-smart-pattern-score first-pattern)))
                  (setq result "PASS")
                (error "Scoring failed"))))))
    (error
     (setq result (format "FAIL: %s" (error-message-string err)))))
  (message "  Pattern Scoring: %s" result))

;; Test 4: Suggestions
(message "\nTest 4: Suggestion Generation...")
(let ((result "FAIL"))
  (condition-case err
      (progn
        ;; Mock buffer functions
        (cl-letf (((symbol-function 'current-buffer)
                   (lambda () (get-buffer-create "test-buffer-1")))
                  ((symbol-function 'buffer-list)
                   (lambda () (list (get-buffer-create "test-buffer-1")
                                    (get-buffer-create "test-buffer-2")))))
          
          ;; Get suggestions
          (let ((suggestions (etm-smart-suggest-buffers 5)))
            (if (listp suggestions)
                (setq result "PASS")
              (error "Suggestions not a list")))))
    (error
     (setq result (format "FAIL: %s" (error-message-string err)))))
  (message "  Suggestion Generation: %s" result))

;; Test 5: Blacklist
(message "\nTest 5: Privacy Blacklist...")
(let ((result "FAIL"))
  (condition-case err
      (progn
        ;; Try to track a blacklisted buffer
        (etm-smart-track-switch "normal-buffer" "*scratch*")
        
        ;; Check it wasn't tracked
        (let* ((patterns (gethash (etm-core-get-current-tab-id) etm-smart-patterns))
               (found-blacklisted nil))
          (when patterns
            (maphash (lambda (_key pattern)
                       (when (or (string-match-p "\\*scratch\\*" 
                                                  (etm-smart-pattern-from-buffer pattern))
                                 (string-match-p "\\*scratch\\*" 
                                                  (etm-smart-pattern-to-buffer pattern)))
                         (setq found-blacklisted t)))
                     patterns))
          (if found-blacklisted
              (error "Blacklisted buffer was tracked")
            (setq result "PASS"))))
    (error
     (setq result (format "FAIL: %s" (error-message-string err)))))
  (message "  Privacy Blacklist: %s" result))

(message "\n=== Validation Complete ===\n")

;; Clean up
(when (get-buffer "test-buffer-1") (kill-buffer "test-buffer-1"))
(when (get-buffer "test-buffer-2") (kill-buffer "test-buffer-2"))

(provide 'validate-smart-minimal)
;;; validate-smart-minimal.el ends here