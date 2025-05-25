;;; test-etm-smart-patterns.el --- Tests for ETM smart pattern tracking -*- coding: utf-8; lexical-binding: t -*-

;; Author: ywatanabe
;; Date: 2025-05-26
;; Version: 1.0.0

;;; Commentary:
;; Test suite for ETM smart pattern tracking functionality.
;; Tests pattern recording, scoring, and retrieval.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load paths for test environment
(add-to-list 'load-path (expand-file-name "../.." (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../etm-core" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../etm-smart" (file-name-directory load-file-name)))

;; Test helpers
(defvar test-etm-smart-patterns nil
  "Test pattern storage.")

(defun test-etm-smart-patterns-setup ()
  "Set up test environment."
  (setq test-etm-smart-patterns (make-hash-table :test 'equal))
  ;; Mock the global patterns variable
  (setq etm-smart-patterns test-etm-smart-patterns))

(defun test-etm-smart-patterns-teardown ()
  "Tear down test environment."
  (setq test-etm-smart-patterns nil)
  (setq etm-smart-patterns nil))

;; Tests

(ert-deftest test-etm-smart-pattern-structure ()
  "Test that pattern structures are created correctly."
  (test-etm-smart-patterns-setup)
  (unwind-protect
      (progn
        (require 'etm-smart-patterns)
        
        ;; Test creating a pattern
        (let ((pattern (etm-smart-make-pattern
                        :from-buffer "main.el"
                        :to-buffer "test-main.el"
                        :count 1
                        :timestamps (list (current-time))
                        :context nil
                        :score 0.5)))
          
          ;; Verify structure
          (should (etm-smart-pattern-p pattern))
          (should (equal (etm-smart-pattern-from-buffer pattern) "main.el"))
          (should (equal (etm-smart-pattern-to-buffer pattern) "test-main.el"))
          (should (= (etm-smart-pattern-count pattern) 1))
          (should (= (etm-smart-pattern-score pattern) 0.5))))
    (test-etm-smart-patterns-teardown)))

(ert-deftest test-etm-smart-track-switch ()
  "Test tracking buffer switches."
  (test-etm-smart-patterns-setup)
  (unwind-protect
      (progn
        (require 'etm-smart-patterns)
        
        ;; Mock current tab
        (cl-letf (((symbol-function 'tab-bar--current-tab-name)
                   (lambda () "test-tab")))
          
          ;; Track a switch
          (etm-smart-track-switch "buffer-a" "buffer-b")
          
          ;; Verify pattern was recorded
          (let ((patterns (gethash "test-tab" etm-smart-patterns)))
            (should patterns)
            (should (= (length patterns) 1))
            
            (let ((pattern (car patterns)))
              (should (equal (etm-smart-pattern-from-buffer pattern) "buffer-a"))
              (should (equal (etm-smart-pattern-to-buffer pattern) "buffer-b"))
              (should (= (etm-smart-pattern-count pattern) 1))))
          
          ;; Track same switch again
          (etm-smart-track-switch "buffer-a" "buffer-b")
          
          ;; Verify count increased
          (let ((patterns (gethash "test-tab" etm-smart-patterns)))
            (should (= (length patterns) 1))
            (let ((pattern (car patterns)))
              (should (= (etm-smart-pattern-count pattern) 2))))))
    (test-etm-smart-patterns-teardown)))

(ert-deftest test-etm-smart-get-patterns ()
  "Test retrieving patterns for a buffer."
  (test-etm-smart-patterns-setup)
  (unwind-protect
      (progn
        (require 'etm-smart-patterns)
        
        ;; Mock current tab
        (cl-letf (((symbol-function 'tab-bar--current-tab-name)
                   (lambda () "test-tab")))
          
          ;; Track multiple switches
          (etm-smart-track-switch "main.el" "test.el")
          (etm-smart-track-switch "main.el" "utils.el")
          (etm-smart-track-switch "main.el" "test.el") ; repeat
          (etm-smart-track-switch "test.el" "main.el")
          
          ;; Get patterns from main.el
          (let ((patterns (etm-smart-get-patterns "main.el")))
            (should (= (length patterns) 2))
            
            ;; Find test.el pattern
            (let ((test-pattern (cl-find "test.el" patterns
                                         :key #'etm-smart-pattern-to-buffer
                                         :test #'equal)))
              (should test-pattern)
              (should (= (etm-smart-pattern-count test-pattern) 2))))
          
          ;; Get patterns from test.el
          (let ((patterns (etm-smart-get-patterns "test.el")))
            (should (= (length patterns) 1))
            (should (equal (etm-smart-pattern-to-buffer (car patterns)) "main.el")))))
    (test-etm-smart-patterns-teardown)))

(ert-deftest test-etm-smart-pattern-scoring ()
  "Test pattern scoring algorithm."
  (test-etm-smart-patterns-setup)
  (unwind-protect
      (progn
        (require 'etm-smart-patterns)
        
        ;; Create patterns with different characteristics
        (let* ((now (current-time))
               (hour-ago (time-subtract now (seconds-to-time 3600)))
               (day-ago (time-subtract now (seconds-to-time 86400)))
               
               ;; High frequency, recent
               (pattern1 (etm-smart-make-pattern
                          :from-buffer "a" :to-buffer "b"
                          :count 10
                          :timestamps (list now (time-subtract now (seconds-to-time 60)))
                          :score 0))
               
               ;; Low frequency, recent
               (pattern2 (etm-smart-make-pattern
                          :from-buffer "a" :to-buffer "c"
                          :count 2
                          :timestamps (list now)
                          :score 0))
               
               ;; High frequency, old
               (pattern3 (etm-smart-make-pattern
                          :from-buffer "a" :to-buffer "d"
                          :count 15
                          :timestamps (list day-ago)
                          :score 0)))
          
          ;; Calculate scores
          (etm-smart-calculate-scores (list pattern1 pattern2 pattern3) nil)
          
          ;; Pattern1 should have highest score (high frequency + recent)
          (should (> (etm-smart-pattern-score pattern1)
                     (etm-smart-pattern-score pattern2)))
          (should (> (etm-smart-pattern-score pattern1)
                     (etm-smart-pattern-score pattern3)))
          
          ;; Pattern3 should score higher than pattern2 despite age
          ;; (much higher frequency)
          (should (> (etm-smart-pattern-score pattern3)
                     (etm-smart-pattern-score pattern2)))))
    (test-etm-smart-patterns-teardown)))

(ert-deftest test-etm-smart-pattern-context ()
  "Test context-aware pattern tracking."
  (test-etm-smart-patterns-setup)
  (unwind-protect
      (progn
        (require 'etm-smart-patterns)
        
        ;; Mock current tab and context
        (cl-letf (((symbol-function 'tab-bar--current-tab-name)
                   (lambda () "test-tab"))
                  ((symbol-function 'project-current)
                   (lambda (&optional _) '(vc . "~/project"))))
          
          ;; Set major-mode as a variable
          (let ((major-mode 'emacs-lisp-mode))
          
            ;; Track switch with context
            (etm-smart-track-switch "main.el" "test.el")
            
            ;; Verify context was recorded
            (let* ((patterns (gethash "test-tab" etm-smart-patterns))
                   (pattern (car patterns))
                   (context (etm-smart-pattern-context pattern)))
              (should context)
              (should (equal (etm-smart-context-project-root context) "~/project"))
              (should (eq (etm-smart-context-major-mode context) 'emacs-lisp-mode))))))
    (test-etm-smart-patterns-teardown)))

(ert-deftest test-etm-smart-pattern-limits ()
  "Test pattern storage limits and pruning."
  (test-etm-smart-patterns-setup)
  (unwind-protect
      (progn
        (require 'etm-smart-patterns)
        
        ;; Mock current tab
        (cl-letf (((symbol-function 'tab-bar--current-tab-name)
                   (lambda () "test-tab")))
          
          ;; Set a low limit for testing
          (let ((etm-smart-max-patterns-per-tab 5))
            
            ;; Track more patterns than the limit
            (dotimes (i 10)
              (etm-smart-track-switch 
               (format "buffer-%d" i)
               (format "buffer-%d" (1+ i))))
            
            ;; Verify we don't exceed the limit
            (let ((patterns (gethash "test-tab" etm-smart-patterns)))
              (should (<= (length patterns) etm-smart-max-patterns-per-tab))))))
    (test-etm-smart-patterns-teardown)))

(ert-deftest test-etm-smart-pattern-privacy ()
  "Test privacy features for pattern tracking."
  (test-etm-smart-patterns-setup)
  (unwind-protect
      (progn
        (require 'etm-smart-patterns)
        
        ;; Mock current tab
        (cl-letf (((symbol-function 'tab-bar--current-tab-name)
                   (lambda () "test-tab")))
          
          ;; Set blacklist patterns (matching implementation format)
          (let ((etm-smart-blacklist-patterns '("\\*password" "\\*private")))
            
            ;; Try to track blacklisted buffer
            (etm-smart-track-switch "main.el" "*password-store*")
            (etm-smart-track-switch "main.el" "*private-notes*")
            
            ;; These should not be tracked
            (let ((patterns (gethash "test-tab" etm-smart-patterns)))
              (should-not patterns))
            
            ;; Normal buffer should be tracked
            (etm-smart-track-switch "main.el" "test.el")
            (let ((patterns (gethash "test-tab" etm-smart-patterns)))
              (should (= (length patterns) 1))))))
    (test-etm-smart-patterns-teardown)))

;; Run tests if executed directly
;; (when (and (boundp 'load-file-name) load-file-name)
;;   (ert-run-tests-batch-and-exit))

(provide 'test-etm-smart-patterns)
;;; test-etm-smart-patterns.el ends here