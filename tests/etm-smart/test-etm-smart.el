;;; -*- coding: utf-8; lexical-binding: t -*-
;;; test-etm-smart.el --- Tests for ETM Smart Suggestions main module
;;; Author: ywatanabe
;;; Time-stamp: <2025-01-26 04:16:00 ywatanabe>
;;; Commentary:
;;; Tests for the main Smart Suggestions integration module.

;;; Code:

(require 'ert)
(require 'etm-test-utils)

;; Load the module under test
(require 'etm-smart)

;;; Test Initialization

(ert-deftest test-etm-smart-init ()
  "Test Smart Suggestions initialization."
  (with-etm-test-environment
    ;; Reset state
    (setq etm-smart-initialized nil
          etm-smart-enabled nil)
    
    ;; Initialize
    (etm-smart-init)
    
    ;; Check initialization
    (should etm-smart-initialized)
    (should-not etm-smart-enabled)))

(ert-deftest test-etm-smart-toggle ()
  "Test toggling Smart Suggestions on/off."
  (with-etm-test-environment
    (etm-smart-init)
    
    ;; Initially disabled
    (should-not etm-smart-enabled)
    
    ;; Toggle on
    (etm-smart-toggle)
    (should etm-smart-enabled)
    
    ;; Toggle off
    (etm-smart-toggle)
    (should-not etm-smart-enabled)))

;;; Test Hook Integration

(ert-deftest test-etm-smart-buffer-switch-tracking ()
  "Test that buffer switches are tracked when enabled."
  (with-etm-test-environment
    (etm-smart-init)
    (etm-smart-toggle) ; Enable
    
    ;; Create test buffers
    (let ((buf1 (get-buffer-create "test-buffer-1"))
          (buf2 (get-buffer-create "test-buffer-2")))
      
      ;; Switch to buf1
      (switch-to-buffer buf1)
      
      ;; Mock the hook callback
      (let ((etm-smart-enabled t))
        (with-current-buffer buf2
          (etm-smart--on-buffer-switch)))
      
      ;; Should have tracked the switch
      (let ((patterns (gethash (etm-core-get-current-tab-id) etm-smart--patterns)))
        (should patterns)))))

;;; Test Commands

(ert-deftest test-etm-smart-clear-patterns ()
  "Test clearing patterns for current tab."
  (with-etm-test-environment
    (etm-smart-init)
    
    ;; Add some test patterns
    (let ((tab-id (etm-core-get-current-tab-id)))
      (puthash tab-id (make-hash-table :test 'equal) etm-smart--patterns)
      
      ;; Mock yes-or-no-p
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (etm-smart-clear-patterns))
      
      ;; Patterns should be cleared
      (should-not (gethash tab-id etm-smart--patterns)))))

(ert-deftest test-etm-smart-reset-scores ()
  "Test resetting scores for patterns."
  (with-etm-test-environment
    (etm-smart-init)
    
    ;; Create test pattern
    (let* ((tab-id (etm-core-get-current-tab-id))
           (patterns (make-hash-table :test 'equal))
           (pattern (make-etm-smart-pattern
                     :from-buffer "buf1"
                     :to-buffer "buf2"
                     :count 5
                     :timestamps '(1234567890)
                     :context nil
                     :score 10.0)))
      
      (puthash "buf1->buf2" pattern patterns)
      (puthash tab-id patterns etm-smart--patterns)
      
      ;; Mock yes-or-no-p
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (etm-smart-reset-scores))
      
      ;; Score should be reset
      (should (= 0.0 (etm-smart-pattern-score pattern))))))

;;; Test Storage

(ert-deftest test-etm-smart-save-load-data ()
  "Test saving and loading Smart Suggestions data."
  (with-etm-test-environment
    (etm-smart-init)
    
    ;; Create test data
    (let ((tab-id (etm-core-get-current-tab-id))
          (test-file (make-temp-file "etm-smart-test-")))
      
      ;; Set up test pattern
      (etm-smart-track-switch "buffer-1" "buffer-2")
      
      ;; Save to test file
      (let ((etm-smart-storage-file test-file))
        (etm-smart-save-data)
        
        ;; Clear patterns
        (setq etm-smart--patterns (make-hash-table :test 'equal))
        
        ;; Load back
        (etm-smart-load-data)
        
        ;; Should have patterns restored
        (let ((patterns (gethash tab-id etm-smart--patterns)))
          (should patterns)))
      
      ;; Clean up
      (delete-file test-file))))

;;; Test Mode

(ert-deftest test-etm-smart-mode ()
  "Test ETM Smart minor mode."
  (with-etm-test-environment
    ;; Mode should be off initially
    (should-not etm-smart-mode)
    (should-not etm-smart-enabled)
    
    ;; Enable mode
    (etm-smart-mode 1)
    (should etm-smart-mode)
    (should etm-smart-enabled)
    
    ;; Disable mode
    (etm-smart-mode -1)
    (should-not etm-smart-mode)
    (should-not etm-smart-enabled)))

;;; Test Auto-save

(ert-deftest test-etm-smart-auto-save ()
  "Test auto-save functionality."
  (with-etm-test-environment
    (etm-smart-init)
    
    ;; Timer should be started
    (should etm-smart-auto-save-timer)
    (should (timerp etm-smart-auto-save-timer))
    
    ;; Shutdown should stop timer
    (etm-smart-shutdown)
    (should-not etm-smart-auto-save-timer)))

(provide 'test-etm-smart)
;;; test-etm-smart.el ends here