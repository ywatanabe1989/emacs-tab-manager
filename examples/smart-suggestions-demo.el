;;; smart-suggestions-demo.el --- Demo script for ETM Smart Suggestions -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Time-stamp: <2025-01-26 04:45:00 ywatanabe>

;;; Commentary:
;; This file demonstrates how to use ETM Smart Suggestions feature.
;; Load this file and follow the instructions to see Smart Suggestions in action.

;;; Code:

(require 'etm)
(require 'etm-smart)

;;; Demo Setup

(defun etm-smart-demo-setup ()
  "Set up a demo environment for Smart Suggestions."
  (interactive)
  
  ;; Initialize ETM if not already done
  (unless (bound-and-true-p tab-bar-mode)
    (etm-init))
  
  ;; Create a demo tab
  (tab-bar-new-tab)
  (tab-bar-rename-tab "Smart Demo")
  
  ;; Create some demo buffers
  (let ((demo-files '(("demo-main.el" . ";;; Main file for demo\n\n(defun demo-main ()\n  \"Main function.\"\n  (message \"Hello from main!\"))\n")
                      ("demo-test.el" . ";;; Test file for demo\n\n(ert-deftest test-demo-main ()\n  \"Test main function.\"\n  (should (demo-main)))\n")
                      ("demo-utils.el" . ";;; Utilities for demo\n\n(defun demo-helper ()\n  \"Helper function.\"\n  (message \"Helper called\"))\n")
                      ("demo-config.el" . ";;; Configuration for demo\n\n(defvar demo-config-var \"value\"\n  \"Demo configuration.\")\n")
                      ("README.md" . "# Demo Project\n\nThis is a demo for Smart Suggestions.\n"))))
    
    (dolist (file-spec demo-files)
      (let ((name (car file-spec))
            (content (cdr file-spec)))
        (with-current-buffer (get-buffer-create name)
          (erase-buffer)
          (insert content)
          (set-buffer-modified-p nil)))))
  
  (message "Demo setup complete! Now try the demo scenarios."))

;;; Demo Scenarios

(defun etm-smart-demo-scenario-1 ()
  "Demo Scenario 1: Learn code-test pattern."
  (interactive)
  
  ;; Enable Smart Suggestions
  (unless etm-smart-enabled
    (etm-smart-mode 1))
  
  (message "=== Scenario 1: Code-Test Pattern ===")
  (message "Watch as we switch between main and test files...")
  
  ;; Simulate working pattern: main -> test -> main -> test
  (switch-to-buffer "demo-main.el")
  (sit-for 1)
  
  (switch-to-buffer "demo-test.el")
  (sit-for 1)
  
  (switch-to-buffer "demo-main.el")
  (sit-for 1)
  
  (switch-to-buffer "demo-test.el")
  (sit-for 1)
  
  (switch-to-buffer "demo-main.el")
  (message "Pattern learned! Now try: C-x t S s")
  (message "Smart Suggestions should recommend demo-test.el"))

(defun etm-smart-demo-scenario-2 ()
  "Demo Scenario 2: Learn config editing pattern."
  (interactive)
  
  (message "=== Scenario 2: Config Editing Pattern ===")
  (message "Simulating config file editing workflow...")
  
  ;; Pattern: utils -> config -> utils -> config
  (switch-to-buffer "demo-utils.el")
  (sit-for 1)
  
  (switch-to-buffer "demo-config.el")
  (sit-for 1)
  
  (switch-to-buffer "demo-utils.el")
  (sit-for 1)
  
  (switch-to-buffer "demo-config.el")
  (sit-for 1)
  
  (switch-to-buffer "demo-utils.el")
  (message "Pattern learned! Smart Suggestions should now recommend demo-config.el"))

(defun etm-smart-demo-scenario-3 ()
  "Demo Scenario 3: Show all suggestions."
  (interactive)
  
  (message "=== Scenario 3: View All Suggestions ===")
  
  ;; Create more patterns
  (switch-to-buffer "README.md")
  (sit-for 0.5)
  (switch-to-buffer "demo-main.el")
  (sit-for 0.5)
  (switch-to-buffer "README.md")
  (sit-for 0.5)
  (switch-to-buffer "demo-config.el")
  (sit-for 0.5)
  
  (message "Multiple patterns created. Now showing all suggestions...")
  (sit-for 1)
  
  ;; Show suggestions buffer
  (etm-smart-show-suggestions)
  (message "You should see a buffer with ranked suggestions!"))

(defun etm-smart-demo-check-patterns ()
  "Check what patterns have been learned."
  (interactive)
  
  (let* ((tab-id (etm-core-get-current-tab-id))
         (patterns (gethash tab-id etm-smart-patterns)))
    (if patterns
        (progn
          (message "=== Learned Patterns ===")
          (maphash
           (lambda (key pattern)
             (message "Pattern: %s -> %s (count: %d, score: %.2f)"
                      (etm-smart-pattern-from-buffer pattern)
                      (etm-smart-pattern-to-buffer pattern)
                      (etm-smart-pattern-count pattern)
                      (etm-smart-pattern-score pattern)))
           patterns))
      (message "No patterns learned yet. Run demo scenarios first!"))))

(defun etm-smart-demo-test-overlay ()
  "Test the overlay display feature."
  (interactive)
  
  (message "=== Testing Overlay Display ===")
  
  ;; Ensure overlay is enabled
  (setq etm-smart-ui-use-overlay t)
  (setq etm-smart-ui-overlay-delay 0.5)
  
  (switch-to-buffer "demo-main.el")
  (message "Wait 0.5 seconds to see suggestion overlay...")
  
  ;; Trigger overlay
  (etm-smart-ui-show-overlay))

;;; Interactive Demo Menu

(defun etm-smart-demo-menu ()
  "Show interactive demo menu."
  (interactive)
  
  (let ((choice (read-char-choice
                 (concat "ETM Smart Suggestions Demo Menu:\n"
                         "------------------------\n"
                         "1. Setup demo environment\n"
                         "2. Run Scenario 1 (Code-Test Pattern)\n"
                         "3. Run Scenario 2 (Config Pattern)\n"
                         "4. Run Scenario 3 (Show All Suggestions)\n"
                         "5. Check learned patterns\n"
                         "6. Test overlay display\n"
                         "7. Clear all patterns\n"
                         "8. Toggle Smart Suggestions\n"
                         "q. Quit\n"
                         "\nChoose: ")
                 '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?q))))
    
    (pcase choice
      (?1 (etm-smart-demo-setup))
      (?2 (etm-smart-demo-scenario-1))
      (?3 (etm-smart-demo-scenario-2))
      (?4 (etm-smart-demo-scenario-3))
      (?5 (etm-smart-demo-check-patterns))
      (?6 (etm-smart-demo-test-overlay))
      (?7 (when (yes-or-no-p "Clear all patterns? ")
            (etm-smart-clear-patterns)
            (message "Patterns cleared!")))
      (?8 (etm-smart-toggle)
          (message "Smart Suggestions %s" 
                   (if etm-smart-enabled "enabled" "disabled")))
      (?q (message "Demo ended")))
    
    (unless (eq choice ?q)
      (when (yes-or-no-p "Continue with demo? ")
        (etm-smart-demo-menu)))))

;;; Demo Configuration

(defun etm-smart-demo-configure ()
  "Configure Smart Suggestions for optimal demo experience."
  (interactive)
  
  ;; Lower thresholds for demo
  (setq etm-smart-min-pattern-count 1)      ; Show suggestions after 1 occurrence
  (setq etm-smart-min-confidence 0.1)       ; Low threshold for demo
  (setq etm-smart-max-suggestions 5)        ; Show up to 5 suggestions
  (setq etm-smart-decay-factor 0.99)        ; Slow decay for demo
  
  ;; Enable UI features
  (setq etm-smart-ui-use-overlay t)
  (setq etm-smart-show-scores t)
  (setq etm-smart-show-mode-line t)
  
  (message "Smart Suggestions configured for demo!"))

;;; Entry Point

;;;###autoload
(defun etm-smart-demo ()
  "Start the ETM Smart Suggestions interactive demo."
  (interactive)
  
  (message "Welcome to ETM Smart Suggestions Demo!")
  (message "This demo will show you how Smart Suggestions learns and helps you.")
  (sit-for 2)
  
  ;; Configure for demo
  (etm-smart-demo-configure)
  
  ;; Start menu
  (etm-smart-demo-menu))

(provide 'smart-suggestions-demo)
;;; smart-suggestions-demo.el ends here