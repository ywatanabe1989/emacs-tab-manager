;;; test-etm-groups-navigation.el --- Tests for ETM buffer groups navigation -*- coding: utf-8; lexical-binding: t -*-
;; Author: ywatanabe
;; Time-stamp: <2025-01-25 15:43:00 (ywatanabe)>
;;; Commentary:
;; Tests for navigating between buffers within groups
;;; Code:

(require 'ert)

;; Test helper functions
(defun test-etm-groups-nav-setup ()
  "Set up test environment for group navigation."
  (when (boundp 'etm-tab-buffer-groups)
    (clrhash etm-tab-buffer-groups))
  ;; Create test buffers
  (dolist (name '("main.el" "test.el" "utils.el" "README.md"))
    (with-current-buffer (get-buffer-create name)
      (setq buffer-file-name (expand-file-name name)))))

(defun test-etm-groups-nav-teardown ()
  "Clean up test environment."
  ;; Kill test buffers
  (dolist (name '("main.el" "test.el" "utils.el" "README.md"))
    (when (get-buffer name)
      (kill-buffer name)))
  (when (boundp 'etm-tab-buffer-groups)
    (clrhash etm-tab-buffer-groups)))

;; Navigation tests
(ert-deftest test-etm-groups-next-buffer-in-group ()
  "Test navigating to next buffer in group."
  (test-etm-groups-nav-setup)
  (unwind-protect
      (progn
        (require 'etm-groups-navigation)
        ;; Create group with buffers
        (etm-groups-create "project-foo")
        (etm-groups-add-buffer "project-foo" "main.el")
        (etm-groups-add-buffer "project-foo" "test.el")
        (etm-groups-add-buffer "project-foo" "utils.el")
        
        ;; Start in main.el
        (switch-to-buffer "main.el")
        ;; Navigate to next
        (etm-groups-next-buffer "project-foo")
        (should (string= (buffer-name) "test.el"))
        ;; Navigate again
        (etm-groups-next-buffer "project-foo")
        (should (string= (buffer-name) "utils.el"))
        ;; Should wrap around
        (etm-groups-next-buffer "project-foo")
        (should (string= (buffer-name) "main.el")))
    (test-etm-groups-nav-teardown)))

(ert-deftest test-etm-groups-previous-buffer-in-group ()
  "Test navigating to previous buffer in group."
  (test-etm-groups-nav-setup)
  (unwind-protect
      (progn
        (require 'etm-groups-navigation)
        ;; Create group with buffers
        (etm-groups-create "project-foo")
        (etm-groups-add-buffer "project-foo" "main.el")
        (etm-groups-add-buffer "project-foo" "test.el")
        (etm-groups-add-buffer "project-foo" "utils.el")
        
        ;; Start in main.el
        (switch-to-buffer "main.el")
        ;; Navigate to previous (should wrap to end)
        (etm-groups-previous-buffer "project-foo")
        (should (string= (buffer-name) "utils.el"))
        ;; Navigate again
        (etm-groups-previous-buffer "project-foo")
        (should (string= (buffer-name) "test.el")))
    (test-etm-groups-nav-teardown)))

(ert-deftest test-etm-groups-switch-to-group ()
  "Test switching to first buffer in a group."
  (test-etm-groups-nav-setup)
  (unwind-protect
      (progn
        (require 'etm-groups-navigation)
        ;; Create groups
        (etm-groups-create "project-foo")
        (etm-groups-add-buffer "project-foo" "main.el")
        (etm-groups-add-buffer "project-foo" "test.el")
        
        (etm-groups-create "docs")
        (etm-groups-add-buffer "docs" "README.md")
        
        ;; Start somewhere else
        (switch-to-buffer "*scratch*")
        
        ;; Switch to project group
        (etm-groups-switch-to-group "project-foo")
        (should (member (buffer-name) '("main.el" "test.el")))
        
        ;; Switch to docs group
        (etm-groups-switch-to-group "docs")
        (should (string= (buffer-name) "README.md")))
    (test-etm-groups-nav-teardown)))

(ert-deftest test-etm-groups-cycle-groups ()
  "Test cycling through different groups."
  (test-etm-groups-nav-setup)
  (unwind-protect
      (progn
        (require 'etm-groups-navigation)
        ;; Create multiple groups
        (etm-groups-create "project-a")
        (etm-groups-add-buffer "project-a" "main.el")
        
        (etm-groups-create "project-b")
        (etm-groups-add-buffer "project-b" "test.el")
        
        (etm-groups-create "docs")
        (etm-groups-add-buffer "docs" "README.md")
        
        ;; Start in project-a
        (switch-to-buffer "main.el")
        
        ;; Cycle to next group
        (etm-groups-cycle-next)
        (should (string= (buffer-name) "test.el"))
        
        ;; Cycle again
        (etm-groups-cycle-next)
        (should (string= (buffer-name) "README.md"))
        
        ;; Should wrap around
        (etm-groups-cycle-next)
        (should (string= (buffer-name) "main.el")))
    (test-etm-groups-nav-teardown)))

(ert-deftest test-etm-groups-navigation-with-killed-buffers ()
  "Test navigation handles killed buffers gracefully."
  (test-etm-groups-nav-setup)
  (unwind-protect
      (progn
        (require 'etm-groups-navigation)
        ;; Create group with buffers
        (etm-groups-create "project-foo")
        (etm-groups-add-buffer "project-foo" "main.el")
        (etm-groups-add-buffer "project-foo" "test.el")
        (etm-groups-add-buffer "project-foo" "utils.el")
        
        ;; Start in main.el
        (switch-to-buffer "main.el")
        
        ;; Kill test.el
        (kill-buffer "test.el")
        
        ;; Navigation should skip killed buffer
        (etm-groups-next-buffer "project-foo")
        (should (string= (buffer-name) "utils.el")))
    (test-etm-groups-nav-teardown)))

(provide 'test-etm-groups-navigation)
;;; test-etm-groups-navigation.el ends here