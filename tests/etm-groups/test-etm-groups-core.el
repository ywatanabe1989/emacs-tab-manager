;;; test-etm-groups-core.el --- Tests for ETM buffer groups core functionality -*- coding: utf-8; lexical-binding: t -*-
;; Author: ywatanabe
;; Time-stamp: <2025-01-25 15:41:00 (ywatanabe)>
;;; Commentary:
;; Tests for core buffer grouping functionality
;;; Code:

(require 'ert)

;; Test helper functions
(defun test-etm-groups-setup ()
  "Set up test environment for buffer groups."
  (when (boundp 'etm-tab-buffer-groups)
    (clrhash etm-tab-buffer-groups)))

(defun test-etm-groups-teardown ()
  "Clean up test environment."
  (when (boundp 'etm-tab-buffer-groups)
    (clrhash etm-tab-buffer-groups)))

;; Core functionality tests
(ert-deftest test-etm-groups-create-group ()
  "Test creating a new buffer group."
  (test-etm-groups-setup)
  (unwind-protect
      (progn
        (require 'etm-groups-core)
        ;; Create a group
        (etm-groups-create "project-foo")
        ;; Verify group exists
        (should (etm-groups-exists-p "project-foo"))
        ;; Verify group is empty
        (should (null (etm-groups-get-buffers "project-foo"))))
    (test-etm-groups-teardown)))

(ert-deftest test-etm-groups-add-buffer-to-group ()
  "Test adding buffers to a group."
  (test-etm-groups-setup)
  (unwind-protect
      (progn
        (require 'etm-groups-core)
        ;; Create group and add buffers
        (etm-groups-create "project-foo")
        (etm-groups-add-buffer "project-foo" "main.el")
        (etm-groups-add-buffer "project-foo" "test.el")
        ;; Verify buffers are in group
        (let ((buffers (etm-groups-get-buffers "project-foo")))
          (should (member "main.el" buffers))
          (should (member "test.el" buffers))
          (should (= 2 (length buffers)))))
    (test-etm-groups-teardown)))

(ert-deftest test-etm-groups-remove-buffer-from-group ()
  "Test removing buffers from a group."
  (test-etm-groups-setup)
  (unwind-protect
      (progn
        (require 'etm-groups-core)
        ;; Create group with buffers
        (etm-groups-create "project-foo")
        (etm-groups-add-buffer "project-foo" "main.el")
        (etm-groups-add-buffer "project-foo" "test.el")
        ;; Remove one buffer
        (etm-groups-remove-buffer "project-foo" "main.el")
        ;; Verify removal
        (let ((buffers (etm-groups-get-buffers "project-foo")))
          (should-not (member "main.el" buffers))
          (should (member "test.el" buffers))
          (should (= 1 (length buffers)))))
    (test-etm-groups-teardown)))

(ert-deftest test-etm-groups-delete-group ()
  "Test deleting a group."
  (test-etm-groups-setup)
  (unwind-protect
      (progn
        (require 'etm-groups-core)
        ;; Create and delete group
        (etm-groups-create "project-foo")
        (etm-groups-add-buffer "project-foo" "main.el")
        (should (etm-groups-exists-p "project-foo"))
        (etm-groups-delete "project-foo")
        ;; Verify deletion
        (should-not (etm-groups-exists-p "project-foo")))
    (test-etm-groups-teardown)))

(ert-deftest test-etm-groups-list-all ()
  "Test listing all groups in current tab."
  (test-etm-groups-setup)
  (unwind-protect
      (progn
        (require 'etm-groups-core)
        ;; Create multiple groups
        (etm-groups-create "project-foo")
        (etm-groups-create "project-bar")
        (etm-groups-create "documentation")
        ;; Verify all groups are listed
        (let ((groups (etm-groups-list-all)))
          (should (member "project-foo" groups))
          (should (member "project-bar" groups))
          (should (member "documentation" groups))
          (should (= 3 (length groups)))))
    (test-etm-groups-teardown)))

(ert-deftest test-etm-groups-tab-isolation ()
  "Test that groups are isolated per tab."
  (test-etm-groups-setup)
  (unwind-protect
      (progn
        (require 'etm-groups-core)
        ;; Mock the tab ID function for testing
        (let ((test-tab-id "test-tab-1"))
          (cl-letf (((symbol-function 'etm-groups--get-current-tab-id)
                     (lambda () test-tab-id)))
            ;; Create group in tab 1
            (etm-groups-create "project-foo")
            (etm-groups-add-buffer "project-foo" "main.el")
            
            ;; Switch to tab 2
            (setq test-tab-id "test-tab-2")
            ;; Group should not exist in tab 2
            (should-not (etm-groups-exists-p "project-foo"))
            
            ;; Create different group in tab 2
            (etm-groups-create "project-bar")
            
            ;; Switch back to tab 1
            (setq test-tab-id "test-tab-1")
            ;; Original group should still exist
            (should (etm-groups-exists-p "project-foo"))
            ;; Tab 2's group should not be visible
            (should-not (etm-groups-exists-p "project-bar")))))
    (test-etm-groups-teardown)))

(ert-deftest test-etm-groups-buffer-in-multiple-groups ()
  "Test that a buffer can belong to multiple groups."
  (test-etm-groups-setup)
  (unwind-protect
      (progn
        (require 'etm-groups-core)
        ;; Create groups and add same buffer to both
        (etm-groups-create "project-foo")
        (etm-groups-create "important-files")
        (etm-groups-add-buffer "project-foo" "main.el")
        (etm-groups-add-buffer "important-files" "main.el")
        
        ;; Verify buffer is in both groups
        (should (member "main.el" (etm-groups-get-buffers "project-foo")))
        (should (member "main.el" (etm-groups-get-buffers "important-files")))
        
        ;; Test finding groups for buffer
        (let ((groups (etm-groups-find-buffer-groups "main.el")))
          (should (member "project-foo" groups))
          (should (member "important-files" groups))
          (should (= 2 (length groups)))))
    (test-etm-groups-teardown)))

(provide 'test-etm-groups-core)
;;; test-etm-groups-core.el ends here