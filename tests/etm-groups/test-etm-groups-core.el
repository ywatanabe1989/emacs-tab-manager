;;; test-etm-groups-core.el --- Tests for ETM buffer groups core functionality -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Comprehensive test suite for core buffer grouping functionality

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Test helper functions
;; ----------------------------------------

(defun test-etm-groups-setup ()
  "Set up test environment for buffer groups."
  (when (boundp 'etm-tab-buffer-groups)
    (clrhash etm-tab-buffer-groups)))

(defun test-etm-groups-teardown ()
  "Clean up test environment."
  (when (boundp 'etm-tab-buffer-groups)
    (clrhash etm-tab-buffer-groups)))

(defmacro with-etm-groups-test-env (&rest body)
  "Execute BODY in a clean groups test environment."
  `(progn
     (test-etm-groups-setup)
     (unwind-protect
         (cl-letf (((symbol-function 'etm-groups--get-current-tab-id)
                    (lambda () "test-tab")))
           ,@body)
       (test-etm-groups-teardown))))

;; Module loading tests
;; ----------------------------------------

(ert-deftest test-etm-groups-core-loadable ()
  "Test that etm-groups-core module loads correctly."
  (require 'etm-groups-core)
  (should (featurep 'etm-groups-core)))

(ert-deftest test-etm-groups-core-functions-exist ()
  "Test that key functions are defined."
  (require 'etm-groups-core)
  (should (fboundp 'etm-groups-create))
  (should (fboundp 'etm-groups-exists-p))
  (should (fboundp 'etm-groups-delete))
  (should (fboundp 'etm-groups-add-buffer))
  (should (fboundp 'etm-groups-remove-buffer))
  (should (fboundp 'etm-groups-get-buffers))
  (should (fboundp 'etm-groups-list-all))
  (should (fboundp 'etm-groups-find-buffer-groups)))

;; Core functionality tests
;; ----------------------------------------

(ert-deftest test-etm-groups-create-group ()
  "Test creating a new buffer group."
  (with-etm-groups-test-env
    (require 'etm-groups-core)
    (etm-groups-create "project-foo")
    (should (etm-groups-exists-p "project-foo"))
    (should (null (etm-groups-get-buffers "project-foo")))))

(ert-deftest test-etm-groups-create-duplicate-error ()
  "Test that creating duplicate group raises error."
  (with-etm-groups-test-env
    (require 'etm-groups-core)
    (etm-groups-create "project-foo")
    (should-error (etm-groups-create "project-foo"))))

(ert-deftest test-etm-groups-add-buffer-to-group ()
  "Test adding buffers to a group."
  (with-etm-groups-test-env
    (require 'etm-groups-core)
    (etm-groups-create "project-foo")
    (etm-groups-add-buffer "project-foo" "main.el")
    (etm-groups-add-buffer "project-foo" "test.el")
    (let ((buffers (etm-groups-get-buffers "project-foo")))
      (should (member "main.el" buffers))
      (should (member "test.el" buffers))
      (should (= 2 (length buffers))))))

(ert-deftest test-etm-groups-add-buffer-no-duplicate ()
  "Test that adding same buffer twice doesn't create duplicate."
  (with-etm-groups-test-env
    (require 'etm-groups-core)
    (etm-groups-create "project-foo")
    (etm-groups-add-buffer "project-foo" "main.el")
    (etm-groups-add-buffer "project-foo" "main.el")
    (let ((buffers (etm-groups-get-buffers "project-foo")))
      (should (= 1 (length buffers))))))

(ert-deftest test-etm-groups-add-buffer-nonexistent-group ()
  "Test that adding buffer to nonexistent group raises error."
  (with-etm-groups-test-env
    (require 'etm-groups-core)
    (should-error (etm-groups-add-buffer "nonexistent" "main.el"))))

(ert-deftest test-etm-groups-remove-buffer-from-group ()
  "Test removing buffers from a group."
  (with-etm-groups-test-env
    (require 'etm-groups-core)
    (etm-groups-create "project-foo")
    (etm-groups-add-buffer "project-foo" "main.el")
    (etm-groups-add-buffer "project-foo" "test.el")
    (etm-groups-remove-buffer "project-foo" "main.el")
    (let ((buffers (etm-groups-get-buffers "project-foo")))
      (should-not (member "main.el" buffers))
      (should (member "test.el" buffers))
      (should (= 1 (length buffers))))))

(ert-deftest test-etm-groups-delete-group ()
  "Test deleting a group."
  (with-etm-groups-test-env
    (require 'etm-groups-core)
    (etm-groups-create "project-foo")
    (etm-groups-add-buffer "project-foo" "main.el")
    (should (etm-groups-exists-p "project-foo"))
    (etm-groups-delete "project-foo")
    (should-not (etm-groups-exists-p "project-foo"))))

(ert-deftest test-etm-groups-delete-nonexistent-error ()
  "Test that deleting nonexistent group raises error."
  (with-etm-groups-test-env
    (require 'etm-groups-core)
    (should-error (etm-groups-delete "nonexistent"))))

(ert-deftest test-etm-groups-list-all ()
  "Test listing all groups in current tab."
  (with-etm-groups-test-env
    (require 'etm-groups-core)
    (etm-groups-create "project-foo")
    (etm-groups-create "project-bar")
    (etm-groups-create "documentation")
    (let ((groups (etm-groups-list-all)))
      (should (member "project-foo" groups))
      (should (member "project-bar" groups))
      (should (member "documentation" groups))
      (should (= 3 (length groups))))))

(ert-deftest test-etm-groups-list-all-empty ()
  "Test listing groups when no groups exist."
  (with-etm-groups-test-env
    (require 'etm-groups-core)
    (should (null (etm-groups-list-all)))))

;; Tab isolation tests
;; ----------------------------------------

(ert-deftest test-etm-groups-tab-isolation ()
  "Test that groups are isolated per tab."
  (test-etm-groups-setup)
  (unwind-protect
      (progn
        (require 'etm-groups-core)
        (let ((test-tab-id "test-tab-1"))
          (cl-letf (((symbol-function 'etm-groups--get-current-tab-id)
                     (lambda () test-tab-id)))
            ;; Create group in tab 1
            (etm-groups-create "project-foo")
            (etm-groups-add-buffer "project-foo" "main.el")

            ;; Switch to tab 2
            (setq test-tab-id "test-tab-2")
            (should-not (etm-groups-exists-p "project-foo"))

            ;; Create different group in tab 2
            (etm-groups-create "project-bar")

            ;; Switch back to tab 1
            (setq test-tab-id "test-tab-1")
            (should (etm-groups-exists-p "project-foo"))
            (should-not (etm-groups-exists-p "project-bar")))))
    (test-etm-groups-teardown)))

;; Multiple group membership tests
;; ----------------------------------------

(ert-deftest test-etm-groups-buffer-in-multiple-groups ()
  "Test that a buffer can belong to multiple groups."
  (with-etm-groups-test-env
    (require 'etm-groups-core)
    (etm-groups-create "project-foo")
    (etm-groups-create "important-files")
    (etm-groups-add-buffer "project-foo" "main.el")
    (etm-groups-add-buffer "important-files" "main.el")

    (should (member "main.el" (etm-groups-get-buffers "project-foo")))
    (should (member "main.el" (etm-groups-get-buffers "important-files")))

    (let ((groups (etm-groups-find-buffer-groups "main.el")))
      (should (member "project-foo" groups))
      (should (member "important-files" groups))
      (should (= 2 (length groups))))))

(ert-deftest test-etm-groups-find-buffer-not-in-any-group ()
  "Test finding groups for buffer not in any group."
  (with-etm-groups-test-env
    (require 'etm-groups-core)
    (etm-groups-create "project-foo")
    (should (null (etm-groups-find-buffer-groups "lonely-buffer.el")))))

;; Validation tests
;; ----------------------------------------

(ert-deftest test-etm-groups-validate-empty-name ()
  "Test that empty group name is rejected."
  (with-etm-groups-test-env
    (require 'etm-groups-core)
    (should-error (etm-groups-create ""))))

(ert-deftest test-etm-groups-validate-newline-in-name ()
  "Test that group name with newline is rejected."
  (with-etm-groups-test-env
    (require 'etm-groups-core)
    (should-error (etm-groups-create "project\nfoo"))))

(ert-deftest test-etm-groups-validate-tab-in-name ()
  "Test that group name with tab is rejected."
  (with-etm-groups-test-env
    (require 'etm-groups-core)
    (should-error (etm-groups-create "project\tfoo"))))

;; Default names tests
;; ----------------------------------------

(ert-deftest test-etm-groups-default-names-exist ()
  "Test that default group names are defined."
  (require 'etm-groups-core)
  (should (boundp 'etm-groups-default-names))
  (should (listp etm-groups-default-names))
  (should (> (length etm-groups-default-names) 0)))

(provide 'test-etm-groups-core)

(when (not load-file-name)
  (ert-run-tests-interactively t))

;;; test-etm-groups-core.el ends here
