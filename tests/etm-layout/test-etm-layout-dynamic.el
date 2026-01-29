;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-01-30>
;;; Test file for: etm-layout-dynamic.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-layout-dynamic - dynamic command generation

;;; Code:

(require 'ert)
(require 'etm-layout-dynamic)

;; Test Configuration Variables
;; ----------------------------------------

(ert-deftest test-etm-dynamic-project-dirs-is-alist ()
  "etm-dynamic-project-dirs should be an alist."
  (should (listp etm-dynamic-project-dirs))
  (dolist (entry etm-dynamic-project-dirs)
    (should (consp entry))
    (should (stringp (car entry)))
    (should (stringp (cdr entry)))))

(ert-deftest test-etm-dynamic-layout-spec-format ()
  "etm-dynamic-layout-spec should have correct format."
  (should (listp etm-dynamic-layout-spec))
  (dolist (spec etm-dynamic-layout-spec)
    (should (>= (length spec) 6))
    (should (memq (nth 0 spec) '(file shell)))))

;; Test Helper Functions
;; ----------------------------------------

(ert-deftest test-etm-dynamic-expand-layout-spec ()
  "Layout spec expansion should replace dir symbol."
  (let ((expanded (--etm-dynamic-expand-layout-spec "/test/path")))
    (should (listp expanded))
    (dolist (spec expanded)
      (should (equal (nth 1 spec) "/test/path")))))

(ert-deftest test-etm-dynamic-is-project-dir-p-rejects-hidden ()
  "Hidden directories should not be considered projects."
  (should-not (--etm-dynamic-is-project-dir-p "/path/to/.hidden")))

;; Test Command Generation
;; ----------------------------------------

(ert-deftest test-etm-dynamic-create-command ()
  "Command creation should define interactive function."
  (let
      ((sym
	(--etm-dynamic-create-command "test" "myproj" "/tmp/myproj")))
    (should (symbolp sym))
    (should (equal (symbol-name sym) "test-myproj"))
    (should (fboundp sym))
    (should (commandp sym))
    ;; Cleanup
    (fmakunbound sym)))

(ert-deftest test-etm-dynamic-generate-commands-returns-count ()
  "Generate commands should return number of commands created."
  (let ((etm-dynamic-project-dirs nil)
        (etm-dynamic--generated-commands nil))
    (should (numberp (etm-dynamic-generate-commands)))))

(ert-deftest test-etm-dynamic-clear-commands ()
  "Clear commands should unbind all generated functions."
  (let ((etm-dynamic--generated-commands nil))
    ;; Create a test command
    (--etm-dynamic-create-command "test" "cleartest" "/tmp/cleartest")
    (should (fboundp 'test-cleartest))
    ;; Clear commands
    (etm-dynamic-clear-commands)
    (should-not (fboundp 'test-cleartest))
    (should (null etm-dynamic--generated-commands))))

;; Test Add Project Directory
;; ----------------------------------------

(ert-deftest test-etm-dynamic-add-project-dir-validates ()
  "Adding non-existent directory should error."
  (should-error
   (etm-dynamic-add-project-dir "test" "/nonexistent/path/12345")))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;;; test-etm-layout-dynamic.el ends here
