;;; -*- coding: utf-8; lexical-binding: t -*-
;;; test-etm-core-variables.el --- Tests for ETM core variables
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Test suite for ETM core variable definitions and utility functions

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'etm-core-variables)

;; Tests for module loading
;; ----------------------------------------

(ert-deftest test-etm-core-variables-loadable ()
  "Test that etm-core-variables module loads correctly."
  (should (featurep 'etm-core-variables)))

;; Tests for version and constants
;; ----------------------------------------

(ert-deftest test-etm-core-variables-constants-exist ()
  "Test that core constants are defined."
  (should (boundp 'etm-version))
  (should (boundp 'etm-default-buffer-types))
  (should (boundp 'etm-registered-buffer-types)))

(ert-deftest test-etm-version-format ()
  "Test that version is a properly formatted string."
  (should (stringp etm-version))
  (should (string-match-p "^[0-9]+\\.[0-9]+\\.[0-9]+" etm-version)))

(ert-deftest test-etm-default-buffer-types-content ()
  "Test that default buffer types contain expected types."
  (should (listp etm-default-buffer-types))
  (should (member "home" etm-default-buffer-types))
  (should (member "semi-home" etm-default-buffer-types))
  (should (member "results" etm-default-buffer-types)))

;; Tests for customizable variables
;; ----------------------------------------

(ert-deftest test-etm-customizable-variables-exist ()
  "Test that customizable variables are defined."
  (should (boundp 'etm-custom-buffer-types))
  (should (boundp 'etm-registered-buffers))
  (should (boundp 'etm-protected-buffers))
  (should (boundp 'etm-layout-save-dir))
  (should (boundp 'etm-max-numeric-buffers))
  (should (boundp 'etm-auto-track-buffers))
  (should (boundp 'etm-close-kills-tracked-buffers)))

(ert-deftest test-etm-max-numeric-buffers-default ()
  "Test that max numeric buffers has reasonable default."
  (should (integerp etm-max-numeric-buffers))
  (should (>= etm-max-numeric-buffers 1))
  (should (<= etm-max-numeric-buffers 99)))

(ert-deftest test-etm-track-exclude-patterns-format ()
  "Test that track exclude patterns are valid regexps."
  (should (listp etm-track-exclude-patterns))
  (dolist (pattern etm-track-exclude-patterns)
    (should (stringp pattern))
    ;; Should not error when used as regexp
    (should (or (string-match-p pattern "") t))))

;; Tests for debug functions
;; ----------------------------------------

(ert-deftest test-etm-toggle-debug ()
  "Test that toggle-debug function toggles the debug flag."
  (let ((original-debug etm-debug-flag-master))
    (unwind-protect
        (progn
          (setq etm-debug-flag-master nil)
          (etm-toggle-debug 'master)
          (should (eq etm-debug-flag-master t))
          (etm-toggle-debug 'master)
          (should (eq etm-debug-flag-master nil)))
      (setq etm-debug-flag-master original-debug))))

(ert-deftest test-etm-debug-message-when-flag-disabled ()
  "Test that etm-debug-message does nothing when flag is disabled."
  (let ((etm-debug-flag-numeric nil)
        (etm-debug-flag-master nil)
        (message-called nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _) (setq message-called t))))
      (etm-debug-message 'numeric "Test message")
      (should-not message-called))))

(ert-deftest test-etm-debug-message-when-flag-enabled ()
  "Test that etm-debug-message logs when category flag is enabled."
  (let ((etm-debug-flag-numeric t)
        (etm-debug-flag-master nil)
        (logged-message nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq logged-message (apply #'format fmt args)))))
      (etm-debug-message 'numeric "Test message: %s" "value")
      (should logged-message)
      (should (string-match-p "\\[ETM:NUMERIC\\]" logged-message))
      (should (string-match-p "Test message: value" logged-message)))))

(ert-deftest test-etm-debug-message-with-master-flag ()
  "Test that etm-debug-message logs when master flag is enabled."
  (let ((etm-debug-flag-numeric nil)
        (etm-debug-flag-master t)
        (logged-message nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq logged-message (apply #'format fmt args)))))
      (etm-debug-message 'numeric "Test message")
      (should logged-message)
      (should (string-match-p "\\[ETM:NUMERIC\\]" logged-message)))))

;; Tests for numeric buffer variables
;; ----------------------------------------

(ert-deftest test-etm-numeric-buffers-variable ()
  "Test that numeric buffers variable exists and is nil initially."
  (should (boundp 'etm-numeric-buffers)))

(ert-deftest test-etm-numeric-indicators-variables ()
  "Test that numeric indicator variables are properly defined."
  (should (boundp 'etm-numeric-indicators-enabled))
  (should (boundp 'etm-numeric-indicator-format))
  (should (boundp 'etm-numeric-indicator-separator))
  (should (stringp etm-numeric-indicator-format))
  (should (stringp etm-numeric-indicator-separator)))

;; Tests for tracking variables
;; ----------------------------------------

(ert-deftest test-etm-tab-tracked-buffers-hash-table ()
  "Test that tab tracked buffers is a hash table."
  (should (boundp 'etm-tab-tracked-buffers))
  (should (hash-table-p etm-tab-tracked-buffers)))

(ert-deftest test-etm-auto-track-buffers-default ()
  "Test that auto-track buffers has correct default."
  (should (boundp 'etm-auto-track-buffers))
  (should (booleanp etm-auto-track-buffers)))

;; Tests for layout variables
;; ----------------------------------------

(ert-deftest test-etm-layout-save-dir-format ()
  "Test that layout save dir is a valid path."
  (should (stringp etm-layout-save-dir)))

(ert-deftest test-etm-layout-default-hosts ()
  "Test that layout default hosts is a hash table."
  (should (boundp 'etm-layout-default-hosts))
  (should (hash-table-p etm-layout-default-hosts)))

;; Tests for localhost names
;; ----------------------------------------

(ert-deftest test-etm-localhost-names-list ()
  "Test that localhost names is a list of strings."
  (should (listp etm-localhost-names))
  (dolist (name etm-localhost-names)
    (should (stringp name))))

(ert-deftest test-etm-localhost-names-contains-localhost ()
  "Test that localhost is in the localhost names list."
  (should (member "localhost" etm-localhost-names)))

(ert-deftest test-etm-localhost-names-contains-lh-alias ()
  "Test that 'lh' is registered as a localhost alias."
  (should (member "lh" etm-localhost-names)))

;; Tests for function existence
;; ----------------------------------------

(ert-deftest test-etm-core-variables-functions-exist ()
  "Test that utility functions are defined."
  (should (fboundp 'etm-toggle-debug))
  (should (fboundp 'etm-debug-message))
  (should (fboundp 'etm-debug-status)))

(provide 'test-etm-core-variables)

(when (not load-file-name)
  (ert-run-tests-interactively t))

;;; test-etm-core-variables.el ends here
