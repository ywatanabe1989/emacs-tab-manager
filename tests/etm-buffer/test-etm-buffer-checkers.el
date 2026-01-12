;;; -*- coding: utf-8; lexical-binding: t -*-
;;; test-etm-buffer-checkers.el --- Tests for ETM buffer checking functions
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Comprehensive test suite for buffer registration and protection checking

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'etm-buffer-checkers)

;; Test Helpers
;; ----------------------------------------

(defmacro with-etm-buffer-test-env (&rest body)
  "Execute BODY in a clean ETM buffer test environment."
  `(let ((etm-registered-buffers nil)
         (etm-protected-buffers nil))
     (unwind-protect
         (progn ,@body)
       (setq etm-registered-buffers nil
             etm-protected-buffers nil))))

;; Tests for --etm-buffer-registered-p
;; ----------------------------------------

(ert-deftest test-etm-buffer-registered-p-with-name-only ()
  "Test buffer registration check with name only."
  (let ((etm-registered-buffers
         '(("tab1" . (("home" . "buffer1"))))))
    (should (--etm-buffer-registered-p "buffer1"))))

(ert-deftest test-etm-buffer-registered-p-with-type ()
  "Test buffer registration check with type filter."
  (let ((etm-registered-buffers
         '(("tab1" . (("home" . "buffer1"))))))
    (should (--etm-buffer-registered-p "buffer1" "home"))
    (should-not (--etm-buffer-registered-p "buffer1" "results"))))

(ert-deftest test-etm-buffer-registered-p-with-tab ()
  "Test buffer registration check with tab filter."
  (let ((etm-registered-buffers
         '(("tab1" . (("home" . "buffer1")))
           ("tab2" . (("home" . "buffer2"))))))
    (should (--etm-buffer-registered-p "buffer1" nil '((name . "tab1"))))
    (should-not (--etm-buffer-registered-p "buffer1" nil '((name . "tab2"))))))

(ert-deftest test-etm-buffer-registered-p-empty-registry ()
  "Test that unregistered buffer returns nil with empty registry."
  (with-etm-buffer-test-env
    (should-not (--etm-buffer-registered-p "nonexistent-buffer"))))

(ert-deftest test-etm-buffer-registered-p-multiple-types ()
  "Test checking buffer registered with multiple types."
  (with-etm-buffer-test-env
    (setq etm-registered-buffers
          '(("test-tab" . (("home" . "main.el")
                           ("semi-home" . "utils.el")
                           ("results" . "output.el")))))
    (should (--etm-buffer-registered-p "main.el"))
    (should (--etm-buffer-registered-p "utils.el"))
    (should (--etm-buffer-registered-p "output.el"))
    (should-not (--etm-buffer-registered-p "nonexistent.el"))))

(ert-deftest test-etm-buffer-registered-p-with-buffer-object ()
  "Test registration check with actual buffer object."
  (with-etm-buffer-test-env
    (let ((test-buffer (get-buffer-create "*etm-test-buffer*")))
      (unwind-protect
          (progn
            (setq etm-registered-buffers
                  '(("test-tab" . (("home" . "*etm-test-buffer*")))))
            (should (--etm-buffer-registered-p test-buffer)))
        (kill-buffer test-buffer)))))

(ert-deftest test-etm-buffer-registered-p-multiple-tabs ()
  "Test registration check across multiple tabs."
  (with-etm-buffer-test-env
    (setq etm-registered-buffers
          '(("tab1" . (("home" . "shared.el") ("semi-home" . "tab1-only.el")))
            ("tab2" . (("home" . "shared.el") ("results" . "tab2-only.el")))))
    ;; Buffer in multiple tabs should be found
    (should (--etm-buffer-registered-p "shared.el"))
    (should (--etm-buffer-registered-p "shared.el" "home"))
    ;; Tab-specific buffers
    (should (--etm-buffer-registered-p "tab1-only.el"))
    (should (--etm-buffer-registered-p "tab2-only.el"))))

;; Tests for --etm-buffer-protected-p
;; ----------------------------------------

(ert-deftest test-etm-buffer-protected-p ()
  "Test basic buffer protection check."
  (let ((etm-protected-buffers '("*scratch*" "*Messages*")))
    (should (--etm-buffer-protected-p "*scratch*"))
    (should-not (--etm-buffer-protected-p "regular-buffer"))))

(ert-deftest test-etm-buffer-protected-p-empty-list ()
  "Test protection check with empty protected list."
  (with-etm-buffer-test-env
    (should-not (--etm-buffer-protected-p "*scratch*"))
    (should-not (--etm-buffer-protected-p "test.el"))))

(ert-deftest test-etm-buffer-protected-p-multiple-buffers ()
  "Test protection check with multiple protected buffers."
  (with-etm-buffer-test-env
    (setq etm-protected-buffers '("*scratch*" "*Messages*" "important.el"))
    (should (--etm-buffer-protected-p "*scratch*"))
    (should (--etm-buffer-protected-p "*Messages*"))
    (should (--etm-buffer-protected-p "important.el"))
    (should-not (--etm-buffer-protected-p "random.el"))))

(ert-deftest test-etm-buffer-protected-p-case-sensitivity ()
  "Test that protection check is case-sensitive."
  (with-etm-buffer-test-env
    (setq etm-protected-buffers '("Important.el"))
    (should (--etm-buffer-protected-p "Important.el"))
    (should-not (--etm-buffer-protected-p "important.el"))
    (should-not (--etm-buffer-protected-p "IMPORTANT.el"))))

;; Edge Cases
;; ----------------------------------------

(ert-deftest test-etm-buffer-registered-p-special-chars ()
  "Test registration check with special characters in buffer name."
  (with-etm-buffer-test-env
    (setq etm-registered-buffers
          '(("tab" . (("home" . "*test<1>*")
                      ("semi-home" . "[2024-01-01]")))))
    (should (--etm-buffer-registered-p "*test<1>*"))
    (should (--etm-buffer-registered-p "[2024-01-01]"))))

(ert-deftest test-etm-buffer-registered-p-empty-string ()
  "Test behavior with empty string buffer name."
  (with-etm-buffer-test-env
    (setq etm-registered-buffers '(("tab" . (("home" . "test.el")))))
    (should-not (--etm-buffer-registered-p ""))))

;; Module Loading Tests
;; ----------------------------------------

(ert-deftest test-etm-buffer-checkers-loadable ()
  "Test that etm-buffer-checkers module loads correctly."
  (should (featurep 'etm-buffer-checkers)))

(ert-deftest test-etm-buffer-checkers-functions-exist ()
  "Test that key functions are defined."
  (should (fboundp '--etm-buffer-registered-p))
  (should (fboundp '--etm-buffer-protected-p)))

(provide 'test-etm-buffer-checkers)

(when (not load-file-name)
  (ert-run-tests-interactively t))

;;; test-etm-buffer-checkers.el ends here
