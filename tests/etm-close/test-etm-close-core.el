;;; -*- coding: utf-8; lexical-binding: t -*-
;;; test-etm-close-core.el --- Tests for ETM close core functionality
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Comprehensive test suite for tab closing functionality

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'etm-close-core)

;; Test Helpers
;; ----------------------------------------

(defvar test-etm-close-mock-tabs nil
  "Mock tabs for testing.")

(defvar test-etm-close-closed-tabs nil
  "List of tabs that were closed during testing.")

(defvar test-etm-close-killed-buffers nil
  "List of buffers that were killed during testing.")

(defmacro with-etm-close-test-env (&rest body)
  "Execute BODY in a clean close test environment with mocked tab functions."
  `(let ((test-etm-close-mock-tabs
          '(((name . "tab1") (explicit-name . t))
            ((name . "tab2") (explicit-name . t))
            ((name . "tab3") (explicit-name . t))))
         (test-etm-close-closed-tabs nil)
         (test-etm-close-killed-buffers nil)
         (etm-ssh-connections (make-hash-table :test 'equal))
         (etm-tab-tracked-buffers (make-hash-table :test 'equal))
         (etm-numeric-buffers (make-hash-table :test 'equal))
         (etm-close-kills-tracked-buffers nil)
         (etm-debug nil))
     (cl-letf (((symbol-function 'tab-bar-tabs)
                (lambda () test-etm-close-mock-tabs))
               ((symbol-function 'tab-bar--current-tab)
                (lambda () (car test-etm-close-mock-tabs)))
               ((symbol-function 'tab-bar-close-tab)
                (lambda (&optional index)
                  (let ((idx (or index 1)))
                    (push (nth (1- idx) test-etm-close-mock-tabs)
                          test-etm-close-closed-tabs)
                    (setq test-etm-close-mock-tabs
                          (cl-remove-if
                           (lambda (tab)
                             (equal tab (nth (1- idx) test-etm-close-mock-tabs)))
                           test-etm-close-mock-tabs)))))
               ((symbol-function 'tab-close)
                (lambda ()
                  (push (car test-etm-close-mock-tabs) test-etm-close-closed-tabs)
                  (setq test-etm-close-mock-tabs (cdr test-etm-close-mock-tabs))))
               ((symbol-function 'tab-next)
                (lambda () nil))
               ((symbol-function 'tab-previous)
                (lambda () nil))
               ((symbol-function 'tab-bar-rename-tab)
                (lambda (&optional _name) nil))
               ((symbol-function 'tab-bar-mode)
                (lambda (&optional _arg) t))
               ((symbol-function 'etm-message)
                (lambda (&rest _) nil))
               ((symbol-function 'etm-tab-first-tab-p)
                (lambda () (= (length test-etm-close-mock-tabs) 1)))
               ((symbol-function 'etm-kill-tracked-buffers)
                (lambda (tab-name)
                  (let ((count (length (gethash tab-name etm-tab-tracked-buffers))))
                    (push (cons tab-name count) test-etm-close-killed-buffers)
                    count)))
               ((symbol-function 'etm-numeric-clear-tab)
                (lambda (tab-name)
                  (remhash tab-name etm-numeric-buffers)))
               ((symbol-function '--etm-unregister-ssh-connection)
                (lambda (tab-name)
                  (remhash tab-name etm-ssh-connections))))
       ,@body)))

;; Module Loading Tests
;; ----------------------------------------

(ert-deftest test-etm-close-core-loadable ()
  "Test that etm-close-core module loads correctly."
  (should (featurep 'etm-close-core)))

(ert-deftest test-etm-close-core-functions-exist ()
  "Test that key functions are defined."
  (should (fboundp 'etm-close))
  (should (fboundp 'etm-close-all))
  (should (fboundp 'etm-close-by-name))
  (should (fboundp 'etm-close-keep-buffers))
  (should (fboundp 'etm-close-with-connection-management))
  (should (fboundp 'etm-cleanup-unused-connections))
  (should (fboundp 'etm-reset)))

;; Tests for etm-close-by-name
;; ----------------------------------------

(ert-deftest test-etm-close-by-name-existing-tab ()
  "Test closing an existing tab by name."
  (with-etm-close-test-env
    (should (etm-close-by-name "tab2"))
    (should (= 2 (length test-etm-close-mock-tabs)))
    (should-not (cl-find-if
                 (lambda (tab) (string= "tab2" (alist-get 'name tab)))
                 test-etm-close-mock-tabs))))

(ert-deftest test-etm-close-by-name-nonexistent-tab ()
  "Test closing a nonexistent tab returns nil."
  (with-etm-close-test-env
    (should-not (etm-close-by-name "nonexistent-tab"))
    (should (= 3 (length test-etm-close-mock-tabs)))))

(ert-deftest test-etm-close-by-name-last-tab ()
  "Test that last tab cannot be closed."
  (with-etm-close-test-env
    ;; Set up only one tab
    (setq test-etm-close-mock-tabs
          '(((name . "only-tab") (explicit-name . t))))
    (should-not (etm-close-by-name "only-tab"))
    (should (= 1 (length test-etm-close-mock-tabs)))))

(ert-deftest test-etm-close-by-name-clears-numeric-buffers ()
  "Test that closing a tab clears its numeric buffer entries."
  (with-etm-close-test-env
    (puthash "tab2" '(("1" . "buf1") ("2" . "buf2")) etm-numeric-buffers)
    (etm-close-by-name "tab2")
    (should-not (gethash "tab2" etm-numeric-buffers))))

(ert-deftest test-etm-close-by-name-unregisters-ssh ()
  "Test that closing a tab unregisters SSH connection."
  (with-etm-close-test-env
    (puthash "tab2" "user@remote" etm-ssh-connections)
    (etm-close-by-name "tab2")
    (should-not (gethash "tab2" etm-ssh-connections))))

;; Tests for etm-close-all
;; ----------------------------------------

(ert-deftest test-etm-close-all-leaves-one-tab ()
  "Test that close-all leaves exactly one tab."
  (with-etm-close-test-env
    (etm-close-all)
    (should (= 1 (length test-etm-close-mock-tabs)))))

(ert-deftest test-etm-close-all-clears-ssh-connections ()
  "Test that close-all clears all SSH connections."
  (with-etm-close-test-env
    (puthash "tab1" "user@host1" etm-ssh-connections)
    (puthash "tab2" "user@host2" etm-ssh-connections)
    (etm-close-all)
    (should (= 0 (hash-table-count etm-ssh-connections)))))

(ert-deftest test-etm-close-all-with-single-tab ()
  "Test close-all when only one tab exists."
  (with-etm-close-test-env
    (setq test-etm-close-mock-tabs
          '(((name . "only-tab") (explicit-name . t))))
    (etm-close-all)
    (should (= 1 (length test-etm-close-mock-tabs)))))

;; Tests for etm-close
;; ----------------------------------------

(ert-deftest test-etm-close-basic ()
  "Test basic tab closing."
  (with-etm-close-test-env
    (let ((initial-count (length test-etm-close-mock-tabs)))
      (etm-close)
      (should (= (1- initial-count) (length test-etm-close-mock-tabs))))))

(ert-deftest test-etm-close-kills-buffers-when-enabled ()
  "Test that close kills tracked buffers when option is enabled."
  (with-etm-close-test-env
    (let ((etm-close-kills-tracked-buffers t))
      (puthash "tab1" '("buf1" "buf2" "buf3") etm-tab-tracked-buffers)
      (etm-close)
      (should (assoc "tab1" test-etm-close-killed-buffers)))))

(ert-deftest test-etm-close-preserves-buffers-when-disabled ()
  "Test that close preserves tracked buffers when option is disabled."
  (with-etm-close-test-env
    (let ((etm-close-kills-tracked-buffers nil))
      (puthash "tab1" '("buf1" "buf2") etm-tab-tracked-buffers)
      (etm-close)
      (should-not (assoc "tab1" test-etm-close-killed-buffers)))))

(ert-deftest test-etm-close-with-kill-buffers-arg ()
  "Test that close kills buffers when explicit argument is passed."
  (with-etm-close-test-env
    (let ((etm-close-kills-tracked-buffers nil))  ; Disabled by default
      (puthash "tab1" '("buf1" "buf2") etm-tab-tracked-buffers)
      (etm-close t)  ; Explicit argument to kill buffers
      (should (assoc "tab1" test-etm-close-killed-buffers)))))

;; Tests for etm-close-keep-buffers
;; ----------------------------------------

(ert-deftest test-etm-close-keep-buffers-preserves ()
  "Test that close-keep-buffers never kills tracked buffers."
  (with-etm-close-test-env
    (let ((etm-close-kills-tracked-buffers t))  ; Enabled globally
      (puthash "tab1" '("buf1" "buf2") etm-tab-tracked-buffers)
      (etm-close-keep-buffers)
      ;; Should not kill buffers despite global setting
      (should-not (assoc "tab1" test-etm-close-killed-buffers)))))

;; Tests for etm-close-with-connection-management
;; ----------------------------------------

(ert-deftest test-etm-close-with-connection-management ()
  "Test that close-with-connection-management unregisters SSH."
  (with-etm-close-test-env
    (puthash "tab1" "user@remote" etm-ssh-connections)
    (etm-close-with-connection-management)
    (should-not (gethash "tab1" etm-ssh-connections))))

;; Tests for etm-cleanup-unused-connections
;; ----------------------------------------

(ert-deftest test-etm-cleanup-unused-connections-defined ()
  "Test that cleanup-unused-connections is defined and callable."
  (should (fboundp 'etm-cleanup-unused-connections))
  ;; Should not error when called
  (etm-cleanup-unused-connections))

;; Edge Cases
;; ----------------------------------------

(ert-deftest test-etm-close-by-name-special-chars ()
  "Test closing tabs with special characters in name."
  (with-etm-close-test-env
    (setq test-etm-close-mock-tabs
          '(((name . "tab<1>") (explicit-name . t))
            ((name . "[project]") (explicit-name . t))
            ((name . "normal") (explicit-name . t))))
    (should (etm-close-by-name "tab<1>"))
    (should (= 2 (length test-etm-close-mock-tabs)))
    (should (etm-close-by-name "[project]"))
    (should (= 1 (length test-etm-close-mock-tabs)))))

(ert-deftest test-etm-close-multiple-sequential ()
  "Test closing multiple tabs sequentially."
  (with-etm-close-test-env
    (setq test-etm-close-mock-tabs
          '(((name . "tab1") (explicit-name . t))
            ((name . "tab2") (explicit-name . t))
            ((name . "tab3") (explicit-name . t))
            ((name . "tab4") (explicit-name . t))
            ((name . "tab5") (explicit-name . t))))
    (etm-close-by-name "tab2")
    (should (= 4 (length test-etm-close-mock-tabs)))
    (etm-close-by-name "tab4")
    (should (= 3 (length test-etm-close-mock-tabs)))
    (etm-close-by-name "tab1")
    (should (= 2 (length test-etm-close-mock-tabs)))))

(provide 'test-etm-close-core)

(when (not load-file-name)
  (ert-run-tests-interactively t))

;;; test-etm-close-core.el ends here
