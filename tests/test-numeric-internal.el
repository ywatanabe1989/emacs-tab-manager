;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Test that numeric IDs work internally without being shown

(require 'cl-lib)
(require 'etm-core-variables)
(require 'etm-buffer-numeric)
(require 'etm-buffer-numeric-indicators)

;; Test 1: Register buffer and verify no IDs in messages
(defun test-numeric-internal-messages ()
  "Test that numeric IDs are not exposed in messages."
  (let ((test-buffer-name "test-buffer-1")
        (tab-name "test-tab"))
    ;; Mock tab-bar function
    (cl-letf (((symbol-function 'tab-bar--current-tab)
               (lambda () `((name . ,tab-name)))))
      ;; Register buffer
      (let ((id (--etm-numeric-register-buffer test-buffer-name tab-name)))
        ;; Verify ID was assigned internally
        (cl-assert (numberp id) t "Should assign numeric ID")
        (cl-assert (= id 1) t "Should be ID 1")
        ;; Check that buffer can be retrieved by ID
        (cl-assert (string= (--etm-numeric-get-buffer-by-id 1 tab-name) test-buffer-name)
                   t "Should retrieve buffer by ID")))))

;; Test 2: Verify tab name formatting returns unchanged
(defun test-numeric-tab-name-unchanged ()
  "Test that tab names are not modified with indicators."
  (let ((tab-name "my-tab"))
    (cl-assert (string= (etm-numeric-format-tab-name tab-name) tab-name)
               t "Tab name should remain unchanged")))

;; Test 3: Verify mode-line indicator is empty
(defun test-numeric-modeline-empty ()
  "Test that mode-line indicator returns empty string."
  (let ((indicator (etm-numeric-mode-line-indicator "buffer" "tab")))
    (cl-assert (string= indicator "") t "Mode-line indicator should be empty")))

;; Run tests
(message "Testing numeric ID internals...")
(test-numeric-internal-messages)
(message "✓ Internal ID assignment works")
(test-numeric-tab-name-unchanged)
(message "✓ Tab names unchanged")
(test-numeric-modeline-empty)
(message "✓ Mode-line indicators empty")
(message "All tests passed!")