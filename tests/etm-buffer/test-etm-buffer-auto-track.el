;;; -*- coding: utf-8; lexical-binding: t -*-
;;; test-etm-buffer-auto-track.el --- Tests for ETM buffer auto-tracking
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Test suite for automatic buffer tracking functionality

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'etm-core-variables)
(require 'etm-buffer-auto-track)

;; Test Helpers
;; ----------------------------------------

(defmacro with-etm-auto-track-test-env (&rest body)
  "Execute BODY in a clean auto-track test environment."
  `(let ((etm-tab-tracked-buffers (make-hash-table :test 'equal))
         (etm-buffer-tab-association (make-hash-table :test 'equal))
         (etm-protected-buffers nil)
         (etm-auto-track-buffers t)
         (etm-track-exclude-patterns '("\\*Messages\\*" "\\*scratch\\*" "^ ")))
     (unwind-protect
         (cl-letf (((symbol-function 'tab-bar--current-tab)
                    (lambda () '((name . "test-tab")))))
           ,@body)
       (clrhash etm-tab-tracked-buffers)
       (clrhash etm-buffer-tab-association))))

;; Tests for --etm-track-buffer-excluded-p
;; ----------------------------------------

(ert-deftest test-etm-track-buffer-excluded-p-matches ()
  "Test that exclusion patterns correctly match buffer names."
  (with-etm-auto-track-test-env
    (should (--etm-track-buffer-excluded-p "*Messages*"))
    (should (--etm-track-buffer-excluded-p "*scratch*"))
    (should (--etm-track-buffer-excluded-p " hidden-buffer"))))

(ert-deftest test-etm-track-buffer-excluded-p-no-match ()
  "Test that regular buffers are not excluded."
  (with-etm-auto-track-test-env
    (should-not (--etm-track-buffer-excluded-p "main.el"))
    (should-not (--etm-track-buffer-excluded-p "*my-custom*"))
    (should-not (--etm-track-buffer-excluded-p "test-buffer"))))

;; Tests for etm-track-buffer
;; ----------------------------------------

(ert-deftest test-etm-track-buffer-basic ()
  "Test basic buffer tracking functionality."
  (with-etm-auto-track-test-env
    (let ((test-buffer (get-buffer-create "*etm-track-test*")))
      (unwind-protect
          (progn
            (etm-track-buffer test-buffer "test-tab")
            (let ((tracked (gethash "test-tab" etm-tab-tracked-buffers)))
              (should (member "*etm-track-test*" tracked))))
        (kill-buffer test-buffer)))))

(ert-deftest test-etm-track-buffer-with-string-name ()
  "Test tracking buffer by string name."
  (with-etm-auto-track-test-env
    (etm-track-buffer "my-buffer.el" "test-tab")
    (let ((tracked (gethash "test-tab" etm-tab-tracked-buffers)))
      (should (member "my-buffer.el" tracked)))))

(ert-deftest test-etm-track-buffer-excluded-buffer ()
  "Test that excluded buffers are not tracked."
  (with-etm-auto-track-test-env
    (etm-track-buffer "*Messages*" "test-tab")
    (let ((tracked (gethash "test-tab" etm-tab-tracked-buffers)))
      (should-not (member "*Messages*" tracked)))))

(ert-deftest test-etm-track-buffer-no-duplicate ()
  "Test that same buffer is not tracked twice."
  (with-etm-auto-track-test-env
    (etm-track-buffer "unique.el" "test-tab")
    (etm-track-buffer "unique.el" "test-tab")
    (let ((tracked (gethash "test-tab" etm-tab-tracked-buffers)))
      (should (= 1 (cl-count "unique.el" tracked :test 'equal))))))

;; Tests for etm-untrack-buffer
;; ----------------------------------------

(ert-deftest test-etm-untrack-buffer-basic ()
  "Test basic buffer untracking."
  (with-etm-auto-track-test-env
    ;; First track a buffer
    (etm-track-buffer "to-untrack.el" "test-tab")
    (should (member "to-untrack.el" (gethash "test-tab" etm-tab-tracked-buffers)))
    ;; Then untrack it
    (etm-untrack-buffer "to-untrack.el" "test-tab")
    (should-not (member "to-untrack.el" (gethash "test-tab" etm-tab-tracked-buffers)))))

(ert-deftest test-etm-untrack-buffer-nonexistent ()
  "Test untracking a buffer that was never tracked."
  (with-etm-auto-track-test-env
    ;; Should not error
    (etm-untrack-buffer "never-tracked.el" "test-tab")
    (should-not (member "never-tracked.el" (gethash "test-tab" etm-tab-tracked-buffers)))))

;; Tests for etm-get-tracked-buffers
;; ----------------------------------------

(ert-deftest test-etm-get-tracked-buffers-empty ()
  "Test getting tracked buffers from empty tab."
  (with-etm-auto-track-test-env
    (should (null (etm-get-tracked-buffers "empty-tab")))))

(ert-deftest test-etm-get-tracked-buffers-with-buffers ()
  "Test getting tracked buffers with multiple buffers."
  (with-etm-auto-track-test-env
    (etm-track-buffer "buffer1.el" "test-tab")
    (etm-track-buffer "buffer2.el" "test-tab")
    (etm-track-buffer "buffer3.el" "test-tab")
    (let ((tracked (etm-get-tracked-buffers "test-tab")))
      (should (= 3 (length tracked)))
      (should (member "buffer1.el" tracked))
      (should (member "buffer2.el" tracked))
      (should (member "buffer3.el" tracked)))))

;; Tests for etm-clear-tracked-buffers
;; ----------------------------------------

(ert-deftest test-etm-clear-tracked-buffers ()
  "Test clearing all tracked buffers from a tab."
  (with-etm-auto-track-test-env
    (etm-track-buffer "clear1.el" "test-tab")
    (etm-track-buffer "clear2.el" "test-tab")
    (should (= 2 (length (etm-get-tracked-buffers "test-tab"))))
    (etm-clear-tracked-buffers "test-tab")
    (should (null (etm-get-tracked-buffers "test-tab")))))

;; Tests for etm-kill-tracked-buffers
;; ----------------------------------------

(ert-deftest test-etm-kill-tracked-buffers ()
  "Test killing tracked buffers."
  (with-etm-auto-track-test-env
    (let ((buf1 (get-buffer-create "*etm-kill-test-1*"))
          (buf2 (get-buffer-create "*etm-kill-test-2*")))
      (unwind-protect
          (progn
            (etm-track-buffer buf1 "test-tab")
            (etm-track-buffer buf2 "test-tab")
            (should (buffer-live-p buf1))
            (should (buffer-live-p buf2))
            (let ((killed-count (etm-kill-tracked-buffers "test-tab")))
              (should (= 2 killed-count))
              (should-not (buffer-live-p buf1))
              (should-not (buffer-live-p buf2))))
        (when (buffer-live-p buf1) (kill-buffer buf1))
        (when (buffer-live-p buf2) (kill-buffer buf2))))))

(ert-deftest test-etm-kill-tracked-buffers-skips-protected ()
  "Test that protected buffers are not killed."
  (with-etm-auto-track-test-env
    (let ((buf (get-buffer-create "*etm-protected-test*"))
          (etm-protected-buffers '("*etm-protected-test*")))
      (unwind-protect
          (progn
            (etm-track-buffer buf "test-tab")
            (let ((killed-count (etm-kill-tracked-buffers "test-tab")))
              (should (= 0 killed-count))
              (should (buffer-live-p buf))))
        (kill-buffer buf)))))

(ert-deftest test-etm-kill-tracked-buffers-handles-dead ()
  "Test handling of already-dead buffers."
  (with-etm-auto-track-test-env
    ;; Track a buffer name that doesn't exist
    (puthash "test-tab" '("nonexistent-buffer") etm-tab-tracked-buffers)
    (let ((killed-count (etm-kill-tracked-buffers "test-tab")))
      (should (= 0 killed-count)))))

;; Tests for buffer-tab association
;; ----------------------------------------

(ert-deftest test-etm-buffer-tab-association ()
  "Test that buffer-tab association is correctly maintained."
  (with-etm-auto-track-test-env
    (etm-track-buffer "associated.el" "my-tab")
    (should (equal "my-tab" (gethash "associated.el" etm-buffer-tab-association)))))

;; Tests for multiple tabs
;; ----------------------------------------

(ert-deftest test-etm-track-buffer-multiple-tabs ()
  "Test tracking buffers in multiple tabs independently."
  (with-etm-auto-track-test-env
    (etm-track-buffer "tab1-buffer.el" "tab1")
    (etm-track-buffer "tab2-buffer.el" "tab2")
    (etm-track-buffer "shared.el" "tab1")
    (etm-track-buffer "shared.el" "tab2")

    (let ((tab1-tracked (etm-get-tracked-buffers "tab1"))
          (tab2-tracked (etm-get-tracked-buffers "tab2")))
      (should (member "tab1-buffer.el" tab1-tracked))
      (should-not (member "tab1-buffer.el" tab2-tracked))
      (should (member "tab2-buffer.el" tab2-tracked))
      (should-not (member "tab2-buffer.el" tab1-tracked))
      (should (member "shared.el" tab1-tracked))
      (should (member "shared.el" tab2-tracked)))))

;; Module Loading Tests
;; ----------------------------------------

(ert-deftest test-etm-buffer-auto-track-loadable ()
  "Test that etm-buffer-auto-track module loads correctly."
  (should (featurep 'etm-buffer-auto-track)))

(ert-deftest test-etm-buffer-auto-track-functions-exist ()
  "Test that key functions are defined."
  (should (fboundp 'etm-track-buffer))
  (should (fboundp 'etm-untrack-buffer))
  (should (fboundp 'etm-get-tracked-buffers))
  (should (fboundp 'etm-clear-tracked-buffers))
  (should (fboundp 'etm-kill-tracked-buffers))
  (should (fboundp 'etm-auto-track-enable))
  (should (fboundp 'etm-auto-track-disable))
  (should (fboundp '--etm-track-buffer-excluded-p)))

(provide 'test-etm-buffer-auto-track)

(when (not load-file-name)
  (ert-run-tests-interactively t))

;;; test-etm-buffer-auto-track.el ends here
