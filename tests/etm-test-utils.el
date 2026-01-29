;;; -*- coding: utf-8; lexical-binding: t -*-
;;; etm-test-utils.el --- Test utilities for ETM test suite
;;; Author: ywatanabe
;;; Commentary:
;;; Common test utilities and macros for ETM tests

;;; Code:

(require 'ert)

(defmacro with-etm-test-environment (&rest body)
  "Execute BODY in a clean ETM test environment."
  `(let ((etm-registered-buffers nil)
         (etm-numeric-buffers nil)
         (etm-groups nil)
         ;; Preserve original tab state
         (original-tabs (when (fboundp 'tab-bar-tabs) (tab-bar-tabs))))
     (unwind-protect
         (progn ,@body)
       ;; Cleanup
       (setq etm-registered-buffers nil
             etm-numeric-buffers nil
             etm-groups nil))))

(defmacro with-etm-test-tab (tab-name &rest body)
  "Execute BODY with a test tab TAB-NAME."
  `(let ((test-tab-name ,tab-name))
     (cl-letf (((symbol-function 'tab-bar--current-tab)
                (lambda () `((name . ,test-tab-name))))
               ((symbol-function 'alist-get)
                (lambda (key alist &optional default _remove _testfn)
                  (if (eq key 'name)
                      test-tab-name
                    (assoc-default key alist nil default)))))
       ,@body)))

(defmacro with-etm-test-buffer (buffer-name &rest body)
  "Execute BODY with a test buffer BUFFER-NAME."
  `(let ((test-buffer (get-buffer-create ,buffer-name)))
     (unwind-protect
         (with-current-buffer test-buffer
           ,@body)
       (when (buffer-live-p test-buffer)
         (kill-buffer test-buffer)))))

(defun etm-test-create-mock-tab (tab-name)
  "Create a mock tab with TAB-NAME for testing."
  ;; In test environment, we just need to ensure the tab name is available
  tab-name)

(defun etm-test-cleanup ()
  "Clean up test environment."
  (setq etm-registered-buffers nil
        etm-numeric-buffers nil
        etm-groups nil)
  ;; Kill any test buffers
  (dolist (buffer (buffer-list))
    (when (string-match-p "^\\*?test-" (buffer-name buffer))
      (kill-buffer buffer))))

(provide 'etm-test-utils)

;;; etm-test-utils.el ends here
