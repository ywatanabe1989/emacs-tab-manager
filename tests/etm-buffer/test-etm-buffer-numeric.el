;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-24 16:40:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/etm-buffer/test-etm-buffer-numeric.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;; Tests for numeric buffer system

(require 'ert)
(require 'etm-buffer-numeric)

;; Helper function to create a mock tab
(defun --test-setup-mock-tab (tab-name)
  "Setup a mock tab for testing."
  (setq etm-numeric-buffers nil)  ; Clear state
  ;; Mock tab-bar--current-tab to return our test tab
  (cl-letf (((symbol-function 'tab-bar--current-tab)
             (lambda () `((name . ,tab-name)))))
    tab-name))

;; Test basic functionality
;; ----------------------------------------

(ert-deftest test-etm-numeric-register-buffer ()
  "Test registering buffers with numeric IDs."
  (let ((tab-name "test-tab"))
    (--test-setup-mock-tab tab-name)
    
    ;; Test registering first buffer
    (should (equal 1 (--etm-numeric-register-buffer "buffer1" tab-name)))
    (should (equal "buffer1" (--etm-numeric-get-buffer-by-id 1 tab-name)))
    (should (equal 1 (--etm-numeric-get-id-by-buffer "buffer1" tab-name)))
    
    ;; Test registering second buffer
    (should (equal 2 (--etm-numeric-register-buffer "buffer2" tab-name)))
    (should (equal "buffer2" (--etm-numeric-get-buffer-by-id 2 tab-name)))
    
    ;; Test getting next available ID
    (should (equal 3 (--etm-numeric-get-next-id tab-name)))))

(ert-deftest test-etm-numeric-unregister-buffer ()
  "Test unregistering buffers."
  (let ((tab-name "test-tab"))
    (--test-setup-mock-tab tab-name)
    
    ;; Setup: register some buffers
    (--etm-numeric-register-buffer "buffer1" tab-name)
    (--etm-numeric-register-buffer "buffer2" tab-name)
    (--etm-numeric-register-buffer "buffer3" tab-name)
    
    ;; Test unregister by ID
    (--etm-numeric-unregister-buffer 2 tab-name)
    (should (null (--etm-numeric-get-buffer-by-id 2 tab-name)))
    (should (equal "buffer1" (--etm-numeric-get-buffer-by-id 1 tab-name)))
    (should (equal "buffer3" (--etm-numeric-get-buffer-by-id 3 tab-name)))
    
    ;; Test unregister by buffer name
    (--etm-numeric-unregister-buffer "buffer1" tab-name)
    (should (null (--etm-numeric-get-buffer-by-id 1 tab-name)))
    (should (equal "buffer3" (--etm-numeric-get-buffer-by-id 3 tab-name)))))

(ert-deftest test-etm-numeric-max-buffers ()
  "Test maximum buffer limit."
  (let ((tab-name "test-tab")
        (etm-max-numeric-buffers 3))
    (--test-setup-mock-tab tab-name)
    
    ;; Register up to maximum
    (should (equal 1 (--etm-numeric-register-buffer "buffer1" tab-name)))
    (should (equal 2 (--etm-numeric-register-buffer "buffer2" tab-name)))
    (should (equal 3 (--etm-numeric-register-buffer "buffer3" tab-name)))
    
    ;; Should fail to register beyond maximum
    (should (null (--etm-numeric-register-buffer "buffer4" tab-name)))
    (should (null (--etm-numeric-get-next-id tab-name)))))

(ert-deftest test-etm-numeric-multiple-tabs ()
  "Test numeric buffers work independently across tabs."
  (setq etm-numeric-buffers nil)  ; Clear state
  
  ;; Register buffers in different tabs
  (--etm-numeric-register-buffer "buffer1-tab1" "tab1")
  (--etm-numeric-register-buffer "buffer1-tab2" "tab2")
  (--etm-numeric-register-buffer "buffer2-tab1" "tab1")
  
  ;; Verify independence
  (should (equal "buffer1-tab1" (--etm-numeric-get-buffer-by-id 1 "tab1")))
  (should (equal "buffer2-tab1" (--etm-numeric-get-buffer-by-id 2 "tab1")))
  (should (equal "buffer1-tab2" (--etm-numeric-get-buffer-by-id 1 "tab2")))
  (should (null (--etm-numeric-get-buffer-by-id 2 "tab2"))))

(ert-deftest test-etm-numeric-id-reuse ()
  "Test that unregistered IDs can be reused."
  (let ((tab-name "test-tab"))
    (--test-setup-mock-tab tab-name)
    
    ;; Register and unregister
    (--etm-numeric-register-buffer "buffer1" tab-name)
    (--etm-numeric-register-buffer "buffer2" tab-name)
    (--etm-numeric-unregister-buffer 1 tab-name)
    
    ;; Next registration should reuse ID 1
    (should (equal 1 (--etm-numeric-register-buffer "buffer3" tab-name)))
    (should (equal "buffer3" (--etm-numeric-get-buffer-by-id 1 tab-name)))))

;; Test edge cases
;; ----------------------------------------

(ert-deftest test-etm-numeric-empty-tab ()
  "Test behavior with empty tab."
  (let ((tab-name "empty-tab"))
    (--test-setup-mock-tab tab-name)
    
    (should (equal 1 (--etm-numeric-get-next-id tab-name)))
    (should (null (--etm-numeric-get-buffer-by-id 1 tab-name)))
    (should (null (--etm-numeric-get-id-by-buffer "nonexistent" tab-name)))))

(ert-deftest test-etm-numeric-nonexistent-tab ()
  "Test behavior with nonexistent tab."
  (setq etm-numeric-buffers nil)  ; Clear state
  
  (should (equal 1 (--etm-numeric-get-next-id "nonexistent")))
  (should (null (--etm-numeric-get-buffer-by-id 1 "nonexistent"))))

;; Test module loading
;; ----------------------------------------

(ert-deftest test-etm-buffer-numeric-loadable ()
  "Test that etm-buffer-numeric module loads correctly."
  (should (featurep 'etm-buffer-numeric)))

(ert-deftest test-etm-buffer-numeric-functions-exist ()
  "Test that key functions are defined."
  (should (fboundp '--etm-numeric-register-buffer))
  (should (fboundp '--etm-numeric-unregister-buffer))
  (should (fboundp '--etm-numeric-get-buffer-by-id))
  (should (fboundp '--etm-numeric-get-id-by-buffer))
  (should (fboundp 'etm-numeric-register-current-buffer))
  (should (fboundp 'etm-numeric-jump-to-buffer))
  (should (fboundp 'etm-numeric-list-buffers)))

(provide 'test-etm-buffer-numeric)

;;; test-etm-buffer-numeric.el ends here