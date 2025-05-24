;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-25 09:50:00>
;;; File: test-etm-buffer-numeric-indicators.el

(require 'ert)
(require 'etm-buffer-numeric)

;; Test Setup
;; ----------------------------------------

(defmacro with-etm-numeric-test-setup (&rest body)
  "Execute BODY with clean numeric buffer setup."
  `(let ((etm-numeric-buffers nil)
         (etm-max-numeric-buffers 9)
         (etm-numeric-indicators-enabled t)
         (etm-numeric-indicator-format "[%s]")
         (etm-numeric-indicator-separator " "))
     ,@body))

;; Core Function Tests
;; ----------------------------------------

(ert-deftest test-etm-numeric-get-occupied-slots ()
  "Test getting list of occupied numeric slots."
  (with-etm-numeric-test-setup
   ;; Empty tab
   (should (equal (etm-numeric-get-occupied-slots "test-tab") nil))
   
   ;; Register some buffers
   (--etm-numeric-register-buffer "buffer1" "test-tab")
   (--etm-numeric-register-buffer "buffer2" "test-tab")
   (--etm-numeric-register-buffer "buffer3" "test-tab")
   
   ;; Should return sorted list
   (should (equal (etm-numeric-get-occupied-slots "test-tab") '(1 2 3)))
   
   ;; Register non-sequential
   (etm-numeric-set-buffer-id "buffer5" 5 "test-tab")
   (etm-numeric-set-buffer-id "buffer7" 7 "test-tab")
   
   (should (equal (etm-numeric-get-occupied-slots "test-tab") '(1 2 3 5 7)))))

(ert-deftest test-etm-numeric-format-indicator ()
  "Test formatting of numeric indicators."
  (with-etm-numeric-test-setup
   ;; Empty slots
   (should (equal (etm-numeric-format-indicator nil) ""))
   
   ;; Single slot
   (should (equal (etm-numeric-format-indicator '(1)) "[1]"))
   
   ;; Multiple slots
   (should (equal (etm-numeric-format-indicator '(1 3 5)) "[1 3 5]"))
   
   ;; Sequential slots (compact format)
   (should (equal (etm-numeric-format-indicator '(1 2 3 4 5)) "[1-5]"))
   
   ;; Mixed sequential and non-sequential
   (should (equal (etm-numeric-format-indicator '(1 2 3 5 7 8 9)) "[1-3 5 7-9]"))
   
   ;; Full slots
   (should (equal (etm-numeric-format-indicator '(1 2 3 4 5 6 7 8 9)) "[1-9]"))))

(ert-deftest test-etm-numeric-format-indicator-custom ()
  "Test custom formatting options."
  (with-etm-numeric-test-setup
   ;; Custom format
   (let ((etm-numeric-indicator-format "<%s>"))
     (should (equal (etm-numeric-format-indicator '(1 2 3)) "<1 2 3>")))
   
   ;; Custom separator
   (let ((etm-numeric-indicator-separator ","))
     (should (equal (etm-numeric-format-indicator '(1 3 5)) "[1,3,5]")))
   
   ;; Disabled indicators
   (let ((etm-numeric-indicators-enabled nil))
     (should (equal (etm-numeric-format-indicator '(1 2 3)) "")))))

(ert-deftest test-etm-numeric-get-buffer-slot ()
  "Test getting numeric slot for a buffer."
  (with-etm-numeric-test-setup
   ;; No assignment
   (should (equal (etm-numeric-get-buffer-slot "unassigned" "test-tab") nil))
   
   ;; With assignment
   (--etm-numeric-register-buffer "buffer1" "test-tab")
   (should (equal (etm-numeric-get-buffer-slot "buffer1" "test-tab") 1))
   
   ;; Multiple tabs
   (--etm-numeric-register-buffer "buffer2" "tab1")
   (--etm-numeric-register-buffer "buffer2" "tab2")
   (should (equal (etm-numeric-get-buffer-slot "buffer2" "tab1") 1))
   (should (equal (etm-numeric-get-buffer-slot "buffer2" "tab2") 1))))

(ert-deftest test-etm-numeric-update-tab-bar ()
  "Test tab-bar update with numeric indicators."
  (with-etm-numeric-test-setup
   ;; Mock tab-bar state
   (let ((test-tab-name "TestTab"))
     ;; No buffers
     (should (equal (etm-numeric-format-tab-name test-tab-name) "TestTab"))
     
     ;; With buffers
     (--etm-numeric-register-buffer "buffer1" test-tab-name)
     (--etm-numeric-register-buffer "buffer2" test-tab-name)
     (should (equal (etm-numeric-format-tab-name test-tab-name) "TestTab [1 2]"))
     
     ;; Many buffers
     (dotimes (i 7)
       (--etm-numeric-register-buffer (format "buffer%d" (+ i 3)) test-tab-name))
     (should (equal (etm-numeric-format-tab-name test-tab-name) "TestTab [1-9]")))))

(ert-deftest test-etm-numeric-mode-line-indicator ()
  "Test mode-line indicator for current buffer."
  (with-etm-numeric-test-setup
   (let ((test-buffer "test-buffer")
         (test-tab "test-tab"))
     ;; No assignment
     (should (equal (etm-numeric-mode-line-indicator test-buffer test-tab) ""))
     
     ;; With assignment
     (etm-numeric-set-buffer-id test-buffer 3 test-tab)
     (should (equal (etm-numeric-mode-line-indicator test-buffer test-tab) " ETM[3]"))
     
     ;; Disabled
     (let ((etm-numeric-indicators-enabled nil))
       (should (equal (etm-numeric-mode-line-indicator test-buffer test-tab) ""))))))

(ert-deftest test-etm-numeric-indicator-hooks ()
  "Test that indicators update on buffer changes."
  (with-etm-numeric-test-setup
   (let ((update-called nil))
     ;; Mock update function
     (cl-letf (((symbol-function 'etm-numeric-refresh-indicators)
                (lambda () (setq update-called t))))
       ;; Register buffer should trigger update
       (--etm-numeric-register-buffer "buffer1" "test-tab")
       (should update-called)
       
       ;; Reset
       (setq update-called nil)
       
       ;; Unregister should also trigger
       (etm-numeric-unregister-buffer "buffer1" "test-tab")
       (should update-called)))))

(ert-deftest test-etm-numeric-indicator-performance ()
  "Test performance with many buffers."
  (with-etm-numeric-test-setup
   ;; Register many buffers across multiple tabs
   (dotimes (tab-num 5)
     (let ((tab-name (format "tab-%d" tab-num)))
       (dotimes (buf-num 9)
         (--etm-numeric-register-buffer 
          (format "buffer-%d-%d" tab-num buf-num) 
          tab-name))))
   
   ;; Format should be efficient even with many buffers
   (dolist (tab-num '(0 1 2 3 4))
     (let ((tab-name (format "tab-%d" tab-num)))
       (should (equal (etm-numeric-format-indicator
                       (etm-numeric-get-occupied-slots tab-name))
                      "[1-9]"))))))

(provide 'test-etm-buffer-numeric-indicators)