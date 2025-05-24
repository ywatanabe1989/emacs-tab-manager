;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-24 15:10:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/etm-layout/test-etm-layout-reload.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'etm-layout-open)

(ert-deftest test-etm-layout-open-force-reload ()
  "Test etm-layout-open with force-reload parameter."
  (let ((test-layout-name "test-reload")
        (test-function-name 'etm-open-test-reload)
        (original-function-body nil))
    
    ;; Mock function to simulate cached layout function
    (fset test-function-name 
          (lambda () "original-version"))
    
    ;; Mock file existence and loading
    (cl-letf (((symbol-function 'etm-layout-list-available)
               (lambda () (list test-layout-name)))
              ((symbol-function 'etm-layout-function-name)
               (lambda (name) test-function-name))
              ((symbol-function 'etm-layout-file-path)
               (lambda (name) "/mock/path.el"))
              ((symbol-function 'file-exists-p)
               (lambda (path) t))
              ((symbol-function '--etm-load-file-silent)
               (lambda (path) 
                 ;; Simulate reloading with updated function
                 (fset test-function-name 
                       (lambda () "reloaded-version"))))
              ((symbol-function 'completing-read)
               (lambda (prompt collection &rest args) test-layout-name)))
      
      ;; Test without force-reload (should use cached version)
      (let ((result nil))
        (cl-letf (((symbol-function test-function-name)
                   (lambda () (setq result "cached-called"))))
          (etm-layout-open test-layout-name nil)
          (should (string= result "cached-called"))))
      
      ;; Test with force-reload (should reload and use new version)
      (let ((reloaded nil)
            (result nil))
        (cl-letf (((symbol-function '--etm-load-file-silent)
                   (lambda (path) 
                     (setq reloaded t)
                     (fset test-function-name 
                           (lambda () (setq result "reloaded-called")))))
                  ((symbol-function test-function-name)
                   (lambda () (setq result "should-not-be-called"))))
          (etm-layout-open test-layout-name t)
          (should reloaded)
          (should (string= result "reloaded-called"))))))

(ert-deftest test-etm-layout-reload ()
  "Test etm-layout-reload function."
  (let ((test-layout-name "test-single-reload")
        (reloaded nil))
    
    ;; Mock dependencies
    (cl-letf (((symbol-function 'etm-layout-list-available)
               (lambda () (list test-layout-name)))
              ((symbol-function 'etm-layout-function-name)
               (lambda (name) 'etm-open-test-single-reload))
              ((symbol-function 'etm-layout-file-path)
               (lambda (name) "/mock/single-path.el"))
              ((symbol-function 'file-exists-p)
               (lambda (path) t))
              ((symbol-function '--etm-load-file-silent)
               (lambda (path) (setq reloaded t)))
              ((symbol-function 'completing-read)
               (lambda (prompt collection &rest args) test-layout-name))
              ((symbol-function 'message)
               (lambda (format-string &rest args) nil)))
      
      ;; Test reload function
      (etm-layout-reload test-layout-name)
      (should reloaded))))

(provide 'test-etm-layout-reload)