;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-24 15:32:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/etm-layout/test-etm-layout-reload.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)

(ert-deftest test-etm-layout-reload-loadable ()
  "Test that etm-layout-open can be loaded."
  (require 'etm-layout-open)
  (should (featurep 'etm-layout-open)))

(ert-deftest test-etm-layout-reload-function-exists ()
  "Test that etm-layout-reload function exists."
  (require 'etm-layout-open)
  (should (fboundp 'etm-layout-reload)))

(ert-deftest test-etm-layout-reload-calls-load-file ()
  "Test that etm-layout-reload actually calls file loading mechanism."
  (require 'etm-layout-open)
  (require 'cl-lib)
  (let ((load-called nil)
        (test-layout-name "test-reload"))
    
    ;; Mock the dependencies to isolate the reload behavior
    (cl-letf (((symbol-function 'etm-layout-list-available)
               (lambda () (list test-layout-name)))
              ((symbol-function 'etm-layout-file-path)
               (lambda (name) "/mock/path.el"))
              ((symbol-function 'file-exists-p)
               (lambda (path) t))
              ((symbol-function '--etm-load-file-silent)
               (lambda (path) (setq load-called t)))
              ((symbol-function 'completing-read)
               (lambda (prompt collection &rest args) test-layout-name))
              ((symbol-function 'message)
               (lambda (format-string &rest args) nil)))
      
      ;; Test that reload function calls the loading mechanism
      (etm-layout-reload test-layout-name)
      (should load-called))))

(ert-deftest test-etm-layout-open-force-reload-behavior ()
  "Test etm-layout-open force-reload parameter controls caching behavior."
  (require 'etm-layout-open)
  (require 'cl-lib)
  (let ((test-layout-name "test-force-reload")
        (test-function-name 'etm-open-test-force-reload)
        (load-called-count 0))
    
    ;; Set up a mock layout function
    (fset test-function-name (lambda () "test-result"))
    
    ;; Mock dependencies
    (cl-letf (((symbol-function 'etm-layout-list-available)
               (lambda () (list test-layout-name)))
              ((symbol-function 'etm-layout-function-name)
               (lambda (name) test-function-name))
              ((symbol-function 'etm-layout-file-path)
               (lambda (name) "/mock/force-reload-path.el"))
              ((symbol-function 'file-exists-p)
               (lambda (path) t))
              ((symbol-function '--etm-load-file-silent)
               (lambda (path) (setq load-called-count (1+ load-called-count))))
              ((symbol-function 'completing-read)
               (lambda (prompt collection &rest args) test-layout-name))
              ((symbol-function 'message)
               (lambda (format-string &rest args) nil)))
      
      ;; Test without force-reload (should load file)
      (etm-layout-open test-layout-name nil)
      (should (= load-called-count 1))
      
      ;; Test with force-reload (should skip loading)
      (etm-layout-open test-layout-name t)
      (should (= load-called-count 1)))))  ;; Count should remain 1

(provide 'test-etm-layout-reload)