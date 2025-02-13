;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 19:53:18>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-vars.el
;;; test-etm-variables.el --- Tests for etm-variables.el -*- lexical-binding: t -*-

(require 'ert)
(require 'etm-variables)

(ert-deftest test-etm-variables-loadable
    ()
  (should
   (featurep 'etm-variables)))

(ert-deftest test-etm-registered-buffers-exists
    ()
  (should
   (boundp 'etm-registered-buffers)))

(ert-deftest test-etm-registered-buffers-initially-nil
    ()
  (should
   (null etm-registered-buffers)))

(ert-deftest test-etm-registered-buffer-types-exists
    ()
  (should
   (boundp 'etm-registered-buffer-types)))

(ert-deftest test-etm-registered-buffer-types-has-home
    ()
  (should
   (member "home" etm-registered-buffer-types)))

(ert-deftest test-etm-registered-buffer-types-has-semi-home
    ()
  (should
   (member "semi-home" etm-registered-buffer-types)))

(ert-deftest test-etm-registered-buffer-types-has-results
    ()
  (should
   (member "results" etm-registered-buffer-types)))

(ert-deftest test-etm-custom-buffer-types-exists
    ()
  (should
   (boundp 'etm-custom-buffer-types)))

(ert-deftest test-etm-custom-buffer-types-initially-nil
    ()
  (should
   (null etm-custom-buffer-types)))

(ert-deftest test-etm-registered-layouts-exists
    ()
  (should
   (boundp 'etm-registered-layouts)))

(ert-deftest test-etm-registered-layouts-initially-nil
    ()
  (should
   (null etm-registered-layouts)))

(provide 'test-etm-variables)

(provide 'test-etm-vars)

(when
    (not load-file-name)
  (message "test-etm-vars.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))