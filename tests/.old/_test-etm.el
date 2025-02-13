;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 19:40:21>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm.el
;;; test-etm.el --- Tests for etm.el -*- lexical-binding: t -*-

(require 'ert)

(ert-deftest test-etm-version
    ()
  (require 'etm)
  (should
   (string= etm-version "0.1.0")))

(ert-deftest test-etm-loadable
    ()
  (require 'etm)
  (should
   (featurep 'etm-init)))

(ert-deftest test-etm-init-loadable
    ()
  (require 'etm-init)
  (should
   (featurep 'etm-init)))

(ert-deftest test-etm-new-loadable
    ()
  (require 'etm-new)
  (should
   (featurep 'etm-new)))

(ert-deftest test-etm-navigation-loadable
    ()
  (require 'etm-navigation)
  (should
   (featurep 'etm-navigation)))

(ert-deftest test-etm-close-loadable
    ()
  (require 'etm-close)
  (should
   (featurep 'etm-close)))

(ert-deftest test-etm-buffer-loadable
    ()
  (require 'etm-buffer)
  (should
   (featurep 'etm-buffer)))

(ert-deftest test-etm-layout-loadable
    ()
  (require 'etm-layout)
  (should
   (featurep 'etm-layout)))

(ert-deftest test-etm-layout-manual-loadable
    ()
  (require 'etm-layout-manual)
  (should
   (featurep 'etm-layout-manual)))

(ert-deftest test-etm-layout-auto-loadable
    ()
  (require 'etm-layout-auto)
  (should
   (featurep 'etm-layout-auto)))

(provide 'test-etm)

(when
    (not load-file-name)
  (message "test-etm.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))