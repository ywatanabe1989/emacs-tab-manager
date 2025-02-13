;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm.el

(require 'ert)

(ert-deftest test-etm-loadable
    ()
  (require 'etm)
  (should
   (featurep 'etm)))

(ert-deftest test-etm-variables-loadable
    ()
  (should
   (featurep 'etm-variables)))

(ert-deftest test-etm-buffer-loadable
    ()
  (should
   (featurep 'etm-buffer)))

(ert-deftest test-etm-close-loadable
    ()
  (should
   (featurep 'etm-close)))

(ert-deftest test-etm-layout-loadable
    ()
  (should
   (featurep 'etm-layout)))

(ert-deftest test-etm-navigation-loadable
    ()
  (should
   (featurep 'etm-navigation)))

(ert-deftest test-etm-keys-loadable
    ()
  (should
   (featurep 'etm-keys)))

(ert-deftest test-etm-init-loadable
    ()
  (should
   (featurep 'etm-init)))

(provide 'test-etm)

(when
    (not load-file-name)
  (message "test-etm.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))