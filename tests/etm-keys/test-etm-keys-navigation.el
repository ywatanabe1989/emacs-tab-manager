;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-keys-navigation.el

(require 'ert)

(ert-deftest test-etm-keys-navigation-loadable
    ()
  (require 'etm-keys-navigation)
  (should
   (featurep 'etm-keys-navigation)))

(ert-deftest test-etm-keys-navigation-m1-bound
    ()
  (should
   (commandp
    (lookup-key global-map
                (kbd "M-1")))))

(ert-deftest test-etm-keys-navigation-m2-bound
    ()
  (should
   (commandp
    (lookup-key global-map
                (kbd "M-2")))))

(ert-deftest test-etm-keys-navigation-m3-bound
    ()
  (should
   (commandp
    (lookup-key global-map
                (kbd "M-3")))))

(ert-deftest test-etm-keys-navigation-m4-bound
    ()
  (should
   (commandp
    (lookup-key global-map
                (kbd "M-4")))))

(ert-deftest test-etm-keys-navigation-m5-bound
    ()
  (should
   (commandp
    (lookup-key global-map
                (kbd "M-5")))))

(ert-deftest test-etm-keys-navigation-m6-bound
    ()
  (should
   (commandp
    (lookup-key global-map
                (kbd "M-6")))))

(ert-deftest test-etm-keys-navigation-m7-bound
    ()
  (should
   (commandp
    (lookup-key global-map
                (kbd "M-7")))))

(ert-deftest test-etm-keys-navigation-m8-bound
    ()
  (should
   (commandp
    (lookup-key global-map
                (kbd "M-8")))))

(ert-deftest test-etm-keys-navigation-m9-bound
    ()
  (should
   (commandp
    (lookup-key global-map
                (kbd "M-9")))))

(provide 'test-etm-keys-navigation)

(when
    (not load-file-name)
  (message "test-etm-keys-navigation.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))