;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 19:36:43>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-key-bindings.el

(require 'ert)
(require 'etm-key-bindings)

(ert-deftest test-etm-command-0-key
    ()
  (should
   (eq
    (lookup-key etm-command-map
                (kbd "0"))
    'etm-close)))

(ert-deftest test-etm-command-1-key
    ()
  (should
   (eq
    (lookup-key etm-command-map
                (kbd "1"))
    'etm-close-others)))

(ert-deftest test-etm-command-2-key
    ()
  (should
   (eq
    (lookup-key etm-command-map
                (kbd "2"))
    'tab-new)))

(ert-deftest test-etm-command-n-key
    ()
  (should
   (eq
    (lookup-key etm-command-map
                (kbd "n"))
    'tab-new)))

(ert-deftest test-etm-command-m-key
    ()
  (should
   (eq
    (lookup-key etm-command-map
                (kbd "m"))
    'etm-navigation-move)))

(ert-deftest test-etm-command-r-key
    ()
  (should
   (eq
    (lookup-key etm-command-map
                (kbd "r"))
    'etm-rename)))

(ert-deftest test-etm-global-mt-key
    ()
  (should
   (eq
    (lookup-key global-map
                (kbd "M-t"))
    'etm-command-map)))

(ert-deftest test-etm-global-m1-key
    ()
  (should
   (commandp
    (lookup-key global-map
                (kbd "M-1")))))

(ert-deftest test-etm-global-mh-key
    ()
  (should
   (commandp
    (lookup-key global-map
                (kbd "M-h")))))

(ert-deftest test-etm-global-mH-key
    ()
  (should
   (commandp
    (lookup-key global-map
                (kbd "M-H")))))

(ert-deftest test-etm-global-ms-key
    ()
  (should
   (commandp
    (lookup-key global-map
                (kbd "M-s")))))

(ert-deftest test-etm-global-mS-key
    ()
  (should
   (commandp
    (lookup-key global-map
                (kbd "M-S")))))

(ert-deftest test-etm-global-mr-key
    ()
  (should
   (commandp
    (lookup-key global-map
                (kbd "M-r")))))

(ert-deftest test-etm-global-mR-key
    ()
  (should
   (commandp
    (lookup-key global-map
                (kbd "M-R")))))

(provide 'test-etm-key-bindings)

(provide 'test-etm-key-bindings)

(when
    (not load-file-name)
  (message "test-etm-key-bindings.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))