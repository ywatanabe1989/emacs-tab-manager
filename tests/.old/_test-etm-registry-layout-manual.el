;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 19:40:06>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-registry-layout-manual.el
;;; test-etm-layout-manual.el --- Tests for etm-layout-manual.el -*- lexical-binding: t -*-

(require 'ert)
(require 'etm-layout-manual)

(ert-deftest test-etm-init
    ()
  (let
      ((tab-name "test-tab"))
    (etm-new tab-name)
    (should
     (string=
      (alist-get 'name
                 (tab-bar--current-tab))
      tab-name))))

(ert-deftest test-etm-base-left-1-right-1
    ()
  (let
      ((tab-name "test-tab")
       (left-path "/tmp")
       (right-path "/tmp"))
    (--etm-base-left-1-right-1 tab-name left-path right-path)
    (should
     (string=
      (alist-get 'name
                 (tab-bar--current-tab))
      tab-name))
    (should
     (=
      (length
       (window-list))
      2))))

(ert-deftest test-etm-base-left-2-right-1
    ()
  (let
      ((tab-name "test-tab")
       (upper-left-path "/tmp")
       (lower-left-path "/tmp")
       (right-path "/tmp"))
    (--etm-base-left-2-right-1 tab-name upper-left-path lower-left-path right-path)
    (should
     (string=
      (alist-get 'name
                 (tab-bar--current-tab))
      tab-name))
    (should
     (=
      (length
       (window-list))
      3))))

(provide 'test-etm-layout-manual)

(provide 'test-etm-registry-layout-manual)

(when
    (not load-file-name)
  (message "test-etm-registry-layout-manual.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))