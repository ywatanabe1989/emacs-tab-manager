;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 19:29:27>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-close.el
;;; test-etm-close.el --- Tests for etm-close.el -*- lexical-binding: t -*-

(require 'ert)
(require 'etm-close)

(ert-deftest test-etm-close-all
    ()
  (tab-bar-mode 1)
  (tab-new)
  (tab-new)
  (should
   (>
    (length
     (tab-bar-tabs))
    1))
  (etm-close-all)
  (should
   (=
    (length
     (tab-bar-tabs))
    1))
  (tab-bar-mode -1))

(ert-deftest test-etm-close-others
    ()
  (tab-bar-mode 1)
  (let
      ((initial-tabs
        (length
         (tab-bar-tabs))))
    (tab-new)
    (tab-rename "test-1")
    (tab-new)
    (tab-rename "test-2")
    (etm-navigation-jump-by-name "test-1")
    (etm-close-others)
    (should
     (=
      (length
       (tab-bar-tabs))
      initial-tabs))
    (should
     (string=
      (alist-get 'name
                 (tab-bar--current-tab))
      "test-1")))
  (tab-bar-mode -1))

(ert-deftest test-etm-close-by-name
    ()
  (tab-bar-mode 1)
  (let
      ((initial-tabs
        (length
         (tab-bar-tabs))))
    (tab-new)
    (tab-rename "test-tab")
    (should
     (etm-close-by-name "test-tab"))
    (should
     (=
      (length
       (tab-bar-tabs))
      initial-tabs)))
  (tab-bar-mode -1))

(provide 'test-etm-close)

(when
    (not load-file-name)
  (message "test-etm-close.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))