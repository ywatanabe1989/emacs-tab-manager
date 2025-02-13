;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 19:37:44>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-nav.el
;;; test-etm-navigation.el --- Tests for etm-navigation.el -*- lexical-binding: t -*-

(require 'ert)
(require 'etm-navigation)

(ert-deftest test-etm-navigation-jump-to
    ()
  (tab-bar-mode 1)
  (tab-new)
  (tab-new)
  (etm-navigation-jump-to 1)
  (should
   (=
    (tab-bar--current-tab-index)
    0))
  (etm-navigation-jump-to 2)
  (should
   (=
    (tab-bar--current-tab-index)
    1))
  (tab-bar-mode -1))

(ert-deftest test-etm-navigation-jump-by-name
    ()
  (tab-bar-mode 1)
  (tab-new)
  (tab-rename "test-tab")
  (tab-new)
  (etm-navigation-jump-by-name "test-tab")
  (should
   (string=
    (alist-get 'name
               (tab-bar--current-tab))
    "test-tab"))
  (tab-bar-mode -1))

(ert-deftest test-etm-navigation-move
    ()
  (tab-bar-mode 1)
  (tab-new)
  (tab-rename "test-tab")
  (tab-new)
  (let
      ((initial-index
        (tab-bar--current-tab-index)))
    (etm-navigation-move 1)
    (should
     (=
      (tab-bar--current-tab-index)
      (1+ initial-index))))
  (tab-bar-mode -1))

(provide 'test-etm-navigation)

(provide 'test-etm-nav)

(when
    (not load-file-name)
  (message "test-etm-nav.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))