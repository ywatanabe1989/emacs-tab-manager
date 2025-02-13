;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 19:37:32>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-new.el
;;; test-etm-new.el --- Tests for etm-new.el -*- lexical-binding: t -*-

(require 'ert)
(require 'etm-new)

(ert-deftest test-etm-new-function-exists
    ()
  (should
   (fboundp 'etm-new)))

(ert-deftest test-etm-new-is-interactive
    ()
  (should
   (commandp 'etm-new)))

(ert-deftest test-etm-new-creates-tab
    ()
  (tab-bar-mode 1)
  (let
      ((initial-tabs
        (length
         (tab-bar-tabs)))
       (test-name "test-tab"))
    (etm-new test-name)
    (should
     (=
      (length
       (tab-bar-tabs))
      (1+ initial-tabs)))
    (should
     (string=
      (alist-get 'name
                 (tab-bar--current-tab))
      test-name))
    (tab-bar-close-tab))
  (tab-bar-mode -1))

(provide 'test-etm-new)

(provide 'test-etm-new)

(when
    (not load-file-name)
  (message "test-etm-new.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))