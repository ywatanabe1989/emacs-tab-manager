;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-navigation.el

(require 'ert)

(ert-deftest test-etm-navigation-loadable
    ()
  (require 'etm-navigation)
  (should
   (featurep 'etm-navigation)))

(ert-deftest test-etm-navigation-functions-exist
    ()
  (should
   (fboundp 'etm-navigation-jump-by-index))
  (should
   (fboundp 'etm-navigation-jump-by-name))
  (should
   (fboundp 'etm-navigation-move)))

(provide 'test-etm-navigation)

(when
    (not load-file-name)
  (message "test-etm-navigation.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))