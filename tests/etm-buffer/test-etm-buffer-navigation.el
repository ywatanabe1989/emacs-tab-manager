;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/etm-buffer/test-etm-buffer-navigation.el

(require 'ert)
(require 'etm-buffer-navigation)

(ert-deftest test-etm-buffer-navigation-loadable
    ()
  (should
   (featurep 'etm-buffer-navigation)))

(ert-deftest test-etm-buffer-navigation-functions-exist
    ()
  (should
   (fboundp 'etm-navigation-jump-by-buffer-type))
  (should
   (fboundp 'etm-navigation-jump-by-index))
  (should
   (fboundp 'etm-navigation-jump-by-name))
  (should
   (fboundp 'etm-navigation-move)))

(provide 'test-etm-buffer-navigation)

(when
    (not load-file-name)
  (message "test-etm-navigation.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))