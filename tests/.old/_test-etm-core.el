;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 19:13:05>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-init.el
;;; test-etm-init.el --- Tests for etm-init.el -*- lexical-binding: t -*-

(require 'ert)
(require 'etm-init)

(ert-deftest test-etm-init
    ()
  (let
      ((tab-bar-mode nil))
    (etm-init)
    (should tab-bar-mode)
    (should
     (eq tab-bar-new-tab-to 'right))
    (should-not tab-bar-close-button-show)))

(provide 'test-etm-init)

(when
    (not load-file-name)
  (message "test-etm-init.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))