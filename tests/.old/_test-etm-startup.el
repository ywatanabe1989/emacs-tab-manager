;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 19:08:58>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-startup.el
;;; test-etm-startup.el --- Tests for etm-startup.el -*- lexical-binding: t -*-

(require 'ert)
(require 'etm-startup)

(ert-deftest test-etm-startup-loadable
    ()
  (should
   (featurep 'etm-startup)))

(ert-deftest test-etm-startup
    ()
  (tab-bar-mode 1)
  (etm-startup)
  (should
   (string=
    (alist-get 'name
               (tab-bar--current-tab))
    "default"))
  (tab-bar-mode -1))

(ert-deftest test-etm-startup-hook
    ()
  (let
      ((after-init-hook
        '()))
    (add-hook 'after-init-hook #'etm-startup)
    (should
     (member #'etm-startup after-init-hook))))

(provide 'test-etm-startup)

(when
    (not load-file-name)
  (message "test-etm-startup.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))