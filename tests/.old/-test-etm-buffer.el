;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-buffer.el
;;; test-etm-buffer.el
(require 'ert)

(ert-deftest test-etm-buffer-loadable
    ()
  (require 'etm-buffer)
  (should
   (featurep 'etm-buffer)))

(ert-deftest test-etm-buffer-dependencies-loadable
    ()
  (should
   (featurep 'etm-buffer-setters))
  (should
   (featurep 'etm-buffer-getters))
  (should
   (featurep 'etm-buffer-kill-or-bury))
  (should
   (featurep 'etm-buffer-jumpers))
  (should
   (featurep 'etm-buffer-checkers)))

(provide 'test-etm-buffer)

(when
    (not load-file-name)
  (message "test-etm-buffer.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))