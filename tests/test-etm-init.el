;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-init.el

(require 'ert)

(ert-deftest test-etm-init-loads
    ()
  (require 'etm-init)
  (should
   (featurep 'etm-init)))

(ert-deftest test-etm-init-function
    ()
  ;; First load the required module
  (require 'etm-init)
  ;; Now the function should be defined
  (should (fboundp 'etm-init))
  ;; Now call the function
  (etm-init)
  ;; Verify settings are correctly applied
  (should tab-bar-mode)
  (should (eq tab-bar-show etm-show-tab-bar))
  (should tab-bar-tab-hints)
  (should tab-bar-name-truncated)
  (should-not tab-bar-auto-width)
  (should (eq tab-bar-new-tab-to 'right))
  (should-not tab-bar-close-button-show))

(provide 'test-etm-init)

(when
    (not load-file-name)
  (message "test-etm-init.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))