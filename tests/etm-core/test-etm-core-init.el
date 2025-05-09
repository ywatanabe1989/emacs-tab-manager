;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 09:06:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/etm-core/test-etm-core-init.el

(require 'ert)

(ert-deftest test-etm-init-loads
    ()
  (require 'etm-core-init)
  (should
   (featurep 'etm-core-init)))

(ert-deftest test-etm-init-function
    ()
  ;; First load the required modules
  (require 'etm-core-init)
  (require 'etm-layout)
  (require 'etm-layout-load)
  (require 'etm-core-variables)
  
  ;; The function should be defined
  (should (fboundp 'etm-init))
  
  ;; Call etm-init function
  (etm-init)
  
  ;; Verify settings are correctly applied
  (should tab-bar-mode)
  (should (eq tab-bar-show etm-show-tab-bar))
  (should tab-bar-tab-hints)
  (should tab-bar-name-truncated)
  (should-not tab-bar-auto-width)
  (should (eq tab-bar-new-tab-to 'right))
  (should-not tab-bar-close-button-show))

(provide 'test-etm-core-init)

(when
    (not load-file-name)
  (message "test-etm-core-init.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))