;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-09 19:33:01>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/etm-core/test-etm-core-helpers.el

(require 'ert)

(ert-deftest test-etm-core-helpers-loadable
    ()
  (require 'etm-core-helpers)
  (should
   (featurep 'etm-core-helpers)))

(ert-deftest test-etm-core-helpers-functions-exist
    ()
  ;; Load required modules
  (require 'etm-core-ssh-helpers)
  (require 'etm-core-helpers)
  
  ;; Test SSH helper functions
  (should (fboundp '--etm-ssh-select-host))
  (should (fboundp '--etm-ssh-rename-username))
  (should (fboundp '--etm-vterm-new)))

(provide 'test-etm-core-helpers)

(when
    (not load-file-name)
  (message "test-etm-core-helpers.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))