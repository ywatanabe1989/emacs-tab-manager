;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-09 19:30:01>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/etm-core/test-etm-core-variables.el

(require 'ert)

(ert-deftest test-etm-core-variables-loadable
    ()
  (require 'etm-core-variables)
  (should
   (featurep 'etm-core-variables)))

(ert-deftest test-etm-core-variables-constants-exist
    ()
  (require 'etm-core-variables)
  (should (boundp 'etm-version))
  (should (boundp 'etm-default-buffer-types))
  (should (boundp 'etm-registered-buffer-types)))

(provide 'test-etm-core-variables)

(when
    (not load-file-name)
  (message "test-etm-core-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))