;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-variables.el

(require 'ert)

(ert-deftest test-etm-variables-loadable
    ()
  (require 'etm-core-variables)
  (should
   (featurep 'etm-core-variables)))

(ert-deftest test-etm-variables-constants-exist
    ()
  (should
   (boundp 'etm-version))
  (should
   (boundp 'etm-default-buffer-types)))

(provide 'test-etm-variables)

(when
    (not load-file-name)
  (message "test-etm-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))