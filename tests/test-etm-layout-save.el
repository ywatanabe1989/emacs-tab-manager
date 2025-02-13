;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-layout-save.el

(require 'ert)

(ert-deftest test-etm-layout-save-loadable
    ()
  (require 'etm-layout-save)
  (should
   (featurep 'etm-layout-save)))

(ert-deftest test-etm-layout-save-functions-exist
    ()
  (should
   (fboundp 'etm-layout-save))
  (should
   (fboundp '--etm-layout-capture-current-layout)))

(provide 'test-etm-layout-save)

(when
    (not load-file-name)
  (message "test-etm-layout-save.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))