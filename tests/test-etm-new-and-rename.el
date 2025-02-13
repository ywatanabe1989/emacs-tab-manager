;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-new-and-rename.el

(require 'ert)

(ert-deftest test-etm-new-and-rename-loadable
    ()
  (require 'etm-new-and-rename)
  (should
   (featurep 'etm-new-and-rename)))

(ert-deftest test-etm-new-and-rename-functions-exist
    ()
  (should
   (fboundp 'etm-new))
  (should
   (fboundp 'etm-rename)))

(provide 'test-etm-new-and-rename)

(when
    (not load-file-name)
  (message "test-etm-new-and-rename.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))