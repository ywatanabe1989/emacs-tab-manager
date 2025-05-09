;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 09:08:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/etm-tabs/test-etm-new-and-rename.el

(require 'ert)

(ert-deftest test-etm-new-and-rename-loadable
    ()
  (require 'etm-tabs-new-and-rename)
  (should
   (featurep 'etm-tabs-new-and-rename)))

(ert-deftest test-etm-new-and-rename-functions-exist
    ()
  ;; First load the required module
  (require 'etm-tabs-new-and-rename)
  ;; Now the functions should be defined
  (should (fboundp 'etm-new))
  (should (fboundp 'etm-rename)))

(provide 'test-etm-tabs-new-and-rename)

(when
    (not load-file-name)
  (message "test-etm-tabs-new-and-rename.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))