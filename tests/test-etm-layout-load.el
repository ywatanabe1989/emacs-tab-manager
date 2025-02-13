;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-layout-load.el

(require 'ert)

(ert-deftest test-etm-layout-load-loadable
    ()
  (require 'etm-layout-load)
  (should
   (featurep 'etm-layout-load)))

(ert-deftest test-etm-layout-load-function-exists
    ()
  (should
   (fboundp '--etm-layout-load-all)))

(provide 'test-etm-layout-load)

(when
    (not load-file-name)
  (message "test-etm-layout-load.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))