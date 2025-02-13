;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-layout.el

(require 'ert)

(ert-deftest test-etm-layout-loadable
    ()
  (require 'etm-layout)
  (should
   (featurep 'etm-layout)))

(ert-deftest test-etm-layout-dependencies-loadable
    ()
  (should
   (featurep 'etm-layout-core))
  (should
   (featurep 'etm-layout-save))
  (should
   (featurep 'etm-layout-window))
  (should
   (featurep 'etm-layout-load)))

(provide 'test-etm-layout)

(when
    (not load-file-name)
  (message "test-etm-layout.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))