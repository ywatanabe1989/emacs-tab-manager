;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-close.el

(ert-deftest test-etm-close-loadable
    ()
  (require 'etm-close)
  (should
   (featurep 'etm-close)))

(ert-deftest test-etm-close-dependencies-loadable
    ()
  (should
   (featurep 'etm-close-core))
  (should
   (featurep 'etm-close-utils)))

(provide 'test-etm-close)

(when
    (not load-file-name)
  (message "test-etm-close.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))