;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-layout-window.el

(require 'ert)

(ert-deftest test-etm-layout-window-loadable
    ()
  (require 'etm-layout-window)
  (should
   (featurep 'etm-layout-window)))

(ert-deftest test-etm-layout-window-functions-exist
    ()
  (require 'etm-layout-window)
  (should
   (fboundp '--etm-layout-init-windows))
  (should
   (fboundp '--etm-layout-setup-window)))

(provide 'test-etm-layout-window)

(when
    (not load-file-name)
  (message "test-etm-layout-window.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))