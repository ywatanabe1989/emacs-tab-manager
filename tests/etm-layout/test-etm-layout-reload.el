;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-24 15:28:30>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/etm-layout/test-etm-layout-reload.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)

(ert-deftest test-etm-layout-reload-loadable ()
  "Test that etm-layout-open can be loaded."
  (require 'etm-layout-open)
  (should (featurep 'etm-layout-open)))

(ert-deftest test-etm-layout-reload-function-exists ()
  "Test that etm-layout-reload function exists."
  (require 'etm-layout-open)
  (should (fboundp 'etm-layout-reload)))

(provide 'test-etm-layout-reload)

(when (not load-file-name)
  (message "test-etm-layout-reload.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))