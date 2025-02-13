;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-layout-core.el
;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'ert)

(ert-deftest test-etm-layout-core-loadable
    ()
  (require 'etm-layout-core)
  (should
   (featurep 'etm-layout-core)))

(ert-deftest test-etm-layout-cleanup-exists
    ()
  (should
   (fboundp '--etm-layout-cleanup)))

(ert-deftest test-etm-layout-create-exists
    ()
  (should
   (fboundp '--etm-layout-create)))

(provide 'test-etm-layout-core)

(when
    (not load-file-name)
  (message "test-etm-layout-core.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))