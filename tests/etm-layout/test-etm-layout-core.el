;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-layout-create.el
;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'ert)

(ert-deftest test-etm-layout-create-loadable
    ()
  (require 'etm-layout-create)
  (should
   (featurep 'etm-layout-create)))

(ert-deftest test-etm-layout-cleanup-exists
    ()
  (should
   (fboundp '--etm-layout-cleanup)))

(ert-deftest test-etm-layout-create-exists
    ()
  (should
   (fboundp '--etm-layout-create)))

(provide 'test-etm-layout-create)

(when
    (not load-file-name)
  (message "test-etm-layout-create.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))