;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-keys.el

(require 'ert)

(ert-deftest test-etm-keys-loadable
    ()
  (require 'etm-keys)
  (should
   (featurep 'etm-keys)))

(ert-deftest test-etm-keys-dependencies-loadable
    ()
  (should
   (featurep 'etm-keys-command-map))
  (should
   (featurep 'etm-keys-buffer))
  (should
   (featurep 'etm-keys-layout))
  (should
   (featurep 'etm-keys-navigation)))

(provide 'test-etm-keys)

(when
    (not load-file-name)
  (message "test-etm-keys.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))