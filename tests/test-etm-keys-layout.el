;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-keys-layout.el

(ert-deftest test-etm-keys-layout-loadable
    ()
  (require 'etm-keys-layout)
  (should
   (featurep 'etm-keys-layout)))

(ert-deftest test-etm-keys-layout-bindings
    ()
  (should
   (eq
    (lookup-key etm-command-map
                (kbd "1"))
    'etm-close-others))
  (should
   (eq
    (lookup-key etm-command-map
                (kbd "2"))
    'etm-new))
  (should
   (eq
    (lookup-key etm-command-map
                (kbd "n"))
    'etm-new))
  (should
   (eq
    (lookup-key etm-command-map
                (kbd "r"))
    'etm-rename))
  (should
   (commandp
    (lookup-key global-map
                (kbd "M-w")))))

(provide 'test-etm-keys-layout)

(when
    (not load-file-name)
  (message "test-etm-keys-layout.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))