;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 16:02:35>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-keys-buffer.el

(require 'etm-keys-buffer)

(ert-deftest test-etm-keys-buffer-bindings
    ()
  (should
   (eq
    (lookup-key etm-command-map
                (kbd "H"))
    #'etm-buffer-set-home))
  (should
   (eq
    (lookup-key etm-command-map
                (kbd "S"))
    #'etm-buffer-set-semi-home))
  (should
   (eq
    (lookup-key etm-command-map
                (kbd "R"))
    #'etm-buffer-set-results))
  (should
   (eq
    (lookup-key etm-command-map
                (kbd "h"))
    #'etm-buffer-jump-to-home))
  (should
   (eq
    (lookup-key etm-command-map
                (kbd "s"))
    #'etm-buffer-jump-to-semi-home))
  (should
   (eq
    (lookup-key etm-command-map
                (kbd "r"))
    'etm-rename))
  (should
   (eq
    (lookup-key etm-command-map
                (kbd "k"))
    'etm-buffer-kill-or-bury)))

(provide 'test-etm-keys-buffer)

(when
    (not load-file-name)
  (message "test-etm-keys-buffer.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))