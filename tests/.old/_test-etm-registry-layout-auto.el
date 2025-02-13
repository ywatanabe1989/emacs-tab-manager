;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 19:40:06>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-registry-layout-auto.el

(require 'ert)
(require 'etm-layout)

(ert-deftest test-etm-capture-current-layout
    ()
  (let
      ((layout-name "test-layout"))
    (tab-bar-mode 1)
    (tab-new)
    (split-window-horizontally)
    (find-file "/tmp/test.txt")
    (other-window 1)
    (let
        ((layout-string
          (--etm-layout-capture-current-layout layout-name)))
      (should
       (stringp layout-string))
      (should
       (string-match-p layout-name layout-string)))
    (tab-bar-mode -1)))

(ert-deftest test-etm-create
    ()
  (let
      ((tab-name "test-tab")
       (num-left 1)
       (num-right 1)
       (window-configs
        '((file . "/tmp")
          (shell . "/tmp"))))
    (--etm-layout-create tab-name num-left num-right window-configs)
    (should
     (string=
      (alist-get 'name
                 (tab-bar--current-tab))
      tab-name))
    (should
     (=
      (length
       (window-list))
      (+ num-left num-right)))))

(provide 'test-etm-layout-auto)

(provide 'test-etm-registry-layout-auto)

(when
    (not load-file-name)
  (message "test-etm-registry-layout-auto.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))