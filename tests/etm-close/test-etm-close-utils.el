;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-close-utils.el

(require 'ert)
(require 'etm-close-utils)

(ert-deftest test-etm-close-by-id
    ()
  (tab-bar-mode 1)
  (tab-bar-new-tab)
  (let
      ((num-tabs
        (length
         (tab-bar-tabs))))
    (--etm-close-by-id 2)
    (should
     (=
      (length
       (tab-bar-tabs))
      (1- num-tabs)))))

(ert-deftest test-etm-close-and-next
    ()
  (tab-bar-mode 1)
  (tab-bar-new-tab)
  (tab-bar-rename-tab "tab1")
  (tab-bar-new-tab)
  (tab-bar-rename-tab "tab2")
  (let
      ((current-name "tab2"))
    (--etm-close-and-next)
    (should-not
     (member current-name
             (mapcar
              (lambda
                (tab)
                (alist-get 'name tab))
              (tab-bar-tabs))))))

(ert-deftest test-etm-close-by-name-and-prev
    ()
  (tab-bar-mode 1)
  (tab-bar-new-tab)
  (let
      ((prev-name
        (alist-get 'name
                   (tab-bar--current-tab))))
    (tab-bar-new-tab)
    (--etm-close-by-name-and-prev)
    (should
     (string= prev-name
              (alist-get 'name
                         (tab-bar--current-tab))))))

(provide 'test-etm-close-utils)

(when
    (not load-file-name)
  (message "test-etm-close-utils.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))