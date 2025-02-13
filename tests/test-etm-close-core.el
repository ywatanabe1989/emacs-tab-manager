;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-close-core.el

(require 'ert)
(require 'etm-close-core)

(ert-deftest test-etm-close-by-name
    ()
  (tab-bar-mode 1)
  (tab-bar-new-tab)
  (tab-bar-rename-tab "test-tab")
  (should
   (etm-close-by-name "test-tab"))
  (should-not
   (member "test-tab"
           (mapcar
            (lambda
              (tab)
              (alist-get 'name tab))
            (tab-bar-tabs)))))

(ert-deftest test-etm-close-by-name-last-tab
    ()
  (tab-bar-mode 1)
  (while
      (>
       (length
        (tab-bar-tabs))
       1)
    (tab-bar-close-tab))
  (should-not
   (etm-close-by-name "default")))

(ert-deftest test-etm-close-all
    ()
  (tab-bar-mode 1)
  (dotimes
      (_ 3)
    (tab-bar-new-tab))
  (etm-close-all)
  (should
   (=
    (length
     (tab-bar-tabs))
    1)))

(provide 'test-etm-close-core)

(provide 'test-etm-close-core)

(when
    (not load-file-name)
  (message "test-etm-close-core.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))