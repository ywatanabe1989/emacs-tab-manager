;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-buffer-getters.el

(require 'ert)
(require 'etm-buffer-getters)

(ert-deftest test-etm-buffer-get-basic
    ()
  (let
      ((etm-registered-buffers
        '(("tab1" .
           (("home" . "buffer1"))))))
    (should
     (string=
      (etm-buffer-get "home"
                      '((name . "tab1")))
      "buffer1"))))

(ert-deftest test-etm-buffer-get-nonexistent
    ()
  (let
      ((etm-registered-buffers
        '(("tab1" .
           (("home" . "buffer1"))))))
    (should
     (null
      (etm-buffer-get "results"
                      '((name . "tab1")))))))

(ert-deftest test-etm-buffer-get-wrong-tab
    ()
  (let
      ((etm-registered-buffers
        '(("tab1" .
           (("home" . "buffer1"))))))
    (should
     (null
      (etm-buffer-get "home"
                      '((name . "tab2")))))))

(provide 'test-etm-buffer-getters)

(provide 'test-etm-buffer-getters)

(when
    (not load-file-name)
  (message "test-etm-buffer-getters.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))