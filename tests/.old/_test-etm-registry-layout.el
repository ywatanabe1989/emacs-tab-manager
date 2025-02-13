;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 19:40:07>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-registry-layout.el
;;; test-etm-layout.el --- Tests for etm-layout.el -*- lexical-binding: t -*-

(require 'ert)
(require 'etm-layout)

(ert-deftest test-etm-layout-set-get
    ()
  (let
      ((etm-saved-layouts nil))
    (etm-layout-set "test"
                    (lambda
                      ()
                      t))
    (should
     (functionp
      (etm-layout-get "test")))))

(ert-deftest test-etm-layout-remove
    ()
  (let
      ((etm-saved-layouts nil))
    (etm-layout-set "test"
                    (lambda
                      ()
                      t))
    (should
     (etm-layout-get "test"))
    (etm-layout-remove "test")
    (should-not
     (etm-layout-get "test"))))

(ert-deftest test-etm-layout-list
    ()
  (let
      ((etm-saved-layouts nil))
    (etm-layout-set "test1"
                    (lambda
                      ()
                      t))
    (etm-layout-set "test2"
                    (lambda
                      ()
                      t))
    (should
     (equal
      (etm-layout-list)
      '("test2" "test1")))))

(provide 'test-etm-layout)

(provide 'test-etm-registry-layout)

(when
    (not load-file-name)
  (message "test-etm-registry-layout.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))