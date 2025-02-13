;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 19:51:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-registry-buffer.el
;;; test-etm-buffer.el --- Tests for etm-buffer.el -*- lexical-binding: t -*-

(require 'ert)
(require 'etm-buffer)

(ert-deftest test-etm-buffer-set-get
    ()
  (let
      ((etm-registered-buffers nil)
       (etm-registered-buffer-types
        '("home" "semi-home" "results")))
    (with-temp-buffer
      (rename-buffer "test-buffer")
      (etm-buffer-set "home")
      (should
       (--etm-buffer-registered-p "test-buffer" "home"))
      (should
       (string=
        (etm-buffer-get "home")
        "test-buffer")))))

(ert-deftest test---etm-buffer-registered-p
    ()
  (let
      ((etm-registered-buffers nil)
       (etm-registered-buffer-types
        '("home" "semi-home" "results")))
    (with-temp-buffer
      (rename-buffer "test-buffer")
      (etm-buffer-set "home")
      (should
       (--etm-buffer-registered-p "test-buffer"))
      (should
       (--etm-buffer-registered-p "test-buffer" "home"))
      (should-not
       (--etm-buffer-registered-p "test-buffer" "semi-home")))))

(ert-deftest test-etm-registry-kill-or-bury
    ()
  (let
      ((etm-registered-buffers nil)
       (etm-registered-buffer-types
        '("home" "semi-home" "results")))
    (with-temp-buffer
      (rename-buffer "test-buffer")
      (should
       (buffer-live-p
        (current-buffer)))
      (etm-buffer-set "home")
      (etm-buffer-kill-or-bury)
      (should
       (buffer-live-p
        (get-buffer "test-buffer")))
      (setq etm-registered-buffers nil)
      (etm-buffer-kill-or-bury)
      (should-not
       (get-buffer "test-buffer")))))

(provide 'test-etm-registry-buffer)

(when
    (not load-file-name)
  (message "test-etm-registry-buffer.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))