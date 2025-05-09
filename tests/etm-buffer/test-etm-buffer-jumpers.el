;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-buffer-jumpers.el

(require 'ert)
(require 'etm-buffer-jumpers)

(ert-deftest test-etm-navigation-jump-by-buffer-type-existing
    ()
  (with-temp-buffer
    (let
        ((etm-registered-buffers
          '(("tab1" .
             (("home" . "test-buffer")))))
         (current-tab
          '((name . "tab1"))))
      (rename-buffer "test-buffer")
      (should
       (etm-navigation-jump-by-buffer-type "home"))
      (should
       (string=
        (buffer-name)
        "test-buffer")))))

(ert-deftest test-etm-navigation-jump-by-buffer-type-nonexistent
    ()
  (let
      ((etm-registered-buffers nil))
    (with-current-buffer
        (get-buffer-create "*Messages*")
      (let
          ((message-text nil))
        (setq message-text
              (etm-navigation-jump-by-buffer-type "home"))
        (should
         (string= message-text
                  "No home buffer set for current tab"))))))

(ert-deftest test-etm-buffer-define-buffer-type-jumper-function
    ()
  (etm-buffer-define-buffer-type-jumper-function "test")
  (should
   (fboundp 'etm-navigation-jump-by-buffer-type-test)))

(ert-deftest test-etm-buffer-define-buffer-type-jumper-functions
    ()
  (let
      ((etm-registered-buffer-types
        '("home" "semi-home")))
    (etm-buffer-define-buffer-type-jumper-functions)
    (should
     (fboundp 'etm-navigation-jump-by-buffer-type-home))
    (should
     (fboundp 'etm-navigation-jump-by-buffer-type-semi-home))))

(provide 'test-etm-buffer-jumpers)

(when
    (not load-file-name)
  (message "test-etm-buffer-jumpers.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))