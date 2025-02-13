;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-buffer-jumpers.el

(require 'ert)
(require 'etm-buffer-jumpers)

(ert-deftest test-etm-buffer-jump-to-existing
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
       (etm-buffer-jump-to "home"))
      (should
       (string=
        (buffer-name)
        "test-buffer")))))

(ert-deftest test-etm-buffer-jump-to-nonexistent
    ()
  (let
      ((etm-registered-buffers nil))
    (with-current-buffer
        (get-buffer-create "*Messages*")
      (let
          ((message-text nil))
        (setq message-text
              (etm-buffer-jump-to "home"))
        (should
         (string= message-text
                  "No home buffer set for current tab"))))))

(ert-deftest test-etm-define-buffer-jump-to-function
    ()
  (etm-define-buffer-jump-to-function "test")
  (should
   (fboundp 'etm-buffer-jump-to-test)))

(ert-deftest test-etm-define-buffer-jump-to-functions
    ()
  (let
      ((etm-registered-buffer-types
        '("home" "semi-home")))
    (etm-define-buffer-jump-to-functions)
    (should
     (fboundp 'etm-buffer-jump-to-home))
    (should
     (fboundp 'etm-buffer-jump-to-semi-home))))

(provide 'test-etm-buffer-jumpers)

(when
    (not load-file-name)
  (message "test-etm-buffer-jumpers.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))