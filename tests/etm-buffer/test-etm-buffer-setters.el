;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-buffer-setters.el

(require 'ert)
(require 'etm-buffer-setters)

(ert-deftest test-etm-buffer-set-basic
    ()
  (with-temp-buffer
    (let*
        ((buffer-name
          (buffer-name))
         (etm-registered-buffer-types
          '("home"))
         (etm-custom-buffer-types nil))
      (etm-buffer-set "home" "tab1" buffer-name)
      (should
       (--etm-buffer-registered-p buffer-name "home")))))

(ert-deftest test-etm-buffer-set-invalid-type
    ()
  (should-error
   (etm-buffer-set "invalid-type")
   :type 'error))

(ert-deftest test-etm-buffer-define-buffer-type-setter-function
    ()
  (let
      ((etm-registered-buffer-types
        '("test")))
    (etm-buffer-define-buffer-type-setter-function "test")
    (should
     (fboundp 'etm-buffer-set-test))))

(ert-deftest test-etm-buffer-define-buffer-type-setter-functions
    ()
  (let
      ((etm-registered-buffer-types
        '("home" "semi-home")))
    (etm-buffer-define-buffer-type-setter-functions)
    (should
     (fboundp 'etm-buffer-set-home))
    (should
     (fboundp 'etm-buffer-set-semi-home))))

(provide 'test-etm-buffer-setters)

(when
    (not load-file-name)
  (message "test-etm-buffer-setters.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))