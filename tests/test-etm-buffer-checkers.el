;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:49>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-buffer-checkers.el

(require 'ert)
(require 'etm-buffer-checkers)

(ert-deftest test-etm-buffer-registered-p-with-name-only
    ()
  (let
      ((etm-registered-buffers
        '(("tab1" .
           (("home" . "buffer1"))))))
    (should
     (--etm-buffer-registered-p "buffer1"))))

(ert-deftest test-etm-buffer-registered-p-with-type
    ()
  (let
      ((etm-registered-buffers
        '(("tab1" .
           (("home" . "buffer1"))))))
    (should
     (--etm-buffer-registered-p "buffer1" "home"))
    (should-not
     (--etm-buffer-registered-p "buffer1" "results"))))

(ert-deftest test-etm-buffer-registered-p-with-tab
    ()
  (let
      ((etm-registered-buffers
        '(("tab1" .
           (("home" . "buffer1")))
          ("tab2" .
           (("home" . "buffer2"))))))
    (should
     (--etm-buffer-registered-p "buffer1" nil
                                '((name . "tab1"))))
    (should-not
     (--etm-buffer-registered-p "buffer1" nil
                                '((name . "tab2"))))))

(ert-deftest test-etm-buffer-protected-p
    ()
  (let
      ((etm-protected-buffers
        '("*scratch*" "*Messages*")))
    (should
     (--etm-buffer-protected-p "*scratch*"))
    (should-not
     (--etm-buffer-protected-p "regular-buffer"))))

(provide 'test-etm-buffer-checkers)

(when
    (not load-file-name)
  (message "test-etm-buffer-checkers.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))