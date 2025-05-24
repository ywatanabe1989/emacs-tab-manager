;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-24 14:40:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/etm-layout/test-etm-layout-host-selection.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'etm-core-ssh-helpers)

(ert-deftest test-etm-ssh-select-host-noninteractive ()
  "Test --etm-ssh-select-host returns localhost in noninteractive mode."
  (let ((noninteractive t))
    (should (string= (--etm-ssh-select-host) "localhost"))))

(ert-deftest test-etm-ssh-select-host-interactive ()
  "Test --etm-ssh-select-host prompts in interactive mode."
  (let ((noninteractive nil)
        (completing-read-called nil)
        (expected-host "test-host"))
    ;; Mock completing-read to avoid actual user input
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt collection &rest args)
                 (setq completing-read-called t)
                 expected-host))
              ((symbol-function '--etm-ssh-parse-dot-ssh)
               (lambda () 
                 (setq --etm-ssh-hostname-username 
                       '(("test-host" . "user") ("localhost" . "ywatanabe"))))))
      (should (string= (--etm-ssh-select-host) expected-host))
      (should completing-read-called))))

(ert-deftest test-etm-ssh-select-host-localhost-shortcut ()
  "Test --etm-ssh-select-host converts 'l' to 'localhost'."
  (let ((noninteractive nil))
    ;; Mock completing-read to return 'l'
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt collection &rest args) "l"))
              ((symbol-function '--etm-ssh-parse-dot-ssh)
               (lambda () 
                 (setq --etm-ssh-hostname-username 
                       '(("l" . "user") ("localhost" . "ywatanabe"))))))
      (should (string= (--etm-ssh-select-host) "localhost")))))

(provide 'test-etm-layout-host-selection)