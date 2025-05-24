;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-21 11:00:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/tests/etm-layout/test-etm-layout-open.el

(require 'ert)
(add-to-list 'load-path (expand-file-name "../../etm-layout" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../etm-core" (file-name-directory load-file-name)))
(require 'etm-core-variables-custom)
(require 'etm-layout-load)
(require 'etm-layout-open)

(ert-deftest test-etm-layout-list-available ()
  "Test listing available layouts."
  (let ((layouts (etm-layout-list-available)))
    ;; We know there are some layouts in the saved-layouts directory
    (should (listp layouts))
    (should (member "claude" layouts))
    (should (member "inits" layouts))))

(ert-deftest test-etm-layout-function-name ()
  "Test generating function name from layout name."
  (should (eq 'etm-open-claude (etm-layout-function-name "claude")))
  (should (eq 'etm-open-inits (etm-layout-function-name "inits"))))

(ert-deftest test-etm-layout-file-path ()
  "Test generating file path from layout name."
  (let ((path (etm-layout-file-path "claude")))
    (should (stringp path))
    (should (string-match "etm-open-claude\\.el$" path))
    (should (string-match "saved-layouts" path))))

(ert-deftest test-etm-layout-open-file-exists ()
  "Test that layout files exist for known layouts."
  (let ((path (etm-layout-file-path "claude")))
    (should (file-exists-p path))))

(provide 'test-etm-layout-open)