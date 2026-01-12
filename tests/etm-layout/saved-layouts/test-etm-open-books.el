;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-books.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-books

;;; Code:

(require 'ert)
(require 'etm-open-books)

;; Add your tests here
;; (ert-deftest test-etm-open-books-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-books.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-05-23 07:27:10>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-books.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; (defun etm-open-books ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (progn
;;     ;; No SSH connection to register
;;     (--etm-layout-create-from-positions "books"
;;                                         '((file "~/.claude/programming-books/" 0 1 119 60 nil)
;;                                           (shell "~/.claude/programming-books/" 119 1 119 60 nil))
;;                                         "l")))
;; 
;; (defalias 'books 'etm-open-books)
;; 
;; 
;; (provide 'etm-open-books)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-books.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-books.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-books.el ends here
