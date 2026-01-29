;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-writer.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-writer

;;; Code:

(require 'ert)
(require 'etm-open-writer)

;; Add your tests here
;; (ert-deftest test-etm-open-writer-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-writer.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-11-10 00:14:57>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-writer.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-writer ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "writer"
;;                                       '((file "~/proj/scitex-writer/" 0 1 80 62 nil)
;;                                         (shell "~/proj/scitex-writer/" 80 1 80 62 nil)
;;                                         (shell "~/proj/scitex-writer/" 160 1 80 62 nil))
;;                                       nil))
;; 
;; (defalias 'writer 'etm-open-writer)
;; 
;; 
;; (provide 'etm-open-writer)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-writer.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-writer.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-writer.el ends here
