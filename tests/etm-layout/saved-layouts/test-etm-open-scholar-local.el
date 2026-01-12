;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-scholar-local.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-scholar-local

;;; Code:

(require 'ert)
(require 'etm-open-scholar-local)

;; Add your tests here
;; (ert-deftest test-etm-open-scholar-local-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-scholar-local.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-08-09 01:25:10>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-scholar-local.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-scholar-local ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "scholar-local"
;;                                       '((file "~/proj/scitex_repo/scholar/" 0 1 80 62 "localhost")
;;                                         (shell "~/proj/scitex_repo/scholar/" 80 1 80 62 "localhost")
;;                                         (shell "~/proj/scitex_repo/scholar/" 160 1 80 62 "localhost"))
;;                                       "localhost"))
;; 
;; (defalias 'scholar-local 'etm-open-scholar-local)
;; 
;; 
;; (provide 'etm-open-scholar-local)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-scholar-local.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-scholar-local.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-scholar-local.el ends here
