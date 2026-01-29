;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-crossref.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-crossref

;;; Code:

(require 'ert)
(require 'etm-open-crossref)

;; Add your tests here
;; (ert-deftest test-etm-open-crossref-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-crossref.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-12-05 22:35:06>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-crossref.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-crossref ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "crossref"
;;                                       '((file "~/proj/crossref_local/" 0 1 80 62 "ywatanabe@nas")
;;                                         (shell "~/proj/crossref_local/" 80 1 80 62 "ywatanabe@nas")
;;                                         (shell "~/proj/crossref_local/" 160 1 80 62 "ywatanabe@nas"))
;;                                       "nas"))
;; 
;; (defalias 'crossref 'etm-open-crossref)
;; 
;; 
;; (provide 'etm-open-crossref)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-crossref.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-crossref.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-crossref.el ends here
