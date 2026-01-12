;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-scholar.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-scholar

;;; Code:

(require 'ert)
(require 'etm-open-scholar)

;; Add your tests here
;; (ert-deftest test-etm-open-scholar-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-scholar.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-10-17 22:42:41>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-scholar.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-scholar ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "scholar"
;;                                       '((file "~/proj/scitex_repo/src/scitex/scholar/" 0 1 120 62
;;                                               "localhost")
;;                                         (shell "~/proj/scitex_repo/src/scitex/scholar/" 120 1 120 62
;;                                                "localhost"))
;;                                       "localhost"))
;; 
;; (defalias 'scholar 'etm-open-scholar)
;; 
;; 
;; (provide 'etm-open-scholar)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-scholar.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-scholar.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-scholar.el ends here
