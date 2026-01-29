;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-neurovsita-bimodality.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-neurovsita-bimodality

;;; Code:

(require 'ert)
(require 'etm-open-neurovsita-bimodality)

;; Add your tests here
;; (ert-deftest test-etm-open-neurovsita-bimodality-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-neurovsita-bimodality.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-09-04 07:00:14>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-neurovsita-bimodality.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-neurovsita-bimodality ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "neurovsita-bimodality"
;;                                       '((file "~/proj/neurovista/scripts/pac/analyze_bimodality/" 0 1 137
;;                                               70 "sp")
;;                                         (shell "~/proj/neurovista/scripts/pac/analyze_bimodality/" 137 1
;;                                                137 70 "sp"))
;;                                       "sp"))
;; 
;; (defalias 'neurovsita-bimodality 'etm-open-neurovsita-bimodality)
;; 
;; 
;; (provide 'etm-open-neurovsita-bimodality)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-neurovsita-bimodality.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-neurovsita-bimodality.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-neurovsita-bimodality.el ends here
