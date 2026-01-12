;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-stx-sp.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-stx-sp

;;; Code:

(require 'ert)
(require 'etm-open-stx-sp)

;; Add your tests here
;; (ert-deftest test-etm-open-stx-sp-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-stx-sp.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-06-13 22:19:00>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-stx-sp.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-stx-sp ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "stx-sp"
;;                                       '((file "~/proj/SciTeX-Code/" 0 1 80 62 "sp")
;;                                         (shell "~/proj/SciTeX-Code/" 80 1 80 62 "sp")
;;                                         (shell "~/proj/SciTeX-Code/" 160 1 80 62 "sp"))
;;                                       "sp"))
;; 
;; (defalias 'stx-sp 'etm-open-stx-sp)
;; 
;; 
;; (provide 'etm-open-stx-sp)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-stx-sp.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-stx-sp.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-stx-sp.el ends here
