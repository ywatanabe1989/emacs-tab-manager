;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-scitex-sp.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-scitex-sp

;;; Code:

(require 'ert)
(require 'etm-open-scitex-sp)

;; Add your tests here
;; (ert-deftest test-etm-open-scitex-sp-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-scitex-sp.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-07-12 10:17:11>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-scitex-sp.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-scitex-sp ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "scitex-sp"
;;                                       '((file "~/proj/scitex_repo/" 0 1 80 62 "sp")
;;                                         (shell "~/proj/scitex_repo/" 80 1 80 62 "sp")
;;                                         (shell "~/proj/scitex_repo/" 160 1 80 62 "sp"))
;;                                       "sp"))
;; 
;; (defalias 'scitex-sp 'etm-open-scitex-sp)
;; 
;; 
;; (provide 'etm-open-scitex-sp)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-scitex-sp.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-scitex-sp.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-scitex-sp.el ends here
