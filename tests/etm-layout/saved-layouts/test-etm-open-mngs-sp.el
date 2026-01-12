;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-mngs-sp.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-mngs-sp

;;; Code:

(require 'ert)
(require 'etm-open-mngs-sp)

;; Add your tests here
;; (ert-deftest test-etm-open-mngs-sp-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-mngs-sp.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-05-22 00:12:52>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-mngs-sp.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; (defun etm-open-mngs-sp ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (progn
;;     ;; No SSH connection to register
;;     (--etm-layout-create-from-positions "mngs-sp"
;;                                         '((file "~/proj/mngs_repo/" 0 1 80 61 "ywatanabe@sp")
;;                                           (shell "~/proj/mngs_repo/" 80 1 80 61 "ywatanabe@sp")
;;                                           (shell "~/proj/mngs_repo/" 160 1 80 61 "ywatanabe@sp"))
;;                                         "sp")))
;; 
;; (defalias 'mngs-sp 'etm-open-mngs-sp)
;; 
;; 
;; (provide 'etm-open-mngs-sp)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-mngs-sp.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-mngs-sp.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-mngs-sp.el ends here
