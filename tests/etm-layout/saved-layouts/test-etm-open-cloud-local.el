;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-cloud-local.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-cloud-local

;;; Code:

(require 'ert)
(require 'etm-open-cloud-local)

;; Add your tests here
;; (ert-deftest test-etm-open-cloud-local-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-cloud-local.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-10-18 21:12:53>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-cloud-local.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-cloud-local ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "cloud-local"
;;                                       '((file "~/proj/scitex-cloud/" 0 1 80 62 "localhost")
;;                                         (shell "~/proj/scitex-cloud/" 80 1 80 62 "localhost")
;;                                         (shell "~/proj/scitex-cloud/" 160 1 80 62 "localhost"))
;;                                       "localhost"))
;; 
;; (defalias 'cloud-local 'etm-open-cloud-local)
;; 
;; 
;; (provide 'etm-open-cloud-local)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-cloud-local.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-cloud-local.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-cloud-local.el ends here
