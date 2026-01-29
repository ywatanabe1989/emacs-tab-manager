;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-cloud-dev.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-cloud-dev

;;; Code:

(require 'ert)
(require 'etm-open-cloud-dev)

;; Add your tests here
;; (ert-deftest test-etm-open-cloud-dev-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-cloud-dev.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-10-18 21:13:59>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-cloud-dev.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-cloud-dev ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "cloud-dev"
;;                                       '((file "~/proj/scitex-cloud/" 0 1 80 62 "localhost")
;;                                         (shell "~/proj/scitex-cloud/" 80 1 80 62 "localhost")
;;                                         (shell "~/proj/scitex-cloud/" 160 1 80 62 "localhost"))
;;                                       "localhost"))
;; 
;; (defalias 'cloud-dev 'etm-open-cloud-dev)
;; 
;; 
;; (provide 'etm-open-cloud-dev)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-cloud-dev.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-cloud-dev.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-cloud-dev.el ends here
