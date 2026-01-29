;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-scicloud.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-scicloud

;;; Code:

(require 'ert)
(require 'etm-open-scicloud)

;; Add your tests here
;; (ert-deftest test-etm-open-scicloud-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-scicloud.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-05-22 14:17:53>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-scicloud.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; (defun etm-open-scicloud ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (progn
;;     ;; No SSH connection to register
;;     (--etm-layout-create-from-positions "scicloud"
;;                                         '((file "~/proj/SciTeX-Cloud/" 0 1 80 62 nil)
;;                                           (shell "~/proj/SciTeX-Cloud/" 80 1 80 62 nil)
;;                                           (shell "~/proj/SciTeX-Cloud/" 160 1 80 62 nil))
;;                                         "localhost")))
;; 
;; (defalias 'scicloud 'etm-open-scicloud)
;; 
;; 
;; (provide 'etm-open-scicloud)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-scicloud.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-scicloud.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-scicloud.el ends here
