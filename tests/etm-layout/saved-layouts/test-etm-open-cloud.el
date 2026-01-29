;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-cloud.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-cloud

;;; Code:

(require 'ert)
(require 'etm-open-cloud)

;; Add your tests here
;; (ert-deftest test-etm-open-cloud-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-cloud.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-10-13 11:52:53>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-cloud.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-cloud ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "cloud"
;;                                       '((file "~/proj/scitex-cloud/" 0 1 80 62 nil)
;;                                         (shell "~/proj/scitex-cloud/" 80 1 80 62 nil)
;;                                         (shell "~/proj/scitex-cloud/" 160 1 80 62 nil))
;;                                       nil))
;; 
;; (defalias 'cloud 'etm-open-cloud)
;; 
;; 
;; (provide 'etm-open-cloud)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-cloud.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-cloud.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-cloud.el ends here
