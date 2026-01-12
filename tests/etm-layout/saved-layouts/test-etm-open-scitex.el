;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-scitex.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-scitex

;;; Code:

(require 'ert)
(require 'etm-open-scitex)

;; Add your tests here
;; (ert-deftest test-etm-open-scitex-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-scitex.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-10-23 22:35:51>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-scitex.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-scitex ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "scitex"
;;                                       '((file "~/proj/scitex-code/" 0 1 80 62 nil)
;;                                         (shell "~/proj/scitex-code/" 80 1 80 62 nil)
;;                                         (shell "~/proj/scitex-code/" 160 1 80 62 nil))
;;                                       nil))
;; 
;; (defalias 'scitex 'etm-open-scitex)
;; 
;; 
;; (provide 'etm-open-scitex)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-scitex.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-scitex.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-scitex.el ends here
