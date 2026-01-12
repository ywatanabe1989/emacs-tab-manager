;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-scitex-code-local.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-scitex-code-local

;;; Code:

(require 'ert)
(require 'etm-open-scitex-code-local)

;; Add your tests here
;; (ert-deftest test-etm-open-scitex-code-local-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-scitex-code-local.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-12-20 05:15:39>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-scitex-code-local.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-scitex-code-local ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "scitex-code-local"
;;                                       '((file "~/proj/scitex-code/" 0 1 91 70 "localhost")
;;                                         (shell "~/proj/scitex-code/" 91 1 91 70 "localhost")
;;                                         (shell "~/proj/scitex-code/" 182 1 92 70 "localhost"))
;;                                       "localhost"))
;; 
;; (defalias 'scitex-code-local 'etm-open-scitex-code-local)
;; 
;; 
;; (provide 'etm-open-scitex-code-local)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-scitex-code-local.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-scitex-code-local.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-scitex-code-local.el ends here
