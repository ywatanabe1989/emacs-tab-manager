;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-example-research.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-example-research

;;; Code:

(require 'ert)
(require 'etm-open-example-research)

;; Add your tests here
;; (ert-deftest test-etm-open-example-research-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-example-research.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-11-18 15:21:26>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-example-research.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-example-research ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "example-research"
;;                                       '((file "~/proj/examples/scitex_template_research/" 0 1 120 62 nil)
;;                                         (shell "~/proj/examples/scitex_template_research/" 120 1 120 62 nil))
;;                                       nil))
;; 
;; (defalias 'example-research 'etm-open-example-research)
;; 
;; 
;; (provide 'etm-open-example-research)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-example-research.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-example-research.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-example-research.el ends here
