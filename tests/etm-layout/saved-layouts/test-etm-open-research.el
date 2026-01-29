;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-research.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-research

;;; Code:

(require 'ert)
(require 'etm-open-research)

;; Add your tests here
;; (ert-deftest test-etm-open-research-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-research.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-12-09 03:55:23>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-research.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-research ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "research"
;;                                       '((file "~/proj/scitex_template_research/" 0 1 80 62 "localhost")
;;                                         (shell "~/proj/scitex_template_research/" 80 1 80 62 "localhost")
;;                                         (shell "~/proj/scitex_template_research/" 160 1 80 62 "localhost"))
;;                                       "localhost"))
;; 
;; (defalias 'research 'etm-open-research)
;; 
;; 
;; (provide 'etm-open-research)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-research.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-research.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-research.el ends here
