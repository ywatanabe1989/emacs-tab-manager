;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-papers.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-papers

;;; Code:

(require 'ert)
(require 'etm-open-papers)

;; Add your tests here
;; (ert-deftest test-etm-open-papers-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-papers.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-12-15 02:49:18>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-papers.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-papers ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "papers"
;;                                       '((file "~/proj/papers/" 0 1 80 62 "localhost")
;;                                         (shell "~/proj/papers/" 80 1 80 62 "localhost")
;;                                         (shell "~/proj/papers/" 160 1 80 62 "localhost"))
;;                                       "localhost"))
;; 
;; (defalias 'papers 'etm-open-papers)
;; 
;; 
;; (provide 'etm-open-papers)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-papers.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-papers.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-papers.el ends here
