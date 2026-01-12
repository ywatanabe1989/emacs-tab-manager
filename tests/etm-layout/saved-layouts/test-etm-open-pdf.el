;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-pdf.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-pdf

;;; Code:

(require 'ert)
(require 'etm-open-pdf)

;; Add your tests here
;; (ert-deftest test-etm-open-pdf-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-pdf.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-09-28 17:26:02>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-pdf.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-pdf ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "pdf"
;;                                       '((file "~/proj/neurovista/paper/" 0 1 80 62 "sp")
;;                                         (shell "~/proj/neurovista/paper/" 80 1 80 62 "sp")
;;                                         (file
;;                                          "/home/ywatanabe/proj/neurovista/paper/01_manuscript/manuscript.pdf"
;;                                          160 1 80 62 "sp"))
;;                                       "sp"))
;; 
;; (defalias 'pdf 'etm-open-pdf)
;; 
;; 
;; (provide 'etm-open-pdf)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-pdf.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-pdf.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-pdf.el ends here
