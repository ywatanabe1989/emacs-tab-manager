;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-ref.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-ref

;;; Code:

(require 'ert)
(require 'etm-open-ref)

;; Add your tests here
;; (ert-deftest test-etm-open-ref-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-ref.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-09-29 09:51:11>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-ref.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-ref ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "ref"
;;                                       '((file "~/proj/neurovista/paper/01_manuscript/contents/" 0 1 120 62
;;                                               "sp")
;;                                         (file "~/proj/neurovista/paper/shared/bib_files/" 120 1 120 62 "sp"))
;;                                       "sp"))
;; 
;; (defalias 'ref 'etm-open-ref)
;; 
;; 
;; (provide 'etm-open-ref)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-ref.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-ref.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-ref.el ends here
