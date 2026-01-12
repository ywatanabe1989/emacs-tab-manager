;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-manuscript.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-manuscript

;;; Code:

(require 'ert)
(require 'etm-open-manuscript)

;; Add your tests here
;; (ert-deftest test-etm-open-manuscript-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-manuscript.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-09-28 17:22:32>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-manuscript.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-manuscript ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "manuscript"
;;                                       '((file "~/proj/neurovista/paper/01_manuscript/contents/" 0 1 137 75
;;                                               "sp")
;;                                         (file "~/proj/neurovista/paper/01_manuscript/contents/" 137 1 137
;;                                               75 "sp"))
;;                                       "sp"))
;; 
;; (defalias 'manuscript 'etm-open-manuscript)
;; 
;; 
;; (provide 'etm-open-manuscript)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-manuscript.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-manuscript.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-manuscript.el ends here
