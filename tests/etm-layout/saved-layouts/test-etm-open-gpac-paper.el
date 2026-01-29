;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-gpac-paper.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-gpac-paper

;;; Code:

(require 'ert)
(require 'etm-open-gpac-paper)

;; Add your tests here
;; (ert-deftest test-etm-open-gpac-paper-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-gpac-paper.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-05-24 17:38:38>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-gpac-paper.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; (defun etm-open-gpac-paper ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "gpac-paper"
;;                                       '((file "~/proj/gPAC-paper-with-code/" 0 1 80 62 "ywatanabe@sp")
;;                                         (shell "~/proj/gPAC-paper-with-code/" 80 1 80 62 "ywatanabe@sp")
;;                                         (shell "~/proj/gPAC-paper-with-code/" 160 1 80 62 "ywatanabe@sp"))
;;                                       "sp"))
;; 
;; (defalias 'gpac-paper 'etm-open-gpac-paper)
;; 
;; 
;; (provide 'etm-open-gpac-paper)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-gpac-paper.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-gpac-paper.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-gpac-paper.el ends here
