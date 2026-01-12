;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-dmd.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-dmd

;;; Code:

(require 'ert)
(require 'etm-open-dmd)

;; Add your tests here
;; (ert-deftest test-etm-open-dmd-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-dmd.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-05-25 10:21:48>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-dmd.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; (defun etm-open-dmd ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "dmd"
;;                                       '((file "~/proj/gDMD/" 0 1 80 62 "sp")
;;                                         (shell "~/proj/gDMD/" 80 1 80 62 "sp")
;;                                         (file "/home/ywatanabe/proj/mngs_repo/src/mngs/dsp/_pac.py" 160 1
;;                                               80 31 "ywatanabe@sp")
;;                                         (file "/home/ywatanabe/proj/gPAC/src/gpac/_pac.py" 160 32 80 31
;;                                               "ywatanabe@sp"))
;;                                       "sp"))
;; 
;; (defalias 'dmd 'etm-open-dmd)
;; 
;; 
;; (provide 'etm-open-dmd)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-dmd.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-dmd.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-dmd.el ends here
