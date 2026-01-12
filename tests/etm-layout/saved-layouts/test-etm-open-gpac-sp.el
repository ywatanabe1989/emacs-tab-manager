;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-gpac-sp.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-gpac-sp

;;; Code:

(require 'ert)
(require 'etm-open-gpac-sp)

;; Add your tests here
;; (ert-deftest test-etm-open-gpac-sp-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-gpac-sp.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-07-19 09:30:15>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-gpac-sp.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-gpac-sp ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "gpac-sp"
;;                                       '((file "~/proj/gPAC/" 0 1 80 62 "sp")
;;                                         (shell "~/proj/gPAC/" 80 1 80 62 "sp")
;;                                         (shell "~/proj/gPAC/" 160 1 80 62 "sp"))
;;                                       "sp"))
;; 
;; (defalias 'gpac-sp 'etm-open-gpac-sp)
;; 
;; 
;; (provide 'etm-open-gpac-sp)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-gpac-sp.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-gpac-sp.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-gpac-sp.el ends here
