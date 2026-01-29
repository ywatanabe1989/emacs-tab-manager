;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-gpac.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-gpac

;;; Code:

(require 'ert)
(require 'etm-open-gpac)

;; Add your tests here
;; (ert-deftest test-etm-open-gpac-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-gpac.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-05-30 09:17:00>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-gpac.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-gpac ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "gpac"
;;                                       '((file "~/proj/gPAC/" 0 1 80 62 "ywatanabe@sp")
;;                                         (shell "~/proj/gPAC/" 80 1 80 62 "ywatanabe@sp")
;;                                         (shell "~/proj/gPAC/" 160 1 80 62 "ywatanabe@sp"))
;;                                       "sp"))
;; 
;; (defalias 'gpac 'etm-open-gpac)
;; 
;; 
;; (provide 'etm-open-gpac)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-gpac.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-gpac.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-gpac.el ends here
