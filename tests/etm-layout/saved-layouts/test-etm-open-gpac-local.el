;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-gpac-local.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-gpac-local

;;; Code:

(require 'ert)
(require 'etm-open-gpac-local)

;; Add your tests here
;; (ert-deftest test-etm-open-gpac-local-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-gpac-local.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-07-19 10:48:52>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-gpac-local.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-gpac-local ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "gpac-local"
;;                                       '((file "~/proj/gPAC/" 0 1 80 62 "localhost")
;;                                         (shell "~/proj/gPAC/" 80 1 80 62 "localhost")
;;                                         (shell "~/proj/gPAC/" 160 1 80 62 "localhost"))
;;                                       "localhost"))
;; 
;; (defalias 'gpac-local 'etm-open-gpac-local)
;; 
;; 
;; (provide 'etm-open-gpac-local)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-gpac-local.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-gpac-local.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-gpac-local.el ends here
