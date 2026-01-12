;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-scitex-web.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-scitex-web

;;; Code:

(require 'ert)
(require 'etm-open-scitex-web)

;; Add your tests here
;; (ert-deftest test-etm-open-scitex-web-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-scitex-web.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-05-21 03:01:40>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-scitex-web.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; (defun etm-open-scitex-web ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "scitex-web"
;;                                       '((file "~/proj/scitex-web/" 0 1 120 62 nil)
;;                                         (shell "~/proj/scitex-web/" 120 1 120 62 nil))
;;                                       "localhost"))
;; 
;; (defalias 'scitex-web 'etm-open-scitex-web)
;; 
;; 
;; (provide 'etm-open-scitex-web)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-scitex-web.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-scitex-web.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-scitex-web.el ends here
