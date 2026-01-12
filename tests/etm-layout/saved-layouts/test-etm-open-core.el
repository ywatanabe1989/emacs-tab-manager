;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-core.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-core

;;; Code:

(require 'ert)
(require 'etm-open-core)

;; Add your tests here
;; (ert-deftest test-etm-open-core-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-core.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-11-10 22:28:37>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-core.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-core ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "core"
;;                                       '((file "~/proj/scitex-core/" 0 1 80 62 nil)
;;                                         (shell "~/proj/scitex-core/" 80 1 80 62 nil)
;;                                         (shell "~/proj/scitex-core/" 160 1 80 62 nil))
;;                                       nil))
;; 
;; (defalias 'core 'etm-open-core)
;; 
;; 
;; (provide 'etm-open-core)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-core.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-core.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-core.el ends here
