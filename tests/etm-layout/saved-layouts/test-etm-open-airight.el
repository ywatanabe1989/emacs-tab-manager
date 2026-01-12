;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-airight.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-airight

;;; Code:

(require 'ert)
(require 'etm-open-airight)

;; Add your tests here
;; (ert-deftest test-etm-open-airight-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-airight.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-05-21 03:36:07>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-airight.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; (defun etm-open-airight ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "airight"
;;                                       '((file "~/proj/airight/" 0 1 120 62 nil)
;;                                         (shell "~/proj/airight/" 120 1 120 62 nil))
;;                                       "localhost"))
;; 
;; (defalias 'airight 'etm-open-airight)
;; 
;; 
;; (provide 'etm-open-airight)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-airight.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-airight.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-airight.el ends here
