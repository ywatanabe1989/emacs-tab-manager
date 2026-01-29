;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-nv.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-nv

;;; Code:

(require 'ert)
(require 'etm-open-nv)

;; Add your tests here
;; (ert-deftest test-etm-open-nv-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-nv.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-05-27 09:26:02>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-nv.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; (defun etm-open-nv ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "nv"
;;                                       '((file "~/proj/neurovista/" 0 1 79 60 "ywatanabe@sp")
;;                                         (shell "~/proj/neurovista/" 79 1 79 60 "ywatanabe@sp")
;;                                         (shell "~/proj/neurovista/" 158 1 80 60 "ywatanabe@sp"))
;;                                       "sp"))
;; 
;; (defalias 'nv 'etm-open-nv)
;; 
;; 
;; (provide 'etm-open-nv)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-nv.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-nv.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-nv.el ends here
