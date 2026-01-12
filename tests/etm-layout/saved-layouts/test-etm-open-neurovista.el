;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-neurovista.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-neurovista

;;; Code:

(require 'ert)
(require 'etm-open-neurovista)

;; Add your tests here
;; (ert-deftest test-etm-open-neurovista-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-neurovista.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-09-28 17:25:04>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-neurovista.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-neurovista ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "neurovista"
;;                                       '((file "~/proj/neurovista/" 0 1 80 62 "sp")
;;                                         (shell "~/proj/neurovista/" 80 1 80 62 "sp")
;;                                         (shell "~/proj/neurovista/" 160 1 80 62 "sp"))
;;                                       "sp"))
;; 
;; (defalias 'neurovista 'etm-open-neurovista)
;; 
;; 
;; (provide 'etm-open-neurovista)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-neurovista.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-neurovista.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-neurovista.el ends here
