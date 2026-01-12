;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-etm.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-etm

;;; Code:

(require 'ert)
(require 'etm-open-etm)

;; Add your tests here
;; (ert-deftest test-etm-open-etm-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-etm.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-12-24 03:50:05>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/etm-layout/saved-layouts/etm-open-etm.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-etm ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "etm"
;;                                       '((file "~/.emacs.d/lisp/emacs-tab-manager/" 0 1 91 70 "localhost")
;;                                         (shell "~/.emacs.d/lisp/emacs-tab-manager/" 91 1 91 70 "localhost")
;;                                         (shell "~/.emacs.d/lisp/emacs-tab-manager/" 182 1 92 70 "localhost"))
;;                                       "localhost"))
;; 
;; (defalias 'etm 'etm-open-etm)
;; 
;; 
;; (provide 'etm-open-etm)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-etm.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-etm.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-etm.el ends here
