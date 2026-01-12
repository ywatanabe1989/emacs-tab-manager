;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-ecc.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-ecc

;;; Code:

(require 'ert)
(require 'etm-open-ecc)

;; Add your tests here
;; (ert-deftest test-etm-open-ecc-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-ecc.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-12-24 03:34:30>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-ecc.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-ecc ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "ecc"
;;                                       '((file "~/.emacs.d/lisp/emacs-claude-code/" 0 1 91 70 "localhost")
;;                                         (shell "~/.emacs.d/lisp/emacs-claude-code/" 91 1 91 70 "localhost")
;;                                         (shell "~/.emacs.d/lisp/emacs-claude-code/" 182 1 92 70 "localhost"))
;;                                       "localhost"))
;; 
;; (defalias 'ecc 'etm-open-ecc)
;; 
;; 
;; (provide 'etm-open-ecc)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-ecc.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-ecc.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-ecc.el ends here
