;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-lisp.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-lisp

;;; Code:

(require 'ert)
(require 'etm-open-lisp)

;; Add your tests here
;; (ert-deftest test-etm-open-lisp-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-lisp.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-12-24 05:22:32>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-lisp.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-lisp ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "lisp"
;;                                       '((file "~/.emacs.d/lisp/" 0 1 91 70 "localhost")
;;                                         (shell "~/.emacs.d/lisp/" 91 1 91 70 "localhost")
;;                                         (shell "~/.emacs.d/lisp/" 182 1 92 70 "localhost"))
;;                                       "localhost"))
;; 
;; (defalias 'lisp 'etm-open-lisp)
;; 
;; 
;; (provide 'etm-open-lisp)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-lisp.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-lisp.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-lisp.el ends here
