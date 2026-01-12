;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-inits.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-inits

;;; Code:

(require 'ert)
(require 'etm-open-inits)

;; Add your tests here
;; (ert-deftest test-etm-open-inits-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-inits.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-12-22 02:07:34>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-inits.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-inits ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "inits"
;;                                       '((file "~/.emacs.d/inits/" 0 1 91 70 "localhost")
;;                                         (shell "~/.emacs.d/inits/" 91 1 91 70 "localhost")
;;                                         (shell "~/.emacs.d/inits/" 182 1 92 70 "localhost"))
;;                                       "localhost"))
;; 
;; (defalias 'inits 'etm-open-inits)
;; 
;; 
;; (provide 'etm-open-inits)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-inits.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-inits.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-inits.el ends here
