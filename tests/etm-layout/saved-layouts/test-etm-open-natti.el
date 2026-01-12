;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-natti.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-natti

;;; Code:

(require 'ert)
(require 'etm-open-natti)

;; Add your tests here
;; (ert-deftest test-etm-open-natti-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-natti.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-10-14 08:20:40>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-natti.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-natti ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "natti"
;;                                       '((file "~/proj/natti_ccl/" 0 1 120 62 "localhost")
;;                                         (shell "~/proj/natti_ccl/" 120 1 120 62 "localhost"))
;;                                       "localhost"))
;; 
;; (defalias 'natti 'etm-open-natti)
;; 
;; 
;; (provide 'etm-open-natti)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-natti.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-natti.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-natti.el ends here
