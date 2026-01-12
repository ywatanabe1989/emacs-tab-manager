;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-code.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-code

;;; Code:

(require 'ert)
(require 'etm-open-code)

;; Add your tests here
;; (ert-deftest test-etm-open-code-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-code.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-10-29 17:10:25>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-code.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-code ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "code"
;;                                       '((file "~/proj/scitex-code/" 0 1 80 62 nil)
;;                                         (shell "~/proj/scitex-code/" 80 1 80 62 nil)
;;                                         (shell "~/proj/scitex-code/" 160 1 80 62 nil))
;;                                       nil))
;; 
;; (defalias 'code 'etm-open-code)
;; 
;; 
;; (provide 'etm-open-code)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-code.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-code.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-code.el ends here
