;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-fsb.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-fsb

;;; Code:

(require 'ert)
(require 'etm-open-fsb)

;; Add your tests here
;; (ert-deftest test-etm-open-fsb-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-fsb.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-12-20 05:55:38>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-fsb.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-fsb ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "fsb"
;;                                       '((file "~/proj/fsb/" 0 1 91 70 "localhost")
;;                                         (shell "~/proj/fsb/" 91 1 91 70 "localhost")
;;                                         (shell "~/proj/fsb/" 182 1 92 70 "localhost"))
;;                                       "localhost"))
;; 
;; (defalias 'fsb 'etm-open-fsb)
;; 
;; 
;; (provide 'etm-open-fsb)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-fsb.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-fsb.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-fsb.el ends here
