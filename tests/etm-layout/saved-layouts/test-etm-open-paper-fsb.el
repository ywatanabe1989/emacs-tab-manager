;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-paper-fsb.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-paper-fsb

;;; Code:

(require 'ert)
(require 'etm-open-paper-fsb)

;; Add your tests here
;; (ert-deftest test-etm-open-paper-fsb-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-paper-fsb.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-12-19 21:39:32>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-paper-fsb.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-paper-fsb ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "paper-fsb"
;;                                       '((file "~/proj/papers/paper-fsb/" 0 1 91 70 "localhost")
;;                                         (shell "~/proj/papers/paper-fsb/" 91 1 91 70 "localhost")
;;                                         (shell "~/proj/papers/paper-fsb/" 182 1 92 70 "localhost"))
;;                                       "localhost"))
;; 
;; (defalias 'paper-fsb 'etm-open-paper-fsb)
;; 
;; 
;; (provide 'etm-open-paper-fsb)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-paper-fsb.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-paper-fsb.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-paper-fsb.el ends here
