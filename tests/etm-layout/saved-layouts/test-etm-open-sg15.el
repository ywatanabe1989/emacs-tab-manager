;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-sg15.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-sg15

;;; Code:

(require 'ert)
(require 'etm-open-sg15)

;; Add your tests here
;; (ert-deftest test-etm-open-sg15-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-sg15.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-09-01 04:30:12>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-sg15.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-sg15 ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "sg15"
;;                                       '((file
;;                                          "/home/ywatanabe/proj/semantic-graph/examples/15_guidelines_clustering.py"
;;                                          0 1 120 62 "localhost")
;;                                         (shell "/home/ywatanabe/proj/semantic-graph/examples/" 120 1 120 62
;;                                                "localhost"))
;;                                       "localhost"))
;; 
;; (defalias 'sg15 'etm-open-sg15)
;; 
;; 
;; (provide 'etm-open-sg15)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-sg15.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-sg15.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-sg15.el ends here
