;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-semantic-graph.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-semantic-graph

;;; Code:

(require 'ert)
(require 'etm-open-semantic-graph)

;; Add your tests here
;; (ert-deftest test-etm-open-semantic-graph-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-semantic-graph.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-08-31 22:19:31>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-semantic-graph.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-semantic-graph ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "semantic-graph"
;;                                       '((file "~/proj/semantic-graph/" 0 1 120 62 "localhost")
;;                                         (shell "~/proj/semantic-graph/" 120 1 120 62 "localhost"))
;;                                       "localhost"))
;; 
;; (defalias 'semantic-graph 'etm-open-semantic-graph)
;; 
;; 
;; (provide 'etm-open-semantic-graph)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-semantic-graph.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-semantic-graph.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-semantic-graph.el ends here
