;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-paper.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-paper

;;; Code:

(require 'ert)
(require 'etm-open-paper)

;; Add your tests here
;; (ert-deftest test-etm-open-paper-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-paper.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-09-26 10:40:10>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-paper.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-paper ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "paper"
;;                                       '((file "~/proj/neurovista/paper/" 0 1 80 62 "sp")
;;                                         (shell "~/proj/neurovista/paper/" 80 1 80 62 "sp")
;;                                         (shell "~/proj/neurovista/paper/" 160 1 80 62 "sp"))
;;                                       "sp"))
;; 
;; (defalias 'paper 'etm-open-paper)
;; 
;; 
;; (provide 'etm-open-paper)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-paper.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-paper.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-paper.el ends here
