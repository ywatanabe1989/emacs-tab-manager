;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-task-manager.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-task-manager

;;; Code:

(require 'ert)
(require 'etm-open-task-manager)

;; Add your tests here
;; (ert-deftest test-etm-open-task-manager-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-task-manager.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-09-03 05:27:00>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-task-manager.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; (defun etm-open-task-manager ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "task-manager"
;;   '((file "~/proj/task-manager/" 0 1 120 62 nil)
;;    (shell "~/proj/task-manager/" 120 1 120 62 nil))
;;   nil))
;; 
;; (defalias 'task-manager 'etm-open-task-manager)
;; 
;; 
;; (provide 'etm-open-task-manager)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-task-manager.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-task-manager.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-task-manager.el ends here
