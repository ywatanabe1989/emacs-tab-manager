;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-proj-claude.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-proj-claude

;;; Code:

(require 'ert)
(require 'etm-open-proj-claude)

;; Add your tests here
;; (ert-deftest test-etm-open-proj-claude-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-proj-claude.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-05-30 09:16:03>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-proj-claude.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-proj-claude ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "proj-claude"
;;                                       '((file "~/proj/.claude-worktree/" 0 1 80 62 "ywatanabe@sp")
;;                                         (shell "~/proj/.claude-worktree/" 80 1 80 62 "ywatanabe@sp")
;;                                         (shell "~/proj/.claude-worktree/" 160 1 80 62 "ywatanabe@sp"))
;;                                       "sp"))
;; 
;; (defalias 'proj-claude 'etm-open-proj-claude)
;; 
;; 
;; (provide 'etm-open-proj-claude)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-proj-claude.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-proj-claude.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-proj-claude.el ends here
