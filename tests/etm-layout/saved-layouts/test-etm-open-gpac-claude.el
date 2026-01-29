;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-gpac-claude.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-gpac-claude

;;; Code:

(require 'ert)
(require 'etm-open-gpac-claude)

;; Add your tests here
;; (ert-deftest test-etm-open-gpac-claude-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-gpac-claude.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-05-30 09:16:51>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-gpac-claude.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-gpac-claude ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "gpac-claude"
;;                                       '((file "~/proj/.claude-worktree/gPAC/" 0 1 80 62 "ywatanabe@sp")
;;                                         (shell "~/proj/.claude-worktree/gPAC/" 80 1 80 62 "ywatanabe@sp")
;;                                         (shell "~/proj/.claude-worktree/gPAC/" 160 1 80 62 "ywatanabe@sp"))
;;                                       "sp"))
;; 
;; (defalias 'gpac-claude 'etm-open-gpac-claude)
;; 
;; 
;; (provide 'etm-open-gpac-claude)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-gpac-claude.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-gpac-claude.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-gpac-claude.el ends here
