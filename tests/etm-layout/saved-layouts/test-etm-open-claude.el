;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-claude.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-claude

;;; Code:

(require 'ert)
(require 'etm-open-claude)

;; Add your tests here
;; (ert-deftest test-etm-open-claude-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-claude.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-12-24 03:34:43>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-claude.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-claude ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "claude"
;;                                       '((file "~/.claude/" 0 1 91 70 "localhost")
;;                                         (shell "~/.claude/" 91 1 91 70 "localhost")
;;                                         (shell "~/.claude/" 182 1 92 70 "localhost"))
;;                                       "localhost"))
;; 
;; (defalias 'claude 'etm-open-claude)
;; 
;; 
;; (provide 'etm-open-claude)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-claude.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-claude.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-claude.el ends here
