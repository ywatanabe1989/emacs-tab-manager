;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-claude-code.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-claude-code

;;; Code:

(require 'ert)
(require 'etm-open-claude-code)

;; Add your tests here
;; (ert-deftest test-etm-open-claude-code-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-claude-code.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-05-22 23:52:04>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-claude-code.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; (defun etm-open-claude-code ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (progn
;;     ;; No SSH connection to register
;;     (--etm-layout-create-from-positions "claude-code"
;;                                         '((file "~/.emacs.d/lisp/emacs-claude-code/" 0 1 80 62 nil)
;;                                           (shell "~/.emacs.d/lisp/emacs-claude-code/" 80 1 80 62 nil)
;;                                           (shell "~/.emacs.d/lisp/emacs-claude-code/" 160 1 80 62 nil))
;;                                         "localhost")))
;; 
;; (defalias 'claude-code 'etm-open-claude-code)
;; 
;; 
;; (provide 'etm-open-claude-code)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-claude-code.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-claude-code.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-claude-code.el ends here
