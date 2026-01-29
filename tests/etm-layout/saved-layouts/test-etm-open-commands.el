;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-commands.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-commands

;;; Code:

(require 'ert)
(require 'etm-open-commands)

;; Add your tests here
;; (ert-deftest test-etm-open-commands-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-commands.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-05-22 13:53:52>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-commands.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; (defun etm-open-commands ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (progn
;;     ;; No SSH connection to register
;;     (--etm-layout-create-from-positions "commands"
;;                                         '((file "~/.claude/commands/" 0 1 120 62 nil)
;;                                           (shell "~/.claude/commands/" 120 1 120 62 nil))
;;                                         "localhost")))
;; 
;; (defalias 'commands 'etm-open-commands)
;; 
;; 
;; (provide 'etm-open-commands)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-commands.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-commands.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-commands.el ends here
