;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-a2a.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-a2a

;;; Code:

(require 'ert)
(require 'etm-open-a2a)

;; Add your tests here
;; (ert-deftest test-etm-open-a2a-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-a2a.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-05-31 09:26:19>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-a2a.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-a2a ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "a2a"
;;                                       '((file "~/.emacs.d/lisp/agent-to-agent-mcp-server/" 0 1 120 62
;;                                               "localhost")
;;                                         (shell "~/.emacs.d/lisp/agent-to-agent-mcp-server/" 120 1 120 62
;;                                                "localhost"))
;;                                       "localhost"))
;; 
;; (defalias 'a2a 'etm-open-a2a)
;; 
;; 
;; (provide 'etm-open-a2a)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-a2a.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-a2a.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-a2a.el ends here
