;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-github_cleanup.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-github_cleanup

;;; Code:

(require 'ert)
(require 'etm-open-github_cleanup)

;; Add your tests here
;; (ert-deftest test-etm-open-github_cleanup-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-github_cleanup.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-06-21 12:53:10>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-github_cleanup.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-github_cleanup ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "github_cleanup"
;;                                       '((file "~/proj/github_cleanup/" 0 1 80 62 "localhost")
;;                                         (shell "~/proj/github_cleanup/" 80 1 80 62 "localhost")
;;                                         (shell "~/proj/github_cleanup/" 160 1 80 62 "localhost"))
;;                                       "localhost"))
;; 
;; (defalias 'github_cleanup 'etm-open-github_cleanup)
;; 
;; 
;; (provide 'etm-open-github_cleanup)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-github_cleanup.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-github_cleanup.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-github_cleanup.el ends here
