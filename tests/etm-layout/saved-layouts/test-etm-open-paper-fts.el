;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-paper-fts.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-paper-fts

;;; Code:

(require 'ert)
(require 'etm-open-paper-fts)

;; Add your tests here
;; (ert-deftest test-etm-open-paper-fts-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-paper-fts.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-12-20 10:16:51>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-paper-fts.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-paper-fts ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "paper-fts"
;;                                       '((file "~/proj/papers/paper-fts/" 0 1 91 70 "localhost")
;;                                         (shell "~/proj/papers/paper-fts/" 91 1 91 70 "localhost")
;;                                         (shell "~/proj/papers/paper-fts/" 182 1 92 70 "localhost"))
;;                                       "localhost"))
;; 
;; (defalias 'paper-fts 'etm-open-paper-fts)
;; 
;; 
;; (provide 'etm-open-paper-fts)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-paper-fts.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-paper-fts.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-paper-fts.el ends here
