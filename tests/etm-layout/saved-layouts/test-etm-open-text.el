;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-text.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-text

;;; Code:

(require 'ert)
(require 'etm-open-text)

;; Add your tests here
;; (ert-deftest test-etm-open-text-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-text.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-09-28 16:05:11>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-text.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-text ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "text"
;;                                       '((file "~/proj/neurovista/paper/01_manuscript/contents/" 0 1 137 75
;;                                               "sp")
;;                                         (file "~/proj/neurovista/paper/01_manuscript/contents/" 137 1 137
;;                                               75 "sp"))
;;                                       "sp"))
;; 
;; (defalias 'text 'etm-open-text)
;; 
;; 
;; (provide 'etm-open-text)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-text.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-text.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-text.el ends here
