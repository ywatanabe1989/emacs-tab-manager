;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-tables.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-tables

;;; Code:

(require 'ert)
(require 'etm-open-tables)

;; Add your tests here
;; (ert-deftest test-etm-open-tables-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-tables.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-09-28 17:47:24>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-tables.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-tables ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "tables"
;;                                       '((file
;;                                          "~/proj/neurovista/paper/01_manuscript/contents/tables/caption_and_media/"
;;                                          0 1 240 31 "sp")
;;                                         (file "~/proj/neurovista/data/" 0 32 240 31 "sp"))
;;                                       "sp"))
;; 
;; (defalias 'tables 'etm-open-tables)
;; 
;; 
;; (provide 'etm-open-tables)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-tables.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-tables.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-tables.el ends here
