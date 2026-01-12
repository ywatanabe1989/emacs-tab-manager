;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-zotero.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-zotero

;;; Code:

(require 'ert)
(require 'etm-open-zotero)

;; Add your tests here
;; (ert-deftest test-etm-open-zotero-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-zotero.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-10-09 09:53:34>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-zotero.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-zotero ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "zotero"
;;                                       '((file "~/proj/zotero-translators-python/" 0 1 80 43 "localhost")
;;                                         (shell "~/proj/zotero-translators-python/" 80 1 80 43 "localhost"))
;;                                       "localhost"))
;; 
;; (defalias 'zotero 'etm-open-zotero)
;; 
;; 
;; (provide 'etm-open-zotero)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-zotero.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-zotero.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-zotero.el ends here
