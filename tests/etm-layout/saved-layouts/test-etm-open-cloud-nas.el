;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-cloud-nas.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-cloud-nas

;;; Code:

(require 'ert)
(require 'etm-open-cloud-nas)

;; Add your tests here
;; (ert-deftest test-etm-open-cloud-nas-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-cloud-nas.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-11-28 12:25:29>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-cloud-nas.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-cloud-nas ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "cloud-nas"
;;                                       '((file "~/proj/scitex-cloud/" 0 1 80 62 "ywatanabe@nas")
;;                                         (shell "~/proj/scitex-cloud/" 80 1 80 62 "ywatanabe@nas")
;;                                         (shell "~/proj/scitex-cloud/" 160 1 80 62 "ywatanabe@nas"))
;;                                       "nas"))
;; 
;; (defalias 'cloud-nas 'etm-open-cloud-nas)
;; 
;; 
;; (provide 'etm-open-cloud-nas)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-cloud-nas.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-cloud-nas.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-cloud-nas.el ends here
