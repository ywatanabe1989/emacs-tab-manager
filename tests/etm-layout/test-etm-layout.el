;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-layout.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-layout

;;; Code:

(require 'ert)
(require 'etm-layout)

;; Add your tests here
;; (ert-deftest test-etm-layout-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/etm-layout.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-05-20 00:56:11>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/etm-layout/etm-layout.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (require 'etm-layout-create)
;; (require 'etm-layout-save)
;; (require 'etm-layout-window)
;; (require 'etm-layout-load)
;; (require 'etm-layout-open)
;; (require 'etm-layout-preview)
;; (require 'etm-layout-commands)
;; 
;; 
;; (provide 'etm-layout)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-layout.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/etm-layout.el
;; --------------------------------------------------------------------------------

;;; test-etm-layout.el ends here
