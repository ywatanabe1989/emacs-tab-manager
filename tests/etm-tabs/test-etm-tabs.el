;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-tabs.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-tabs

;;; Code:

(require 'ert)
(require 'etm-tabs)

;; Add your tests here
;; (ert-deftest test-etm-tabs-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-tabs/etm-tabs.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-05-10 08:45:05>
;; ;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-tabs/etm-tabs.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; ;;; Commentary:
;; ;; Main module file for ETM tabs functionality
;; ;; This aggregates all tabs-related modules
;; 
;; (require 'etm-tabs-new-and-rename)
;; 
;; (provide 'etm-tabs)
;; 
;; (when (not load-file-name)
;;   (message "etm-tabs.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-tabs/etm-tabs.el
;; --------------------------------------------------------------------------------

;;; test-etm-tabs.el ends here
