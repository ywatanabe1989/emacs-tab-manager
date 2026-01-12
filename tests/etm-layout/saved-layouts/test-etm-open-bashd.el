;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-bashd.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-bashd

;;; Code:

(require 'ert)
(require 'etm-open-bashd)

;; Add your tests here
;; (ert-deftest test-etm-open-bashd-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-bashd.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-05-24 16:35:50>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-bashd.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; (defun etm-open-bashd ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "bashd"
;;                                       '((file "~/.bash.d/" 0 1 80 62 nil)
;;                                         (shell "~/.bash.d/" 80 1 80 62 nil)
;;                                         (shell "~/.bash.d/" 160 1 80 62 nil))
;;                                       nil))
;; 
;; (defalias 'bashd 'etm-open-bashd)
;; 
;; 
;; (provide 'etm-open-bashd)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-bashd.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-bashd.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-bashd.el ends here
