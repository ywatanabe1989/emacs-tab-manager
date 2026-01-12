;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-dot.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-dot

;;; Code:

(require 'ert)
(require 'etm-open-dot)

;; Add your tests here
;; (ert-deftest test-etm-open-dot-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-dot.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-05-24 13:04:54>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-dot.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; (defun etm-open-dot ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "dot"
;;                                       '((file "~/.dotfiles/" 0 1 80 62 nil)
;;                                         (shell "~/.dotfiles/" 80 1 80 62 nil)
;;                                         (shell "~/.dotfiles/" 160 1 80 62 nil))
;;                                       nil))
;; 
;; (defalias 'dot 'etm-open-dot)
;; 
;; 
;; (provide 'etm-open-dot)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-dot.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-dot.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-dot.el ends here
