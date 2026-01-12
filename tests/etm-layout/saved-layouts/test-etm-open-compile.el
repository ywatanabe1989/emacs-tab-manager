;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-compile.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-compile

;;; Code:

(require 'ert)
(require 'etm-open-compile)

;; Add your tests here
;; (ert-deftest test-etm-open-compile-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-compile.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-09-28 16:04:34>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-compile.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-compile ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "compile"
;;                                       '((file "~/proj/neurovista/paper/" 0 1 91 75 "sp")
;;                                         (shell "~/proj/neurovista/paper/" 91 1 91 75 "sp")
;;                                         (shell "~/proj/neurovista/paper/" 182 1 92 75 "sp"))
;;                                       "sp"))
;; 
;; (defalias 'compile 'etm-open-compile)
;; 
;; 
;; (provide 'etm-open-compile)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-compile.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-compile.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-compile.el ends here
