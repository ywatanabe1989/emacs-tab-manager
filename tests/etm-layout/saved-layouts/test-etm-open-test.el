;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-test.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-test

;;; Code:

(require 'ert)
(require 'etm-open-test)

;; Add your tests here
;; (ert-deftest test-etm-open-test-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-test.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-05-25 08:59:40>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-test.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; (defun etm-open-test ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "test"
;;                                       '((file "~/proj/mngs_repo/" 0 1 80 62 "ywatanabe@sp")
;;                                         (shell "~/proj/mngs_repo/" 80 1 80 62 "ywatanabe@sp")
;;                                         (shell "~/proj/mngs_repo/" 160 1 80 62 "ywatanabe@sp"))
;;                                       nil))
;; 
;; (defalias 'test 'etm-open-test)
;; 
;; 
;; (provide 'etm-open-test)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-test.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-test.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-test.el ends here
