;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-gpac-paper-with-code.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-gpac-paper-with-code

;;; Code:

(require 'ert)
(require 'etm-open-gpac-paper-with-code)

;; Add your tests here
;; (ert-deftest test-etm-open-gpac-paper-with-code-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-gpac-paper-with-code.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-05-20 00:56:00>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-gpac-paper-with-code.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-gpac-paper-with-code ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "gpac-paper-with-code"
;;                                       '((file
;;                                          "~/proj/gPAC-paper-with-code/"
;;                                          0 1 80 62 "sp")
;;                                         (shell
;;                                          "~/proj/gPAC-paper-with-code/"
;;                                          80 1 80 62 "sp")
;;                                         (shell
;;                                          "~/proj/gPAC-paper-with-code/"
;;                                          160 1 80 62 "sp"))
;;                                       "sp"))
;; 
;; (defalias 'gpac-paper-with-code 'etm-open-gpac-paper-with-code)
;; 
;; 
;; (provide 'etm-open-gpac-paper-with-code)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-gpac-paper-with-code.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-gpac-paper-with-code.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-gpac-paper-with-code.el ends here
