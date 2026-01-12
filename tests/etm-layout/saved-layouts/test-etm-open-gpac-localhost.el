;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-gpac-localhost.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-gpac-localhost

;;; Code:

(require 'ert)
(require 'etm-open-gpac-localhost)

;; Add your tests here
;; (ert-deftest test-etm-open-gpac-localhost-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-gpac-localhost.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-07-19 10:47:17>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-gpac-localhost.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-gpac-localhost ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "gpac-localhost"
;;                                       '((file "~/proj/gPAC/" 0 1 80 61 "localhost")
;;                                         (shell "~/proj/gPAC/" 80 1 80 61 "localhost")
;;                                         (shell "~/proj/gPAC/" 160 1 80 61 "localhost"))
;;                                       "localhost"))
;; 
;; (defalias 'gpac-localhost 'etm-open-gpac-localhost)
;; 
;; 
;; (provide 'etm-open-gpac-localhost)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-gpac-localhost.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-gpac-localhost.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-gpac-localhost.el ends here
