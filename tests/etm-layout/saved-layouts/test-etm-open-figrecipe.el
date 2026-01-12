;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-figrecipe.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-figrecipe

;;; Code:

(require 'ert)
(require 'etm-open-figrecipe)

;; Add your tests here
;; (ert-deftest test-etm-open-figrecipe-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-figrecipe.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-12-22 03:04:19>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-figrecipe.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-figrecipe ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "figrecipe"
;;                                       '((file "/home/ywatanabe/proj/figrecipe/README.md" 0 1 91 70
;;                                               "localhost")
;;                                         (shell "/home/ywatanabe/proj/figrecipe/" 91 1 91 70 "localhost")
;;                                         (shell "/home/ywatanabe/proj/figrecipe/" 182 1 92 70 "localhost"))
;;                                       "localhost"))
;; 
;; (defalias 'figrecipe 'etm-open-figrecipe)
;; 
;; 
;; (provide 'etm-open-figrecipe)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-figrecipe.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-figrecipe.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-figrecipe.el ends here
