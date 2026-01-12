;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-sakana3.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-sakana3

;;; Code:

(require 'ert)
(require 'etm-open-sakana3)

;; Add your tests here
;; (ert-deftest test-etm-open-sakana3-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-sakana3.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-08-30 22:14:51>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-sakana3.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-sakana3 ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "sakana3"
;;                                       '((file
;;                                          "/home/ywatanabe/proj/sakana3-continuous-thought-machine/README.md"
;;                                          0 1 80 62 "localhost")
;;                                         (shell "/home/ywatanabe/proj/sakana3-continuous-thought-machine/"
;;                                                80 1 80 62 "localhost")
;;                                         (shell "/home/ywatanabe/proj/sakana3-continuous-thought-machine/"
;;                                                160 1 80 62 "localhost"))
;;                                       "localhost"))
;; 
;; (defalias 'sakana3 'etm-open-sakana3)
;; 
;; 
;; (provide 'etm-open-sakana3)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-sakana3.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-sakana3.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-sakana3.el ends here
