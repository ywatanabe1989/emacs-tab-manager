;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-fig.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-fig

;;; Code:

(require 'ert)
(require 'etm-open-fig)

;; Add your tests here
;; (ert-deftest test-etm-open-fig-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-fig.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-09-28 17:27:06>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-fig.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-fig ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "fig"
;;                                       '((file
;;                                          "~/proj/neurovista/paper/01_manuscript/contents/figures/caption_and_media/"
;;                                          0 1 240 31 "sp")
;;                                         (file "~/proj/neurovista/data/" 0 32 240 31 "sp"))
;;                                       "sp"))
;; 
;; (defalias 'fig 'etm-open-fig)
;; 
;; 
;; (provide 'etm-open-fig)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-fig.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-fig.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-fig.el ends here
