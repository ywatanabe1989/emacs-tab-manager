;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-whisper-live.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-whisper-live

;;; Code:

(require 'ert)
(require 'etm-open-whisper-live)

;; Add your tests here
;; (ert-deftest test-etm-open-whisper-live-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-whisper-live.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-11-24 00:07:06>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-whisper-live.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun etm-open-whisper-live ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create-from-positions "whisper-live"
;;                                       '((file "~/.emacs.d/lisp/whisper-live/" 0 1 80 62 "localhost")
;;                                         (shell "~/.emacs.d/lisp/whisper-live/" 80 1 80 62 "localhost")
;;                                         (shell "~/.emacs.d/lisp/whisper-live/" 160 1 80 62 "localhost"))
;;                                       "localhost"))
;; 
;; (defalias 'whisper-live 'etm-open-whisper-live)
;; 
;; 
;; (provide 'etm-open-whisper-live)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-whisper-live.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-whisper-live.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-whisper-live.el ends here
