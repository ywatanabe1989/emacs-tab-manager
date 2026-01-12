;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-llemacs-prompt-04-formats.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-llemacs-prompt-04-formats

;;; Code:

(require 'ert)
(require 'etm-open-llemacs-prompt-04-formats)

;; Add your tests here
;; (ert-deftest test-etm-open-llemacs-prompt-04-formats-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/.llemacs-prompt/etm-open-llemacs-prompt-04-formats.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-02-13 00:22:13>
;; ;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-layout/saved-layouts/etm-open-llemacs-prompt-04-formats.el
;; 
;; (defun etm-open-llemacs-prompt-formats
;;     ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create "formats" 2 1
;;                        '((file . "~/proj/llemacs/workspace/resources/prompts/components/formats/")
;;                          (file . "~/proj/llemacs/workspace/resources/prompts/components/formats/")
;;                          (file . "/home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/formats/not-specified.md"))
;;                        "localhost"))
;; 
;; (when
;;     (not load-file-name)
;;   (message "%s loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; 
;; (provide 'etm-open-llemacs-prompt-04-formats)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-llemacs-prompt-04-formats.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/.llemacs-prompt/etm-open-llemacs-prompt-04-formats.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-llemacs-prompt-04-formats.el ends here
