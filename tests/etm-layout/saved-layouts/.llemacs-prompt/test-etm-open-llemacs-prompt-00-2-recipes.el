;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-open-llemacs-prompt-00-2-recipes.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-open-llemacs-prompt-00-2-recipes

;;; Code:

(require 'ert)
(require 'etm-open-llemacs-prompt-00-2-recipes)

;; Add your tests here
;; (ert-deftest test-etm-open-llemacs-prompt-00-2-recipes-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/.llemacs-prompt/etm-open-llemacs-prompt-00-2-recipes.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-02-13 00:22:12>
;; ;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-layout/saved-layouts/etm-open-llemacs-prompt-00-2-recipes.el
;; 
;; (defun etm-open-llemacs-prompt-recipes
;;     ()
;;   "Create tab layout for specific configuration."
;;   (interactive)
;;   (--etm-layout-create "recipes" 1 1
;;                        '((file . "~/proj/llemacs/workspace/resources/prompts/recipes/")
;;                          (file . "~/proj/llemacs/workspace/resources/prompts/recipes/"))
;;                        "localhost"))
;; 
;; (when
;;     (not load-file-name)
;;   (message "%s loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; 
;; (provide 'etm-open-llemacs-prompt-00-2-recipes)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-open-llemacs-prompt-00-2-recipes.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/.llemacs-prompt/etm-open-llemacs-prompt-00-2-recipes.el
;; --------------------------------------------------------------------------------

;;; test-etm-open-llemacs-prompt-00-2-recipes.el ends here
