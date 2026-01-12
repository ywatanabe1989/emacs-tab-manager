;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-core-startup.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-core-startup

;;; Code:

(require 'ert)
(require 'etm-core-startup)

;; Add your tests here
;; (ert-deftest test-etm-core-startup-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-core/etm-core-startup.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-05-19 06:57:12>
;; ;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-core/etm-core-startup.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (require 'etm-core-variables)
;; (require 'etm-close)
;; (require 'etm-buffer-navigation)
;; 
;; (defcustom etm-startup-layouts-list '("neurovista" "lisp" "genai")
;;   "List of layout names to automatically open at startup.
;; Each name should match an existing layout function `etm-open-LAYOUTNAME'."
;;   :type '(repeat string)
;;   :group 'etm)
;; 
;; (defun etm-startup-layouts ()
;;   "Open all layouts specified in `etm-startup-layouts-list'.
;; Closes the default tab after opening configured layouts."
;;   (interactive)
;;   (dolist (layout-name etm-startup-layouts-list)
;;     (let
;;         ((layout-func
;;           (intern (concat "etm-open-" layout-name))))
;;       (when (fboundp layout-func)
;;         (funcall layout-func))))
;; 
;;   ;; Clean up default tab if it exists
;;   (etm-close-by-name "default")
;; 
;;   ;; Jump to first tab
;;   (when (> (length (tab-bar-tabs)) 0)
;;     (etm-navigation-jump-by-index 1))
;; 
;;   (message "ETM startup layouts loaded"))
;; 
;; (defun etm-startup-edit-layouts ()
;;   "Edit the list of startup layouts through customize interface."
;;   (interactive)
;;   (customize-variable 'etm-startup-layouts-list))
;; 
;; 
;; (provide 'etm-core-startup)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-core-startup.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-core/etm-core-startup.el
;; --------------------------------------------------------------------------------

;;; test-etm-core-startup.el ends here
