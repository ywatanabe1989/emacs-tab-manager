;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-09 19:33:01>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/etm-core/test-etm-core-helpers.el

(require 'ert)

(ert-deftest test-etm-core-helpers-loadable
    ()
  (require 'etm-core-helpers)
  (should
   (featurep 'etm-core-helpers)))

(ert-deftest test-etm-core-helpers-functions-exist
    ()
  ;; Load required modules
  (require 'etm-core-ssh-helpers)
  (require 'etm-core-helpers)
  
  ;; Test SSH helper functions
  (should (fboundp '--etm-ssh-select-host))
  (should (fboundp '--etm-ssh-rename-username))
  (should (fboundp '--etm-vterm-new)))

(provide 'test-etm-core-helpers)

(when
    (not load-file-name)
  (message "test-etm-core-helpers.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-core/etm-core-helpers.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-05-09 19:40:15>
;; ;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-core/etm-core-helpers.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; ;;; Commentary:
;; ;; Core utility functions for ETM (Emacs Tab Manager)
;; ;; These are general-purpose helper functions used throughout the package
;; 
;; (require 'etm-core-variables)
;; (require 'etm-core-ssh-helpers)
;; 
;; ;; This file provides core utility functions used throughout the ETM package.
;; ;; SSH-related helpers and terminal helpers are now in etm-core-ssh-helpers.el
;; 
;; (provide 'etm-core-helpers)
;; 
;; (when (not load-file-name)
;;   (message "etm-core-helpers.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-core/etm-core-helpers.el
;; --------------------------------------------------------------------------------

;;; test-etm-core-helpers.el ends here
