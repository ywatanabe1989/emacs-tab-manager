;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 08:49:30>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-core/etm-core.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;;
;; Core Module for ETM (Emacs Tab Manager)
;;
;; This module provides the essential building blocks used throughout ETM.
;; It includes variables, configuration parameters, helper functions,
;; and tab identifier management.
;;
;; Core Components:
;; - etm-core-variables: Basic variables and data structures
;; - etm-core-variables-custom: Customizable user options
;; - etm-core-helpers: Helper functions for various ETM operations
;; - etm-core-tab-id: Tab identification and management
;; - etm-core-init: Initialization functions (loaded separately)
;; - etm-core-startup: Startup procedures (loaded separately)
;;
;;; Code:

;; Load core components
(require 'etm-core-variables)       ;; Basic variables
(require 'etm-core-variables-custom) ;; User-customizable options
(require 'etm-core-helpers)         ;; Helper functions
(require 'etm-core-tab-id)          ;; Tab identification

;; Note: etm-core-init and etm-core-startup are loaded separately in etm.el
;; to maintain proper initialization order

(provide 'etm-core)

(when (not load-file-name)
  (message "etm-core.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

;;; etm-core.el ends here