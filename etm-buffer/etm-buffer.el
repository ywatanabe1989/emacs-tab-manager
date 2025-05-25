;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 08:50:15>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-buffer/etm-buffer.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;;
;; Buffer Management Module for ETM
;;
;; This is the main entry point for ETM's buffer management functionality.
;; ETM introduces a buffer type system that allows buffers to be registered
;; with specific types (home, semi-home, results by default) within each tab.
;;
;; Features:
;; - Buffer registration with tab-specific types
;; - Navigation between typed buffers
;; - Smart buffer killing (kill or bury based on registration)
;; - Buffer type checking and retrieval
;;
;; Main Components:
;; - etm-buffer-setters: Functions for registering buffers with types
;; - etm-buffer-getters: Functions for retrieving registered buffers
;; - etm-buffer-kill-or-bury: Smart buffer killing functionality
;; - etm-buffer-jumpers: Functions for jumping between registered buffers
;; - etm-buffer-checkers: Functions for checking buffer registration status
;; - etm-buffer-navigation: Navigation between buffers of specific types
;;
;;; Code:

;; Load all buffer module components
(require 'etm-buffer-setters)     ;; Buffer registration
(require 'etm-buffer-getters)     ;; Retrieval of registered buffers
(require 'etm-buffer-kill-or-bury) ;; Smart buffer killing
(require 'etm-buffer-jumpers)     ;; Buffer jumping functions
(require 'etm-buffer-checkers)    ;; Registration status checkers
(require 'etm-buffer-navigation)  ;; Navigation between typed buffers
(require 'etm-buffer-numeric)     ;; Numeric buffer system
(require 'etm-buffer-numeric-indicators) ;; Visual indicators for numeric buffers

(provide 'etm-buffer)

(when (not load-file-name)
  (message "etm-buffer.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

;;; etm-buffer.el ends here