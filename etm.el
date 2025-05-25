;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-19 07:05:10>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; Commentary:
;;
;; Emacs Tab Manager (ETM) - Enhanced Tab Bar Mode
;;
;; ETM extends Emacs' built-in tab-bar.el with advanced buffer management
;; and layout features.
;;
;; Features:
;; - Buffer type system (home, semi-home, results by default)
;;   - Register buffers with types per tab
;;   - Navigate between typed buffers
;;   - Smart buffer killing (kill/hide based on registration)
;; - Layout management
;;   - Save/load window configurations
;;   - Remote host support with path mirroring
;;
;; Usage:
;; (require 'etm)
;; (etm-init)  ;; Initializes and enables enhanced tab-bar mode
;;

;;; Code:

;; 1. Load path setup
;; ----------------------------------------

(let ((this-dir (file-name-directory
                 (or load-file-name buffer-file-name))))
  (add-to-list 'load-path this-dir)
  (dolist (dir '("etm-core" "etm-tabs" "etm-buffer" "etm-close"
                 "etm-layout" "etm-keys" "etm-groups" "etm-remote"
                 "etm-layout/saved-layouts"))
    (add-to-list 'load-path
                 (expand-file-name dir this-dir))))

;; 2. Core functionality
;; ----------------------------------------

(require 'etm-core)

;; 3. Feature modules
;; ----------------------------------------

(require 'etm-tabs)    ;; Tab creation and management
(require 'etm-buffer)  ;; Buffer registration and navigation
(require 'etm-close)   ;; Tab closing utilities
(require 'etm-layout)  ;; Window layout management
(require 'etm-keys)    ;; Keybindings for ETM functions
(require 'etm-groups)  ;; Buffer grouping system
(require 'etm-remote nil t)  ;; Enhanced remote support (optional)

;; 4. Initialization and startup
;; ----------------------------------------

(require 'etm-core-init)
(require 'etm-core-startup)

;; 5. Backward compatibility layer
;; ----------------------------------------
;;
;; This section maintains compatibility with code that uses the old module
;; structure. Each provide statement maps old module names to their new
;; locations in the modular structure.

;; Navigation functionality (moved to etm-buffer-navigation)
(require 'etm-buffer-navigation)

;; Tab creation functionality (moved to etm-tabs-new-and-rename)
(require 'etm-tabs-new-and-rename)

;; Variables (moved to etm-core-variables)
(require 'etm-core-variables)

;; Initialization (moved to etm-core-init)
(require 'etm-core-init)

;; Tab ID management (moved to etm-core-tab-id)
(require 'etm-core-tab-id)

(defalias 'save-layout 'etm-layout-save)

(defalias 'save-layout-startup 'etm-startup-edit-layouts)

(etm-init)
;; 6. Package finalization
;; ----------------------------------------

;;; etm.el ends here


(provide 'etm)

(when
    (not load-file-name)
  (message "etm.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))