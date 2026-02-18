;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-20 21:00:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/etm-core/etm-core-variables.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Core variables for ETM (Emacs Tab Manager)
;; This file defines all global variables and customization options

;; Version
;; ----------------------------------------

(defconst etm-version "2.5.0"
  "Version string for Emacs Tab Manager.")

;; Debug and messaging
;; ----------------------------------------

(defvar etm-debug-flag-master nil
  "Master debug flag. When non-nil, enables all debug output regardless of category flags.")

(defvar etm-debug-flag-core nil
  "Debug flag for core ETM operations.")

(defvar etm-debug-flag-buffer nil
  "Debug flag for buffer registration and type operations.")

(defvar etm-debug-flag-numeric nil
  "Debug flag for numeric buffer system operations.")

(defvar etm-debug-flag-close nil
  "Debug flag for tab close operations.")

(defvar etm-debug-flag-ssh nil
  "Debug flag for SSH connection operations.")

(defvar etm-debug-flag-layout nil
  "Debug flag for layout save/load operations.")

(defvar etm-debug-flag-tracking nil
  "Debug flag for automatic buffer tracking.")

(defun etm-debug-message (category format-string &rest args)
  "Log ETM debug message if CATEGORY flag or master flag is enabled.
CATEGORY should be a symbol like `core', `buffer', `numeric', `close', `ssh', `layout', `tracking'.
FORMAT-STRING and ARGS are passed to `message' with [ETM:CATEGORY] prefix."
  (let ((flag-var (intern (format "etm-debug-flag-%s" category))))
    (when (or etm-debug-flag-master
              (and (boundp flag-var) (symbol-value flag-var)))
      (apply #'message
             (format "[ETM:%s] %s" (upcase (symbol-name category))
		     format-string)
             args))))

(defun etm-toggle-debug (&optional category)
  "Toggle ETM debugging for CATEGORY.
If CATEGORY is nil, toggle master debug flag.
CATEGORY can be: master, core, buffer, numeric, close, ssh, layout, tracking."
  (interactive
   (list (intern (completing-read "Debug category: "
                                  '("master" "core" "buffer" "numeric"
                                    "close" "ssh" "layout" "tracking")
                                  nil t))))
  (let* ((cat (or category 'master))
         (flag-var (intern (format "etm-debug-flag-%s" cat)))
         (new-value (not (symbol-value flag-var))))
    (set flag-var new-value)
    (message "ETM debug [%s]: %s"
             (upcase (symbol-name cat))
             (if new-value "ENABLED" "DISABLED"))))

(defun etm-debug-status ()
  "Show current status of all ETM debug flags."
  (interactive)
  (message
   "ETM Debug Status: master=%s core=%s buffer=%s numeric=%s close=%s ssh=%s layout=%s tracking=%s"
   (if etm-debug-flag-master "ON" "off")
   (if etm-debug-flag-core "ON" "off")
   (if etm-debug-flag-buffer "ON" "off")
   (if etm-debug-flag-numeric "ON" "off")
   (if etm-debug-flag-close "ON" "off")
   (if etm-debug-flag-ssh "ON" "off")
   (if etm-debug-flag-layout "ON" "off")
   (if etm-debug-flag-tracking "ON" "off")))

;; Backward compatibility alias

(defvaralias 'etm-debug 'etm-debug-flag-master
  "Alias for backward compatibility.")

;; Basic
;; ----------------------------------------

(defcustom etm-localhost-names
  '("" "localhost")
  "List of names considered as localhost in ETM."
  :type
  '(repeat string)
  :group 'etm)

(defcustom etm-ignored-host
  "ignored-host"
  "Host name to be ignored in ETM."
  :type 'string
  :group 'etm)

(defgroup etm nil
  "Emacs Tab Manager"
  :prefix "etm-"
  :group 'applications)

;; Appearance
;; ----------------------------------------

(defcustom etm-show-tab-bar t
  "Whether to show tab bar in ETM."
  :type 'boolean
  :group 'etm)

;; Buffer types
;; ----------------------------------------

(defconst etm-default-buffer-types
  '("home" "semi-home" "results")
  "List of default buffer types supported by ETM.
These types determine how buffers are managed and displayed.")

(defcustom etm-custom-buffer-types nil
  "List of additional buffer types defined by user.
These supplement the default types in `etm-registered-buffer-types'."
  :type
  '(repeat string)
  :group 'etm)

(defcustom etm-registered-buffer-types
  etm-default-buffer-types
  "List of active buffer types in ETM.
Initialized with `etm-default-buffer-types'."
  :type
  '(repeat string)
  :group 'etm)

;; Registered buffers
;; ----------------------------------------

(defcustom etm-registered-buffers nil
  "Alist mapping tab names to their buffer configurations.
Each entry is a cons cell (NAME . CONFIG) where NAME is a string
and CONFIG is a buffer configuration sexp."
  :type
  '(alist :key-type string :value-type sexp)
  :group 'etm)

(defcustom etm-protected-buffers
  '()
  "List of buffer names that should be hidden rather than killed."
  :type
  '(repeat string)
  :group 'etm)

;; Registered Layouts
;; ----------------------------------------

(defcustom etm-layout-save-dir
  (expand-file-name "../etm-layout/saved-layouts"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "Directory path for saving ETM layouts."
  :type 'directory
  :group 'etm)

(defcustom etm-layout-default-hosts (make-hash-table :test 'equal)
  "Default hosts for layouts, stored as hash table."
  :type '(alist :key-type string :value-type string)
  :group 'etm)

(add-to-list 'load-path etm-layout-save-dir)

(defvar etm-saved-layouts nil
  "Registry of saved tab layouts.")

(defcustom etm-registered-layouts nil
  "Registry of saved tab layouts.
Each entry is a cons cell (NAME . LAYOUT) where NAME is a string
and LAYOUT is a layout configuration sexp."
  :type
  '(alist :key-type string :value-type sexp)
  :group 'etm)

;; Numeric buffer system
;; ----------------------------------------

(defcustom etm-max-numeric-buffers 10
  "Maximum number of numeric buffer slots per tab (0-9)."
  :type 'integer
  :group 'etm)

(defvar etm-numeric-buffers nil
  "Alist mapping tab names to numeric buffer configurations.
Each entry is (TAB-NAME . ((ID . BUFFER-NAME) ...)).")

;; Numeric buffer visual indicators
;; ----------------------------------------

(defcustom etm-numeric-indicators-enabled t
  "Whether to show numeric buffer indicators in tab-bar and mode-line."
  :type 'boolean
  :group 'etm)

(defcustom etm-numeric-indicator-format "[%s]"
  "Format string for numeric indicators. %s is replaced with slot info."
  :type 'string
  :group 'etm)

(defcustom etm-numeric-indicator-separator " "
  "Separator between slot numbers in indicators."
  :type 'string
  :group 'etm)

;; Add this near the other buffer-related variables

(defvar-local etm-buffer-id nil
  "Buffer-local variable to store a persistent ID for ETM buffer tracking.")

;; Automatic buffer tracking per tab
;; ----------------------------------------

(defcustom etm-auto-track-buffers t
  "When non-nil, automatically track buffers created in each tab."
  :type 'boolean
  :group 'etm)

(defcustom etm-close-kills-tracked-buffers t
  "When non-nil, `etm-close' kills all tracked buffers for the tab."
  :type 'boolean
  :group 'etm)

(defcustom etm-track-exclude-patterns
  '("\\*Messages\\*" "\\*scratch\\*" "\\*Completions\\*" "\\*Help\\*"
    "\\*Backtrace\\*" "\\*Warnings\\*" "\\*Compile-Log\\*"
    "\\*info\\*"
    "\\*Ibuffer\\*" "\\*Buffer List\\*" "^ ")
  "List of regex patterns for buffer names to exclude from auto-tracking."
  :type '(repeat string)
  :group 'etm)

(defvar etm-tab-tracked-buffers (make-hash-table :test 'equal)
  "Hash table mapping tab names to lists of tracked buffer names.")

(provide 'etm-core-variables)

(when
    (not load-file-name)
  (message "etm-core-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
