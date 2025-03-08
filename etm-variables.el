;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-27 12:12:54>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-variables.el

;; Basic
;; ----------------------------------------

(defcustom etm-localhost-names
  '("" "ywata-note-win" "localhost")
  "List of names considered as localhost in ETM."
  :type
  '(repeat string)
  :group 'etm)

(defcustom etm-ignored-host
  "ignored-host"
  "Host name to be ignored in ETM."
  :type 'string
  :group 'etm)

(defconst etm-version "0.1.0")

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
  (expand-file-name "etm-layout/saved-layouts"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "Directory path for saving ETM layouts."
  :type 'directory
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

(provide 'etm-variables)

(when
    (not load-file-name)
  (message "etm-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))