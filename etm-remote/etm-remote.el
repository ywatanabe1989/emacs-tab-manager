;;; etm-remote.el --- Enhanced remote support for ETM -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Time-stamp: <2025-05-25 16:15:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-remote/etm-remote.el

;;; Commentary:
;; This module provides enhanced remote support for ETM, including:
;; - Multi-method TRAMP connection management
;; - Visual indicators for remote buffers
;; - Remote-aware navigation commands
;; - Graceful error handling

;;; Code:

(require 'etm-remote-connection)
(require 'etm-remote-indicators)

;; Future modules will be added here:
;; (require 'etm-remote-navigation)
;; (require 'etm-remote-errors)
;; (require 'etm-remote-layout)

(defgroup etm-remote nil
  "Enhanced remote support for ETM."
  :group 'etm
  :prefix "etm-remote-")

(defvar etm-remote-version "0.1.0"
  "Version of ETM remote support module.")

;;;###autoload
(defun etm-remote-init ()
  "Initialize ETM remote support."
  (interactive)
  ;; Start health monitoring
  (etm-remote-start-health-monitoring)
  ;; Initialize visual indicators
  (etm-remote-indicators-init)
  (message "ETM remote support initialized (v%s)" etm-remote-version))

(defun etm-remote-cleanup ()
  "Cleanup ETM remote support."
  (interactive)
  ;; Stop health monitoring
  (etm-remote-stop-health-monitoring)
  ;; Cleanup visual indicators
  (etm-remote-indicators-cleanup)
  ;; Cleanup all connections
  (etm-remote-cleanup-all)
  (message "ETM remote support cleaned up"))

(provide 'etm-remote)

;;; etm-remote.el ends here