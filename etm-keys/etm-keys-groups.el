;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-01-25 15:58:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-keys/etm-keys-groups.el

(require 'etm-groups)
(require 'etm-keys-command-map)

;; Bind group commands under 'g' prefix
(define-key etm-command-map (kbd "g") etm-groups-command-map)

;; Quick access keys for common group operations
(define-key etm-command-map (kbd "G") #'etm-groups-switch-interactive)
(define-key etm-command-map (kbd "A") #'etm-groups-add-current-buffer)

(provide 'etm-keys-groups)

(when (not load-file-name)
  (message "etm-keys-groups.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))