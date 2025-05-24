;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-24 16:35:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-keys/etm-keys-numeric-buffers.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:
;; Keybindings for numeric buffer system in ETM
;; Provides M-t b 1, M-t b 2, etc. for quick buffer access

(require 'etm-buffer-numeric)
(require 'etm-keys-command-map)

;; Define numeric buffer keybindings
;; ----------------------------------------

(defvar etm-numeric-buffer-map (make-sparse-keymap)
  "Keymap for numeric buffer operations.")

;; Bind individual number keys to buffer jumping
(dotimes (i etm-max-numeric-buffers)
  (let ((num (1+ i)))
    (define-key etm-numeric-buffer-map
                (kbd (number-to-string num))
                `(lambda ()
                   (interactive)
                   (etm-numeric-jump-to-buffer ,num)))))

;; Additional commands for buffer management
(define-key etm-numeric-buffer-map (kbd "l") #'etm-numeric-list-buffers)
(define-key etm-numeric-buffer-map (kbd "r") #'etm-numeric-register-current-buffer)
(define-key etm-numeric-buffer-map (kbd "c") #'etm-numeric-cleanup-dead-buffers)

;; Bind the buffer map to M-t b
(define-key etm-command-map (kbd "b") etm-numeric-buffer-map)

;; Alternative: Define direct keybindings (uncomment if preferred)
;; (dotimes (i 9)
;;   (let ((num (1+ i)))
;;     (define-key etm-command-map
;;                 (kbd (format "b %d" num))
;;                 `(lambda ()
;;                    (interactive)
;;                    (etm-numeric-jump-to-buffer ,num)))))

;; Show help for numeric buffer keys
(defun etm-numeric-buffer-help ()
  "Show help for numeric buffer keybindings."
  (interactive)
  (message (concat "ETM Numeric Buffer Keys: "
                   "M-t b 1-9 (jump), "
                   "M-t b l (list), "
                   "M-t b r (register), "
                   "M-t b c (cleanup)")))

(define-key etm-numeric-buffer-map (kbd "?") #'etm-numeric-buffer-help)
(define-key etm-numeric-buffer-map (kbd "h") #'etm-numeric-buffer-help)

(provide 'etm-keys-numeric-buffers)

;;; etm-keys-numeric-buffers.el ends here