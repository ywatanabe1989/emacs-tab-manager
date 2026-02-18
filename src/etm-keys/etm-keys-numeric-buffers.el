;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-24 16:35:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-keys/etm-keys-numeric-buffers.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Keybindings for numeric buffer system in ETM
;; Provides M-t b 1, M-t b 2, etc. for quick buffer access

(require 'etm-buffer-numeric)
(require 'etm-keys-command-map)

;; Define numeric buffer keybindings
;; ----------------------------------------

(defvar etm-numeric-buffer-map (make-sparse-keymap)
  "Keymap for numeric buffer operations.")

;; Bind individual number keys to buffer jumping (0-based IDs)
;; Keys 1-9 map to IDs 0-8, key 0 maps to ID 9
(dotimes (i 9)
  (let ((key-num (1+ i))
        (id i))
    (define-key etm-numeric-buffer-map
                (kbd (number-to-string key-num))
                `(lambda ()
                   (interactive)
                   (etm-numeric-jump-to-buffer ,id)))))

(define-key etm-numeric-buffer-map
            (kbd "0")
            (lambda ()
              (interactive)
              (etm-numeric-jump-to-buffer 9)))

;; Additional commands for buffer management

(define-key etm-numeric-buffer-map (kbd "l")
	    #'etm-numeric-list-buffers)

(define-key etm-numeric-buffer-map (kbd "r")
	    #'etm-numeric-register-current-buffer)

(define-key etm-numeric-buffer-map (kbd "c")
	    #'etm-numeric-cleanup-dead-buffers)

;; Bind the buffer map to M-t b

(define-key etm-command-map (kbd "b") etm-numeric-buffer-map)

;; Direct numeric keybindings (M-t 1-9 → IDs 0-8, M-t 0 → ID 9)
(dotimes (i 9)
  (let ((key-num (1+ i))
        (id i))
    (define-key etm-command-map
                (kbd (number-to-string key-num))
                `(lambda ()
                   (interactive)
                   (etm-numeric-jump-to-buffer ,id)))))

(define-key etm-command-map
            (kbd "0")
            (lambda ()
              (interactive)
              (etm-numeric-jump-to-buffer 9)))

;; Show help for numeric buffer keys

(defun etm-numeric-buffer-help ()
  "Show help for numeric buffer keybindings."
  (interactive)
  (message (concat "ETM Numeric Buffer Keys: "
                   "M-t b r (register current buffer), "
                   "M-t b 1-9,0 (jump to buffer 0-9), "
                   "M-t b l (list all), "
                   "M-t b c (cleanup). "
                   "First register buffers with M-t b r!")))

(define-key etm-numeric-buffer-map (kbd "?") #'etm-numeric-buffer-help)

(define-key etm-numeric-buffer-map (kbd "h") #'etm-numeric-buffer-help)

(provide 'etm-keys-numeric-buffers)

;;; etm-keys-numeric-buffers.el ends here
