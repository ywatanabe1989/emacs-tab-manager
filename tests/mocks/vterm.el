;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: Claude
;;; Timestamp: <2025-05-19 06:30:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/mocks/vterm.el

;;; Commentary:
;; Mock implementation of vterm for ETM tests.
;; This provides just enough functionality to satisfy the ETM test requirements
;; without needing the actual vterm package installed.

;;; Code:

(defvar vterm-buffer-name nil
  "Name of the vterm buffer.")

(defun vterm ()
  "Create and return a buffer that simulates vterm."
  (interactive)
  (let ((buf (get-buffer-create (or vterm-buffer-name "*vterm*"))))
    (with-current-buffer buf
      ;; Set major mode to something that's always available
      (fundamental-mode)
      ;; Add a property to identify this as a mock vterm buffer
      (setq-local vterm-mock t))
    (switch-to-buffer buf)
    buf))

(defun vterm-send-string (str)
  "Simulate sending STR to the terminal.
In the mock version, we simply append the string to the buffer."
  (with-current-buffer (current-buffer)
    (goto-char (point-max))
    (insert str)
    (goto-char (point-max))))

(provide 'vterm)

;;; vterm.el ends here