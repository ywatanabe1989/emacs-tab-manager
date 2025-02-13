;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 14:58:35>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-buffer/etm-buffer-kill-or-bury.el

(require 'etm-variables)
(require 'etm-buffer-checkers)

;; Kill or Bury
;; ----------------------------------------

(defun etm-buffer-kill-or-bury
    ()
  "Kill buffer if not registered in any tab, otherwise bury it."
  (interactive)
  (let
      ((buf
        (current-buffer)))
    (if
        (or
         (--etm-buffer-registered-p
          (buffer-name buf))
         (--etm-buffer-protected-p
          (buffer-name buf)))
        (progn
          (bury-buffer)
          (message "Buried"))
      (progn
        (kill-buffer)
        (message "Killed")))))

(provide 'etm-buffer-kill-or-bury)

(when
    (not load-file-name)
  (message "etm-buffer-kill-or-bury.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))