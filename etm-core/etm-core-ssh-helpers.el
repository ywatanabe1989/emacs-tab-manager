;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-19 06:56:42>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-core/etm-core-ssh-helpers.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; Commentary:
;; SSH helper functions for ETM

(require 'etm-core-variables)
(require 'vterm)

;; Use the customizable variable instead of duplicating the list
(defvar --etm-local-host-names etm-localhost-names
  "List of host names considered as local machines.")

(defvar --etm-ssh-hostname-username nil
  "Cache for SSH configuration cache: ((hostname . username) ...).")

(defun --etm-ssh-parse-dot-ssh ()
  "Update the SSH configuration cache from config files."
  (interactive)
  (let* ((command
          "awk '/^Host / {host=$2} /^[[:space:]]*User / {printf \"%s %s\\n\", host, $2}' ~/.ssh/config ~/.ssh/conf.d/*.conf 2>/dev/null")
         (output
          (shell-command-to-string command))
         (pairs
          (mapcar
           (lambda (line)
             (let ((parts (split-string line)))
               (cons (nth 0 parts) (nth 1 parts))))
           (split-string output "\n" t))))
    (setq --etm-ssh-hostname-username
          (cons '("localhost" . "ywatanabe")
                pairs))))

(defun --etm-ssh-select-host ()
  "Select an SSH host from cached config."
  (interactive)
  (--etm-ssh-parse-dot-ssh)
  ;; In non-interactive mode (like tests), return localhost
  (if (not (called-interactively-p 'any))
      "localhost"
    (let ((host (completing-read "Choose host: "
                               (mapcar #'car --etm-ssh-hostname-username))))
      ;; If user enters 'l', convert it to 'localhost'
      (if (string= host "l") "localhost" host))))

(defun --etm-ssh-rename-username (path &optional host)
  "Rename username in PATH based on HOST.
If HOST is 'titan', replace 'ywatanabe' with 'yusukew'.
Otherwise, replace 'yusukew' with 'ywatanabe'.

Example:
(--my/ssh-rename-username \"/home/ywatanabe/file.txt\" \"titan\")
=> \"/home/yusukew/file.txt\"

(--my/ssh-rename-username \"/home/yusukew/file.txt\" \"other-host\")
=> \"/home/ywatanabe/file.txt\""
  (interactive)
  (if
      (string= host "titan")
      (replace-regexp-in-string "ywatanabe" "yusukew" path)
    (replace-regexp-in-string "yusukew" "ywatanabe" path)))

(defun --etm-vterm-new (term-name)
  (interactive "sVterm Name: ")
  (let* ((term-name
          (concat term-name "-" (format-time-string "%H:%M:%S")))
         (buffer-name (format "%s" term-name))
         (counter 1)
         (dir default-directory)
         (default-directory (expand-file-name
                             (if (and dir
                                      (not (file-remote-p dir))
                                      (file-exists-p dir))
                                 dir
                               "~/"))))
    (while (get-buffer buffer-name)
      (setq buffer-name (format "Term: %s<%d>" term-name counter)
            counter (1+ counter)))
    (let ((vterm-buffer (vterm buffer-name)))
      (with-current-buffer vterm-buffer
        vterm-buffer))))


(provide 'etm-core-ssh-helpers)

(when
    (not load-file-name)
  (message "etm-core-ssh-helpers.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))