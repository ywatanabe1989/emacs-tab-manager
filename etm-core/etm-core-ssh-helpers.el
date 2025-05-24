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

(defvar --etm-ssh-hostname-aliases nil
  "Cache for SSH hostname aliases: ((alias . actual-hostname) ...).")

(defvar --etm-ssh-hostname-groups nil
  "Cache for SSH hostname groups: ((actual-hostname . (alias1 alias2 ...)) ...).")

(defun --etm-ssh-parse-dot-ssh ()
  "Update the SSH configuration cache from config files."
  (interactive)
  (let* ((user-command
          "awk '/^Host / {host=$2} /^[[:space:]]*User / {printf \"%s %s\\n\", host, $2}' ~/.ssh/config ~/.ssh/conf.d/*.conf 2>/dev/null")
         (hostname-command
          "awk '/^Host / {for(i=2;i<=NF;i++) hosts[i-1]=$i; host_count=NF-1} /^[[:space:]]*HostName / {for(i=1;i<=host_count;i++) printf \"%s %s\\n\", hosts[i], $2}' ~/.ssh/config ~/.ssh/conf.d/*.conf 2>/dev/null")
         (user-output
          (shell-command-to-string user-command))
         (hostname-output
          (shell-command-to-string hostname-command))
         (user-pairs
          (mapcar
           (lambda (line)
             (let ((parts (split-string line)))
               (cons (nth 0 parts) (nth 1 parts))))
           (split-string user-output "\n" t)))
         (hostname-pairs
          (mapcar
           (lambda (line)
             (let ((parts (split-string line)))
               (cons (nth 0 parts) (nth 1 parts))))
           (split-string hostname-output "\n" t))))
    (setq --etm-ssh-hostname-username
          (cons '("localhost" . "ywatanabe") user-pairs))
    (setq --etm-ssh-hostname-aliases
          hostname-pairs)
    
    ;; Build reverse mapping: hostname -> list of aliases
    (setq --etm-ssh-hostname-groups nil)
    (dolist (pair hostname-pairs)
      (let* ((alias (car pair))
             (hostname (cdr pair))
             (existing-group (assoc hostname --etm-ssh-hostname-groups)))
        (if existing-group
            ;; Add alias to existing group
            (setcdr existing-group (cons alias (cdr existing-group)))
          ;; Create new group
          (push (cons hostname (list alias)) --etm-ssh-hostname-groups))))))

(defun --etm-ssh-select-host ()
  "Select an SSH host from cached config."
  (interactive)
  (--etm-ssh-parse-dot-ssh)
  ;; Always prompt for host selection unless explicitly in batch mode
  (if noninteractive
      "localhost"
    (let ((host (completing-read "Choose host: "
                               (mapcar #'car --etm-ssh-hostname-username))))
      ;; If user enters 'l', convert it to 'localhost'
      (if (string= host "l") "localhost" host))))

(defun --etm-ssh-resolve-hostname (host)
  "Resolve SSH alias HOST to its actual hostname.
Returns the actual hostname if HOST is an alias, otherwise returns HOST."
  (--etm-ssh-parse-dot-ssh)
  (let ((actual-hostname (cdr (assoc host --etm-ssh-hostname-aliases))))
    (or actual-hostname host)))

(defun --etm-ssh-get-hostname-aliases (hostname)
  "Get all aliases for a given HOSTNAME.
Returns a list of aliases that point to the same hostname."
  (--etm-ssh-parse-dot-ssh)
  (cdr (assoc hostname --etm-ssh-hostname-groups)))

(defun --etm-ssh-get-all-equivalent-hosts (host)
  "Get all hosts (including aliases) equivalent to HOST.
Returns a list including the resolved hostname and all its aliases."
  (let* ((resolved-hostname (--etm-ssh-resolve-hostname host))
         (aliases (--etm-ssh-get-hostname-aliases resolved-hostname)))
    ;; Include the resolved hostname itself and all aliases
    (cons resolved-hostname aliases)))

(defun --etm-ssh-find-connection-for-any-alias (hostname-or-alias)
  "Find existing SSH connection for HOSTNAME-OR-ALIAS or any of its aliases.
Returns (connection-file . matched-hostname) if found, nil otherwise."
  (let* ((all-equivalent-hosts (--etm-ssh-get-all-equivalent-hosts hostname-or-alias))
         (connection-found nil))
    
    ;; Try to find a connection for any of the equivalent hosts
    (dolist (host all-equivalent-hosts)
      (unless connection-found
        (let* ((escaped-host (regexp-quote host))
               ;; Use colon boundaries to match SSH control socket format
               (pattern (format "\\.control.*:%s:" escaped-host))
               (matches (directory-files "~/.ssh" nil pattern)))
          (when matches
            (setq connection-found (cons (car matches) host))))))
    
    connection-found))

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
  (let* ((timestamp (format-time-string "%H:%M:%S"))
         (random-num (format "%04d" (random 10000)))
         (unique-suffix (concat "-" timestamp "-" random-num))
         (term-name (concat term-name unique-suffix))
         (buffer-name (format "%s" term-name))
         (dir default-directory)
         (default-directory (expand-file-name
                             (if (and dir
                                      (not (file-remote-p dir))
                                      (file-exists-p dir))
                                 dir
                               "~/"))))
    ;; With timestamp and random number, conflicts should be extremely rare
    ;; But keep the counter as fallback
    (let ((counter 1))
      (while (get-buffer buffer-name)
        (setq buffer-name (format "%s<%d>" term-name counter)
              counter (1+ counter))))
    (let ((vterm-buffer (vterm buffer-name)))
      (with-current-buffer vterm-buffer
        vterm-buffer))))


(provide 'etm-core-ssh-helpers)

(when
    (not load-file-name)
  (message "etm-core-ssh-helpers.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))