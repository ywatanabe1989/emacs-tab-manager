;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 00:22:12>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-layout/saved-layouts/etm-open-llemacs-prompt-01-roles.el

(defun etm-open-llemacs-prompt-roles
    ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create "roles" 1 1
                       '((file . "~/proj/llemacs/workspace/resources/prompts/components/01-roles/")
                         (file . "~/proj/llemacs/workspace/resources/prompts/components/01-roles/"))
                       "localhost"))

(when
    (not load-file-name)
  (message "%s loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

(provide 'etm-open-llemacs-prompt-01-roles)

(when
    (not load-file-name)
  (message "etm-open-llemacs-prompt-01-roles.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))