;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 00:22:14>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-layout/saved-layouts/etm-open-llemacs-prompt-06-tools.el

(defun etm-open-llemacs-prompt-tools
    ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create "tools" 1 1
                       '((file . "~/proj/llemacs/workspace/resources/prompts/components/06-tools/")
                         (file . "~/proj/llemacs/workspace/resources/prompts/components/06-tools/"))
                       "localhost"))

(when
    (not load-file-name)
  (message "%s loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

(provide 'etm-open-llemacs-prompt-06-tools)

(when
    (not load-file-name)
  (message "etm-open-llemacs-prompt-06-tools.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))