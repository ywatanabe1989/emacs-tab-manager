;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 00:22:13>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-layout/saved-layouts/etm-open-llemacs-prompt-03-rules.el

(defun etm-open-llemacs-prompt-rules
    ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create "rules" 1 1
                       '((file . "~/proj/llemacs/workspace/resources/prompts/components/03-rules/")
                         (file . "~/proj/llemacs/workspace/resources/prompts/components/03-rules/"))
                       "localhost"))

(when
    (not load-file-name)
  (message "%s loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

(provide 'etm-open-llemacs-prompt-03-rules)

(when
    (not load-file-name)
  (message "etm-open-llemacs-prompt-03-rules.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))