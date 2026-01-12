;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 00:22:15>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-layout/saved-layouts/etm-open-llemacs-prompt-99-factory.el

(defun etm-open-llemacs-prompt-factory
    ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create "prompt-factory" 5 5
                       '((file . "~/proj/llemacs/workspace/resources/prompts/compiled/")
                         (file . "~/proj/llemacs/workspace/resources/prompts/recipes/")
                         (file . "~/proj/llemacs/workspace/resources/prompts/components/01-roles/")
                         (file . "~/proj/llemacs/workspace/resources/prompts/components/02-tasks/")
                         (file . "~/proj/llemacs/workspace/resources/prompts/components/03-rules/")
                         (file . "~/proj/llemacs/workspace/resources/prompts/components/04-formats/")
                         (file . "~/proj/llemacs/workspace/resources/prompts/components/05-examples/")
                         (file . "~/proj/llemacs/workspace/resources/prompts/components/06-tools/")
                         (file . "~/proj/llemacs/workspace/resources/prompts/components/07-resources/")
                         (file . "~/proj/llemacs/workspace/resources/prompts/components/08-requests/"))
                       "localhost"))

(when
    (not load-file-name)
  (message "%s loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

(provide 'etm-open-llemacs-prompt-99-factory)

(when
    (not load-file-name)
  (message "etm-open-llemacs-prompt-99-factory.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))