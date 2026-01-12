;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 00:22:12>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-layout/saved-layouts/etm-open-llemacs-prompt-00-2-recipes.el

(defun etm-open-llemacs-prompt-recipes
    ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create "recipes" 1 1
                       '((file . "~/proj/llemacs/workspace/resources/prompts/recipes/")
                         (file . "~/proj/llemacs/workspace/resources/prompts/recipes/"))
                       "localhost"))

(when
    (not load-file-name)
  (message "%s loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

(provide 'etm-open-llemacs-prompt-00-2-recipes)

(when
    (not load-file-name)
  (message "etm-open-llemacs-prompt-00-2-recipes.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))