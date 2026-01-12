;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 00:22:13>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-layout/saved-layouts/etm-open-llemacs-prompt-04-formats.el

(defun etm-open-llemacs-prompt-formats
    ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create "formats" 2 1
                       '((file . "~/proj/llemacs/workspace/resources/prompts/components/formats/")
                         (file . "~/proj/llemacs/workspace/resources/prompts/components/formats/")
                         (file . "/home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/formats/not-specified.md"))
                       "localhost"))

(when
    (not load-file-name)
  (message "%s loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

(provide 'etm-open-llemacs-prompt-04-formats)

(when
    (not load-file-name)
  (message "etm-open-llemacs-prompt-04-formats.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))