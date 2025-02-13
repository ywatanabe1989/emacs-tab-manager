;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 13:30:31>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm.el

;; Add to load path

(let
    ((this-dir
      (file-name-directory
       (or load-file-name buffer-file-name))))
  (add-to-list 'load-path this-dir)
  (dolist
      (dir
       '("etm-new-and-rename" "etm-buffer" "etm-close" "etm-layout" "etm-keys" "saved-layouts"))
    (add-to-list 'load-path
                 (expand-file-name dir this-dir))))

(require 'etm-variables)
(require 'etm-new-and-rename)
(require 'etm-buffer)
(require 'etm-close)
(require 'etm-layout)
(require 'etm-navigation)
(require 'etm-keys)
(require 'etm-init)

(provide 'etm)

(when
    (not load-file-name)
  (message "etm.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))