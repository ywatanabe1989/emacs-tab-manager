;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-01-05 20:56:13>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-.bin.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-.bin ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions ".bin"
                                      '((file "~/.dotfiles/.bin/" 0 1 91 70 "localhost")
                                        (shell "~/.dotfiles/.bin/" 91 1 91 70 "localhost")
                                        (shell "~/.dotfiles/.bin/" 182 1 92 70 "localhost"))
                                      "localhost"))

(defalias '.bin 'etm-open-.bin)


(provide 'etm-open-.bin)

(when
    (not load-file-name)
  (message "etm-open-.bin.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))