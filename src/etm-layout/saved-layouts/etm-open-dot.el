;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24 16:39:01>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-dot.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-dot ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "dot"
                                      '((file "~/.dotfiles/" 0 1 91 70 nil)
                                        (shell "~/.dotfiles/" 91 1 91 70 nil)
                                        (shell "~/.dotfiles/" 182 1 92 70 nil))
                                      nil))

(defalias 'dot 'etm-open-dot)


(provide 'etm-open-dot)

(when
    (not load-file-name)
  (message "etm-open-dot.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))