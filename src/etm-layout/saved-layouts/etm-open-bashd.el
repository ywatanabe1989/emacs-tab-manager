;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-24 16:35:50>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-bashd.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

(defun etm-open-bashd ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "bashd"
                                      '((file "~/.bash.d/" 0 1 80 62 nil)
                                        (shell "~/.bash.d/" 80 1 80 62 nil)
                                        (shell "~/.bash.d/" 160 1 80 62 nil))
                                      nil))

(defalias 'bashd 'etm-open-bashd)


(provide 'etm-open-bashd)

(when
    (not load-file-name)
  (message "etm-open-bashd.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))