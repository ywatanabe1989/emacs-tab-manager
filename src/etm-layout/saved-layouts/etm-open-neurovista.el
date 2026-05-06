;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-05-06 07:28:58>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-neurovista.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-neurovista ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "neurovista"
                                      '((file "~/proj/neurovista/" 0 1 91 70 "sp")
                                        (shell "~/proj/neurovista/" 91 1 91 70 "sp")
                                        (shell "~/proj/neurovista/" 182 1 92 70 "sp"))
                                      nil))

(defalias 'neurovista 'etm-open-neurovista)


(provide 'etm-open-neurovista)

(when
    (not load-file-name)
  (message "etm-open-neurovista.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))