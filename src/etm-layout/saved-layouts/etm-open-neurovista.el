;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-09-28 17:25:04>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-neurovista.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-neurovista ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "neurovista"
                                      '((file "~/proj/neurovista/" 0 1 80 62 "sp")
                                        (shell "~/proj/neurovista/" 80 1 80 62 "sp")
                                        (shell "~/proj/neurovista/" 160 1 80 62 "sp"))
                                      "sp"))

(defalias 'neurovista 'etm-open-neurovista)


(provide 'etm-open-neurovista)

(when
    (not load-file-name)
  (message "etm-open-neurovista.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))