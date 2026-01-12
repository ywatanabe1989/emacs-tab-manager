;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-27 09:26:02>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-nv.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

(defun etm-open-nv ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "nv"
                                      '((file "~/proj/neurovista/" 0 1 79 60 "ywatanabe@sp")
                                        (shell "~/proj/neurovista/" 79 1 79 60 "ywatanabe@sp")
                                        (shell "~/proj/neurovista/" 158 1 80 60 "ywatanabe@sp"))
                                      "sp"))

(defalias 'nv 'etm-open-nv)


(provide 'etm-open-nv)

(when
    (not load-file-name)
  (message "etm-open-nv.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))