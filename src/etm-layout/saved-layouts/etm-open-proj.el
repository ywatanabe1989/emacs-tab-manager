;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-01-29 06:35:21>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-proj.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-proj ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "proj"
                                      '((file "~/proj/" 0 1 91 70 nil)
                                        (shell "~/proj/" 91 1 91 70 nil)
                                        (shell "~/proj/" 182 1 92 70 nil))
                                      nil))

(defalias 'proj 'etm-open-proj)


(provide 'etm-open-proj)

(when
    (not load-file-name)
  (message "etm-open-proj.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))