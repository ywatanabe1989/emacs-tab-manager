;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-01-09 13:36:39>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-scitex-writer.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-scitex-writer ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "scitex-writer"
                                      '((file "~/proj/scitex-writer/" 0 1 91 70 nil)
                                        (shell "~/proj/scitex-writer/" 91 1 91 70 nil)
                                        (shell "~/proj/scitex-writer/" 182 1 92 70 nil))
                                      nil))

(defalias 'scitex-writer 'etm-open-scitex-writer)


(provide 'etm-open-scitex-writer)

(when
    (not load-file-name)
  (message "etm-open-scitex-writer.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))