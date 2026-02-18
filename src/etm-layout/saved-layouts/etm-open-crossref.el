;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-01-14 04:08:28>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-crossref.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-crossref ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "crossref"
                                      '((file "~/proj/crossref-local/" 0 1 91 70 nil)
                                        (shell "~/proj/crossref-local/" 91 1 91 70 nil)
                                        (shell "~/proj/crossref-local/" 182 1 92 70 nil))
                                      nil))

(defalias 'crossref 'etm-open-crossref)


(provide 'etm-open-crossref)

(when
    (not load-file-name)
  (message "etm-open-crossref.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))