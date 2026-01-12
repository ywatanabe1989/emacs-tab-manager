;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-01-04 15:13:56>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-crossref-nas.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-crossref-nas ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "crossref-nas"
                                      '((file "~/proj/crossref_local/" 0 1 91 70 "nas")
                                        (shell "~/proj/crossref_local/" 91 1 91 70 "nas")
                                        (shell "~/proj/crossref_local/" 182 1 92 70 "nas"))
                                      "nas"))

(defalias 'crossref-nas 'etm-open-crossref-nas)


(provide 'etm-open-crossref-nas)

(when
    (not load-file-name)
  (message "etm-open-crossref-nas.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))