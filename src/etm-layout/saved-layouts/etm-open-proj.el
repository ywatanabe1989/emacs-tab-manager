;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-29 06:13:30>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-proj.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-proj ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "proj"
                                      '((file "~/proj/" 0 1 56 70 nil)
                                        (shell "~/proj/" 56 1 54 70 nil)
                                        (shell "~/proj/" 110 1 54 70 nil)
                                        (shell "~/proj/" 164 1 54 70 nil)
                                        (shell "~/proj/" 218 1 56 70 nil))
                                      nil))

(defalias 'proj 'etm-open-proj)


(provide 'etm-open-proj)

(when
    (not load-file-name)
  (message "etm-open-proj.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))