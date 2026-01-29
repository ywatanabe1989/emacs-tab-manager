;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-01-04 13:18:26>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-scitex-code.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-scitex-code ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "scitex-code"
                                      '((file "~/proj/scitex-code/" 0 1 91 70 nil)
                                        (shell "~/proj/scitex-code/" 91 1 91 70 nil)
                                        (shell "~/proj/scitex-code/" 182 1 92 70 nil))
                                      nil))

(defalias 'scitex-code 'etm-open-scitex-code)


(provide 'etm-open-scitex-code)

(when
    (not load-file-name)
  (message "etm-open-scitex-code.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))