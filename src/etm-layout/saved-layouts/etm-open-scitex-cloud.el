;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-01-03 20:59:32>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-scitex-cloud.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-scitex-cloud ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "scitex-cloud"
                                      '((file "~/proj/scitex-cloud/" 0 1 91 70 nil)
                                        (shell "~/proj/scitex-cloud/" 91 1 91 70 nil)
                                        (shell "~/proj/scitex-cloud/" 182 1 92 70 nil))
                                      nil))

(defalias 'scitex-cloud 'etm-open-scitex-cloud)


(provide 'etm-open-scitex-cloud)

(when
    (not load-file-name)
  (message "etm-open-scitex-cloud.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))