;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-03-09 04:28:14>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-scitex-cloud-local.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-scitex-cloud-local ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "scitex-cloud-local"
                                      '((file "~/proj/scitex-cloud/" 0 1 68 70 "localhost")
                                        (shell "~/proj/scitex-cloud/" 68 1 68 70 "localhost")
                                        (shell "~/proj/scitex-cloud/" 136 1 69 70 "localhost")
                                        (shell "~/proj/scitex-cloud/" 205 1 69 70 "localhost"))
                                      "localhost"))

(defalias 'scitex-cloud-local 'etm-open-scitex-cloud-local)


(provide 'etm-open-scitex-cloud-local)

(when
    (not load-file-name)
  (message "etm-open-scitex-cloud-local.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))