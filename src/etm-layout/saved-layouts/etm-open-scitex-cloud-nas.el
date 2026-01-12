;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-12 00:15:54>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-scitex-cloud-nas.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-scitex-cloud-nas ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "scitex-cloud-nas"
                                      '((file "~/proj/scitex-cloud/" 0 1 80 62 "nas")
                                        (shell "~/proj/scitex-cloud/" 80 1 80 62 "nas")
                                        (shell "~/proj/scitex-cloud/" 160 1 80 62 "nas"))
                                      "nas"))

(defalias 'scitex-cloud-nas 'etm-open-scitex-cloud-nas)


(provide 'etm-open-scitex-cloud-nas)

(when
    (not load-file-name)
  (message "etm-open-scitex-cloud-nas.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))