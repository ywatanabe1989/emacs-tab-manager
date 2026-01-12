;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-31 10:34:37>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-resell.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-resell ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "resell"
                                      '((file "~/proj/resell-from-Melbourne/" 0 1 137 70 "localhost")
                                        (shell "~/proj/resell-from-Melbourne/" 137 1 137 70 "localhost"))
                                      "localhost"))

(defalias 'resell 'etm-open-resell)


(provide 'etm-open-resell)

(when
    (not load-file-name)
  (message "etm-open-resell.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))