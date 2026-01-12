;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-01-09 14:10:08>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-calkit.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-calkit ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "calkit"
                                      '((file "~/proj/calkit/" 0 1 91 70 "localhost")
                                        (shell "~/proj/calkit/" 91 1 91 70 "localhost")
                                        (shell "~/proj/calkit/" 182 1 92 70 "localhost"))
                                      "localhost"))

(defalias 'calkit 'etm-open-calkit)


(provide 'etm-open-calkit)

(when
    (not load-file-name)
  (message "etm-open-calkit.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))