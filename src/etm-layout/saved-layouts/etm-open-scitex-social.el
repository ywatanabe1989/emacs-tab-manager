;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-01-01 12:10:30>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-scitex-social.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-scitex-social ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "scitex-social"
                                      '((file "~/proj/scitex-social/" 0 1 137 70 "localhost")
                                        (shell "~/proj/scitex-social/" 137 1 137 70 "localhost"))
                                      "localhost"))

(defalias 'scitex-social 'etm-open-scitex-social)


(provide 'etm-open-scitex-social)

(when
    (not load-file-name)
  (message "etm-open-scitex-social.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))