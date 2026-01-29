;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-01-01 12:40:59>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-social.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-social ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "social"
                                      '((file "~/proj/social/" 0 1 137 70 "localhost")
                                        (shell "~/proj/social/" 137 1 137 70 "localhost"))
                                      "localhost"))

(defalias 'social 'etm-open-social)


(provide 'etm-open-social)

(when
    (not load-file-name)
  (message "etm-open-social.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))