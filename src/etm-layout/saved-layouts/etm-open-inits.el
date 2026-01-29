;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-25 06:54:32>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-inits.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-inits ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "inits"
                                      '((file "~/.emacs.d/inits/" 0 1 106 81 "localhost")
                                        (shell "~/.emacs.d/inits/" 106 1 107 81 "localhost")
                                        (shell "~/.emacs.d/inits/" 213 1 107 81 "localhost"))
                                      "localhost"))

(defalias 'inits 'etm-open-inits)


(provide 'etm-open-inits)

(when
    (not load-file-name)
  (message "etm-open-inits.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))