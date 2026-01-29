;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24 05:22:32>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-lisp.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-lisp ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "lisp"
                                      '((file "~/.emacs.d/lisp/" 0 1 91 70 "localhost")
                                        (shell "~/.emacs.d/lisp/" 91 1 91 70 "localhost")
                                        (shell "~/.emacs.d/lisp/" 182 1 92 70 "localhost"))
                                      "localhost"))

(defalias 'lisp 'etm-open-lisp)


(provide 'etm-open-lisp)

(when
    (not load-file-name)
  (message "etm-open-lisp.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))