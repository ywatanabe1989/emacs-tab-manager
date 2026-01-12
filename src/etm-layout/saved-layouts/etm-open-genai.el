;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-24 13:17:16>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-genai.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

(defun etm-open-genai ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "genai"
                                      '((file "~/.emacs.d/lisp/genai/" 0 1 120 61 nil)
                                        (shell "~/.emacs.d/lisp/genai/" 120 1 120 61 nil))
                                      nil))

(defalias 'genai 'etm-open-genai)


(provide 'etm-open-genai)

(when
    (not load-file-name)
  (message "etm-open-genai.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))