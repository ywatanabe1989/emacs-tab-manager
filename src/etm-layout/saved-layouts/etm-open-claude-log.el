;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-01-09 16:58:13>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-claude-log.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-claude-log ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "claude-log"
                                      '((file "~/proj/claude-log/" 0 1 91 70 nil)
                                        (shell "~/proj/claude-log/" 91 1 91 70 nil)
                                        (shell "~/proj/claude-log/" 182 1 92 70 nil))
                                      nil))

(defalias 'claude-log 'etm-open-claude-log)


(provide 'etm-open-claude-log)

(when
    (not load-file-name)
  (message "etm-open-claude-log.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))