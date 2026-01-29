;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24 05:26:25>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-cc-hooks.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-cc-hooks ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "cc-hooks"
                                      '((file "~/.claude/hooks/" 0 1 91 70 "localhost")
                                        (shell "~/.claude/hooks/" 91 1 91 70 "localhost")
                                        (shell "~/.claude/hooks/" 182 1 92 70 "localhost"))
                                      "localhost"))

(defalias 'cc-hooks 'etm-open-cc-hooks)


(provide 'etm-open-cc-hooks)

(when
    (not load-file-name)
  (message "etm-open-cc-hooks.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))