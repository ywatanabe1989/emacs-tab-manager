;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-28 07:30:05>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-cc-to-claude.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-cc-to-claude ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "cc-to-claude"
                                      '((file "~/.claude/to_claude/" 0 1 91 70 "localhost")
                                        (shell "~/.claude/to_claude/" 91 1 91 70 "localhost")
                                        (shell "~/.claude/to_claude/" 182 1 92 70 "localhost"))
                                      "localhost"))

(defalias 'cc-to-claude 'etm-open-cc-to-claude)


(provide 'etm-open-cc-to-claude)

(when
    (not load-file-name)
  (message "etm-open-cc-to-claude.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))