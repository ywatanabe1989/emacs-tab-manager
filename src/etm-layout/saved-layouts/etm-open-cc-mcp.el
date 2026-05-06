;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-03-14 02:04:32>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-cc-mcp.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-cc-mcp ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "cc-mcp"
                                      '((file "~/.claude/mcp-configs-dynamic/" 0 1 122 94 "localhost")
                                        (shell "~/.claude/mcp-configs-dynamic/" 122 1 122 94 "localhost")
                                        (shell "~/.claude/mcp-configs-dynamic/" 244 1 122 94 "localhost"))
                                      "localhost"))

(defalias 'cc-mcp 'etm-open-cc-mcp)


(provide 'etm-open-cc-mcp)

(when
    (not load-file-name)
  (message "etm-open-cc-mcp.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))