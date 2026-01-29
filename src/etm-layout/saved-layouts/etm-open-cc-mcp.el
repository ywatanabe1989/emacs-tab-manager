;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-01-01 13:32:43>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-cc-mcp.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-cc-mcp ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "cc-mcp"
                                      '((file "~/.claude/mcp-configs-dynamic/" 0 1 91 70 "localhost")
                                        (shell "~/.claude/mcp-configs-dynamic/" 91 1 91 70 "localhost")
                                        (shell "~/.claude/mcp-configs-dynamic/" 182 1 92 70 "localhost"))
                                      "localhost"))

(defalias 'cc-mcp 'etm-open-cc-mcp)


(provide 'etm-open-cc-mcp)

(when
    (not load-file-name)
  (message "etm-open-cc-mcp.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))