;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-28 07:29:27>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-cc-agents.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-cc-agents ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "cc-agents"
                                      '((file "~/.claude/agents/" 0 1 91 70 "localhost")
                                        (shell "~/.claude/agents/" 91 1 91 70 "localhost")
                                        (shell "~/.claude/agents/" 182 1 92 70 "localhost"))
                                      "localhost"))

(defalias 'cc-agents 'etm-open-cc-agents)


(provide 'etm-open-cc-agents)

(when
    (not load-file-name)
  (message "etm-open-cc-agents.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))