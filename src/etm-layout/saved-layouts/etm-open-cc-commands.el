;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-28 07:29:17>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-cc-commands.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-cc-commands ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "cc-commands"
                                      '((file "~/.claude/commands/" 0 1 91 70 "localhost")
                                        (shell "~/.claude/commands/" 91 1 91 70 "localhost")
                                        (shell "~/.claude/commands/" 182 1 92 70 "localhost"))
                                      "localhost"))

(defalias 'cc-commands 'etm-open-cc-commands)


(provide 'etm-open-cc-commands)

(when
    (not load-file-name)
  (message "etm-open-cc-commands.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))