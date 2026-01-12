;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-28 07:29:49>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-cc-skills.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-cc-skills ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "cc-skills"
                                      '((file "~/.claude/skills/" 0 1 91 70 "localhost")
                                        (shell "~/.claude/skills/" 91 1 91 70 "localhost")
                                        (shell "~/.claude/skills/" 182 1 92 70 "localhost"))
                                      "localhost"))

(defalias 'cc-skills 'etm-open-cc-skills)


(provide 'etm-open-cc-skills)

(when
    (not load-file-name)
  (message "etm-open-cc-skills.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))