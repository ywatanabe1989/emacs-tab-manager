;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-01-02 06:26:06>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-.ssh.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-.ssh ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions ".ssh"
                                      '((file "~/.ssh/" 0 1 91 70 nil)
                                        (shell "~/.ssh/" 91 1 91 70 nil)
                                        (shell "~/.ssh/" 182 1 92 70 nil))
                                      nil))

(defalias '.ssh 'etm-open-.ssh)


(provide 'etm-open-.ssh)

(when
    (not load-file-name)
  (message "etm-open-.ssh.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))