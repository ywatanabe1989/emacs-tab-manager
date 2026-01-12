;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-29 15:44:48>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-davinci-resolve-mcp.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-davinci-resolve-mcp ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "davinci-resolve-mcp"
                                      '((file "~/proj/davinci-resolve-mcp/" 0 1 91 70 "localhost")
                                        (shell "~/proj/davinci-resolve-mcp/" 91 1 91 70 "localhost")
                                        (shell "~/proj/davinci-resolve-mcp/" 182 1 92 70 "localhost"))
                                      "localhost"))

(defalias 'davinci-resolve-mcp 'etm-open-davinci-resolve-mcp)


(provide 'etm-open-davinci-resolve-mcp)

(when
    (not load-file-name)
  (message "etm-open-davinci-resolve-mcp.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))