;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-01-10 21:47:23>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-figrecipe.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-figrecipe ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "figrecipe"
                                      '((file "~/proj/figrecipe/" 0 1 91 70 nil)
                                        (shell "~/proj/figrecipe/" 91 1 91 70 nil)
                                        (shell "~/proj/figrecipe/" 182 1 92 70 nil))
                                      nil))

(defalias 'figrecipe 'etm-open-figrecipe)


(provide 'etm-open-figrecipe)

(when
    (not load-file-name)
  (message "etm-open-figrecipe.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))