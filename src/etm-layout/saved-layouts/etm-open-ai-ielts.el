;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-29 18:58:11>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-ai-ielts.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-ai-ielts ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "ai-ielts"
                                      '((file "~/proj/ai-ielts/" 0 1 54 70 nil)
                                        (shell "~/proj/ai-ielts/" 54 1 55 70 nil)
                                        (shell "~/proj/ai-ielts/" 109 1 55 70 nil)
                                        (shell "~/proj/ai-ielts/" 164 1 55 70 nil)
                                        (shell "~/proj/ai-ielts/" 219 1 55 70 nil))
                                      nil))

(defalias 'ai-ielts 'etm-open-ai-ielts)


(provide 'etm-open-ai-ielts)

(when
    (not load-file-name)
  (message "etm-open-ai-ielts.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))