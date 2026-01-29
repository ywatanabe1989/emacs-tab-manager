;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-11-18 15:21:26>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-example-research.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-example-research ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "example-research"
                                      '((file "~/proj/examples/scitex_template_research/" 0 1 120 62 nil)
                                        (shell "~/proj/examples/scitex_template_research/" 120 1 120 62 nil))
                                      nil))

(defalias 'example-research 'etm-open-example-research)


(provide 'etm-open-example-research)

(when
    (not load-file-name)
  (message "etm-open-example-research.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))