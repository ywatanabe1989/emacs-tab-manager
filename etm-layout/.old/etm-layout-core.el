;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-14 02:59:31>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/etm-layout/etm-layout-core.el
;;; -*- coding: utf-8; lexical-binding: t -*-

(defun --etm-layout-cleanup
    (tab-name)
  "Perform cleanup operations after setting up the tab TAB-NAME."
  (etm-close-by-name "default"))

(provide 'etm-layout-core)

(when
    (not load-file-name)
  (message "etm-layout-core.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))