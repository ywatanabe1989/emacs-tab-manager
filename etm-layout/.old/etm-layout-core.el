;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-19 06:52:47>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-layout/.old/etm-layout-core.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


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