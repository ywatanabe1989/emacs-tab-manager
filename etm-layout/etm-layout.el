;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 00:56:11>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/etm-layout/etm-layout.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'etm-layout-create)
(require 'etm-layout-save)
(require 'etm-layout-window)
(require 'etm-layout-load)
(require 'etm-layout-open)
(require 'etm-layout-preview)


(provide 'etm-layout)

(when
    (not load-file-name)
  (message "etm-layout.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))