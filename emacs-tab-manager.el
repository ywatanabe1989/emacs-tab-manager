;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-09 18:56:01>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/emacs-tab-manager.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(let
    ((this-dir
      (file-name-directory
       (or load-file-name buffer-file-name))))
  (add-to-list 'load-path this-dir)
  (dolist
      (dir
       '("etm-core" "etm-new-and-rename" "etm-buffer" "etm-close" "etm-layout"
         "etm-keys" "etm-layout/saved-layouts"))
    (add-to-list 'load-path
                 (expand-file-name dir this-dir))))

(require 'etm-core)
(require 'etm-new-and-rename)
(require 'etm-buffer)
(require 'etm-close)
(require 'etm-layout)
(require 'etm-navigation)
(require 'etm-keys)
(require 'etm-init)
(require 'etm-startup)

(defalias 'emacs-tab-manager-save-layout 'etm-layout-save)

(defalias 'emacs-tab-manager-save-layout-startup
  'etm-startup-edit-layouts)


(provide 'emacs-tab-manager)

(when
    (not load-file-name)
  (message "emacs-tab-manager.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))