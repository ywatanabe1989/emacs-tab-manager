;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 08:24:08>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/etm.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(let
    ((this-dir
      (file-name-directory
       (or load-file-name buffer-file-name))))
  (add-to-list 'load-path this-dir)
  (dolist
      (dir
       '("etm-core" "etm-new-and-rename" "etm-buffer" "etm-close"
         "etm-layout"
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


(provide 'etm)

(when
    (not load-file-name)
  (message "etm.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))