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
       '("etm-core" "etm-tabs" "etm-buffer" "etm-close"
         "etm-layout" "etm-keys" 
         "etm-layout/saved-layouts"))
    (add-to-list 'load-path
                 (expand-file-name dir this-dir))))

;; Core functionality
(require 'etm-core)

;; Module specific features
(require 'etm-tabs)
(require 'etm-buffer)
(require 'etm-close)
(require 'etm-layout)
(require 'etm-keys)

;; Load initialization and startup
(require 'etm-core-init)
(require 'etm-core-startup)

;; -------------------------
;; Backward compatibility layer
;; -------------------------

;; Module compatibility for old module names
(require 'etm-buffer-navigation)
(provide 'etm-navigation)

(require 'etm-tabs-new-and-rename)
(provide 'etm-new-and-rename)

(require 'etm-core-variables)
(provide 'etm-variables)

(require 'etm-core-init)
(provide 'etm-init)

(require 'etm-core-tab-id)
(provide 'etm-tab-id)

;; Function aliases not needed for these functions since
;; they kept their original names in the new modules

(provide 'etm)

(when
    (not load-file-name)
  (message "etm.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))