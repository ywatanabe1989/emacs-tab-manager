;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 00:03:51>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-layout/etm-layout-load.el
;;; -*- coding: utf-8; lexical-binding: t -*-

(defun --etm-layout-load-all
    ()
  (dolist
      (file
       (directory-files etm-layout-save-dir t "\\.el$"))
    (load-file file)))

(--etm-layout-load-all)

(provide 'etm-layout-load)

(when
    (not load-file-name)
  (message "etm-layout-load.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))