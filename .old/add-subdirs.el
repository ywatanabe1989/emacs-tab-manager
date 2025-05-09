;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-09 23:28:08>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/add-subdirs.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;; Add subdirectories to load-path

(defun add-subdirs-to-load-path (base-dir)
  "Add BASE-DIR and its subdirectories to `load-path'."
  (add-to-list 'load-path base-dir)
  (dolist (dir (directory-files base-dir t))
    (when (and (file-directory-p dir)
               (not
                (string-match "\\`\\." (file-name-nondirectory dir))))
      (add-to-list 'load-path dir))))

;; Function to add emacs-tab-manager directories to load-path

(defun etm-add-to-load-path ()
  "Add all emacs-tab-manager directories to load-path."
  (let ((project-root (file-name-directory load-file-name)))
    (add-subdirs-to-load-path project-root)))

;; Add project root and subdirectories by default when loading the file
(when load-file-name
  (let ((project-root (file-name-directory load-file-name)))
    (add-subdirs-to-load-path project-root)))


(provide 'add-subdirs)

(when
    (not load-file-name)
  (message "add-subdirs.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))