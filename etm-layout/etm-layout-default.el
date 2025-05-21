;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-19 12:01:14>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/etm-layout/etm-layout-default.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(defun etm-layout-set-default-host (layout-name host)
  "Set HOST as the default for LAYOUT-NAME."
  (interactive
   (list
    (completing-read "Layout name: "
                     (mapcar (lambda (file)
                               (string-remove-prefix "etm-open-"
                                                     (string-remove-suffix
                                                      ".el"
                                                      (file-name-nondirectory
                                                       file))))
                             (directory-files etm-layout-save-dir t
                                              "etm-open-.*\\.el$")))
    (read-string "Host: "
                 (gethash (tab-bar-tab-name-current)
                          etm-layout-default-hosts))))
  (puthash layout-name host etm-layout-default-hosts)
  (message "Set default host for %s: %s" layout-name host))

(defun etm-layout-clear-default-host (layout-name)
  "Clear default host for LAYOUT-NAME."
  (interactive
   (list
    (completing-read "Layout name: "
                     (let ((keys '()))
                       (maphash (lambda (k _v) (push k keys))
                                etm-layout-default-hosts)
                       keys))))
  (remhash layout-name etm-layout-default-hosts)
  (message "Cleared default host for %s" layout-name))


(provide 'etm-layout-default)

(when
    (not load-file-name)
  (message "etm-layout-default.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))