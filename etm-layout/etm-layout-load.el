;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-23 12:03:28>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/etm-layout/etm-layout-load.el

(defun --etm-load-file-silent
    (file-path)
  "Load Emacs Lisp file at FILE-PATH silently.
Suppresses all messages, warnings and outputs during loading.
Only error messages will be shown if any.

Arguments:
- FILE-PATH: Path to the Emacs Lisp file to load"
  (let
      ((inhibit-message t)
       (message-log-max nil))
    (with-temp-message ""
      (with-temp-buffer
        ;; Temporarily redirect stderr
        (let
            ((standard-output
              (current-buffer))
             (warning-minimum-level :error))
          (load-file file-path))))))

(defun --etm-layout-load-all
    ()
  (dolist
      (file
       (directory-files etm-layout-save-dir t "\\.el$"))
    (--etm-load-file-silent file)))

(--etm-layout-load-all)

(provide 'etm-layout-load)

(when
    (not load-file-name)
  (message "etm-layout-load.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))