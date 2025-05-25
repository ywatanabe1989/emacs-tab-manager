;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-24 14:35:53>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/etm-layout/etm-layout-open.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; Commentary:
;; This module provides functionality for opening saved ETM layouts.
;; It allows users to interactively select and apply previously saved layouts.

(require 'etm-layout-load)  ; For loading layout files
(require 'etm-core-ssh-helpers)  ; For host selection
(require 'cl-lib)

(defvar etm-layout-load-hook nil
  "Hook run after a layout is loaded.")

(defun etm-layout-list-available ()
  "Return a list of available layout names without the 'etm-open-' prefix."
  (let
      ((layout-files
        (directory-files etm-layout-save-dir nil "^etm-open-.*\\.el$")))
    (mapcar (lambda (file)
              (replace-regexp-in-string
               "^etm-open-\\(.*\\)\\.el$" "\\1" file))
            layout-files)))

(defun etm-layout-function-name (layout-name)
  "Return the function name for LAYOUT-NAME."
  (intern (format "etm-open-%s" layout-name)))

(defun etm-layout-file-path (layout-name)
  "Return the file path for LAYOUT-NAME."
  (expand-file-name (format "etm-open-%s.el" layout-name)
                    etm-layout-save-dir))

(defun etm-layout-open (layout-name &optional force-reload)
  "Open the layout with LAYOUT-NAME.
Always reloads the layout file to ensure latest behavior unless cached with C-u prefix.
If FORCE-RELOAD is non-nil (C-u prefix), skip reload and use cached version."
  (interactive
   (list (completing-read "Select layout: "
                          (etm-layout-list-available)
                          nil t)
         current-prefix-arg))
  (let ((function-name (etm-layout-function-name layout-name))
        (file-path (etm-layout-file-path layout-name)))
    ;; Always reload unless explicitly using cached version with C-u
    (unless force-reload
      (if (file-exists-p file-path)
          (--etm-load-file-silent file-path)
        (error "Layout file %s does not exist" file-path)))
    ;; Call the layout function
    (if (fboundp function-name)
        (progn
          (when force-reload 
            (message "Using cached %s layout" layout-name))
          (funcall function-name)
          ;; Run load hook
          (run-hooks 'etm-layout-load-hook))
      (error "Function %s not found after loading file" function-name))))

(defun etm-layout-open-with-host (layout-name &optional host)
  "Open LAYOUT-NAME with optional HOST override.
If HOST is provided, it will override the default host in the layout."
  (interactive
   (list (completing-read "Select layout: "
                          (etm-layout-list-available)
                          nil t)
         (--etm-ssh-select-host)))
  (let ((function-name (etm-layout-function-name layout-name))
        (file-path (etm-layout-file-path layout-name)))
    ;; Load the file if the function is not already defined
    (unless (fboundp function-name)
      (if (file-exists-p file-path)
          (--etm-load-file-silent file-path)
        (error "Layout file %s does not exist" file-path)))
    ;; Check if host selection is needed
    (when (or (not host) (string-empty-p host))
      ;; No host provided, let the layout function handle host selection
      (funcall function-name))

    ;; Host provided, need to override the layout function
    (let ((original-function (symbol-function function-name)))
      (unwind-protect
          (progn
            ;; Load the file to get the source
            (--etm-load-file-silent file-path)
            ;; Read the function definition from the file
            (with-temp-buffer
              (insert-file-contents file-path)
              (goto-char (point-min))
              (when
                  (search-forward (format "(defun %s " function-name)
                                  nil t)
                (let* ((start (match-beginning 0))
                       (func-form (read (current-buffer)))
                       (body (nthcdr 3 func-form))
                       (layout-call (car (last body))))
                  ;; Extract layout name and specs from the function call
                  (when (and (listp layout-call)
                             (eq (car layout-call)
                                 '--etm-layout-create-from-positions))
                    (let ((layout-name-arg (nth 1 layout-call))
                          (specs-arg (nth 2 layout-call)))
                      ;; Define temporary function with new host
                      (eval `(defun ,function-name ()
                               "Temporary override with custom host."
                               (interactive)
                               (--etm-layout-create-from-positions
                                ,layout-name-arg
                                ,specs-arg
                                ,host))))))))
            (funcall function-name))
        ;; Restore the original function
        (fset function-name original-function)))))

(defun etm-layout-reload (layout-name)
  "Reload a specific layout function by LAYOUT-NAME."
  (interactive
   (list (completing-read "Reload layout: "
                          (etm-layout-list-available)
                          nil t)))
  (let ((function-name (etm-layout-function-name layout-name))
        (file-path (etm-layout-file-path layout-name)))
    (if (file-exists-p file-path)
        (progn
          (--etm-load-file-silent file-path)
          (message "Reloaded %s layout" layout-name))
      (error "Layout file %s does not exist" file-path))))

(defun etm-layout-reload-all ()
  "Reload all layout files from the layout directory."
  (interactive)
  (--etm-layout-load-all)
  (message "Reloaded all layout files"))

(defun etm-layout-delete (layout-name)
  "Delete the layout with LAYOUT-NAME."
  (interactive
   (list (completing-read "Delete layout: "
                          (etm-layout-list-available)
                          nil t)))
  (let ((file-path (etm-layout-file-path layout-name)))
    (when
        (yes-or-no-p
         (format "Really delete layout '%s'? " layout-name))
      (if (file-exists-p file-path)
          (progn
            (delete-file file-path)
            (message "Deleted layout '%s'" layout-name))
        (error "Layout file %s does not exist" file-path)))))


(provide 'etm-layout-open)

(when
    (not load-file-name)
  (message "etm-layout-open.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))