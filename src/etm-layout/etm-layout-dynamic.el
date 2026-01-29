;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2026-01-30 00:00:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/etm-layout-dynamic.el

;;; Copyright (C) 2026 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Dynamic command generation for project directories.
;; Automatically creates M-x commands based on directory structure:
;;   ~/.emacs.d/lisp/xxx -> M-x lisp-xxx
;;   ~/proj/xxx -> M-x proj-xxx
;;
;; Each command creates a 3-column layout:
;;   Left: dired (project directory)
;;   Center: vterm
;;   Right: vterm

;;; Code:

(require 'etm-layout-create)

;; Configuration
;; ----------------------------------------

(defvar etm-dynamic-project-dirs
  '(("lisp" . "~/.emacs.d/lisp")
    ("proj" . "~/proj"))
  "Alist mapping command prefix to project directory.
Each entry is (PREFIX . DIRECTORY).
For each subdirectory in DIRECTORY, a command PREFIX-SUBDIRNAME is created.")

(defvar etm-dynamic-layout-spec
  '((file dir 0 0 91 70 nil)
    (shell dir 91 0 91 70 nil)
    (shell dir 182 0 92 70 nil))
  "Layout specification template for dynamic project commands.
The symbol `dir' is replaced with the actual project directory path.
Format: (type path x y width height host)")

(defvar etm-dynamic--generated-commands nil
  "List of dynamically generated command symbols for cleanup.")

;; Helper Functions
;; ----------------------------------------

(defun --etm-dynamic-expand-layout-spec (dir)
  "Expand `etm-dynamic-layout-spec' with DIR replacing `dir' symbol."
  (mapcar (lambda (spec)
            (mapcar (lambda (elem)
                      (if (eq elem 'dir) dir elem))
                    spec))
          etm-dynamic-layout-spec))

(defun --etm-dynamic-is-project-dir-p (path)
  "Return non-nil if PATH looks like a valid project directory."
  (and (file-directory-p path)
       (not (string-prefix-p "." (file-name-nondirectory path)))))

(defun --etm-dynamic-scan-directory (base-dir)
  "Scan BASE-DIR and return list of project directory names."
  (let ((expanded (expand-file-name base-dir)))
    (when (file-directory-p expanded)
      (cl-remove-if-not
       (lambda (name)
         (--etm-dynamic-is-project-dir-p
          (expand-file-name name expanded)))
       (directory-files expanded nil "^[^.]")))))

(defun --etm-dynamic-create-command (prefix project-name project-dir)
  "Create a command for PROJECT-NAME in PROJECT-DIR with command PREFIX.
Creates function named PREFIX-PROJECT-NAME."
  (let* ((cmd-name (format "%s-%s" prefix project-name))
         (cmd-sym (intern cmd-name))
         (tab-name cmd-name)
         (layout-spec (--etm-dynamic-expand-layout-spec project-dir)))
    ;; Define the interactive command
    (fset cmd-sym
          (lambda ()
            (interactive)
            (--etm-layout-create-from-positions tab-name layout-spec
						nil)))
    ;; Add documentation
    (put cmd-sym 'function-documentation
         (format "Open %s project with 3-column layout (dired | vterm | vterm).
Project directory: %s"
		 project-name project-dir))
    ;; Track for potential cleanup
    (push cmd-sym etm-dynamic--generated-commands)
    cmd-sym))

;; Main Functions
;; ----------------------------------------

(defun etm-dynamic-generate-commands ()
  "Generate commands for all configured project directories.
Scans directories in `etm-dynamic-project-dirs' and creates
interactive commands for each subdirectory found."
  (interactive)
  (let ((count 0))
    (dolist (entry etm-dynamic-project-dirs)
      (let* ((prefix (car entry))
             (base-dir (cdr entry))
             (projects (--etm-dynamic-scan-directory base-dir)))
        (dolist (project projects)
          (let ((project-dir (expand-file-name project base-dir)))
            (--etm-dynamic-create-command prefix project project-dir)
            (cl-incf count)))))
    (message "ETM: Generated %d dynamic project commands" count)
    count))

(defun etm-dynamic-clear-commands ()
  "Remove all dynamically generated commands."
  (interactive)
  (dolist (sym etm-dynamic--generated-commands)
    (when (fboundp sym)
      (fmakunbound sym)))
  (setq etm-dynamic--generated-commands nil)
  (message "ETM: Cleared all dynamic commands"))

(defun etm-dynamic-regenerate-commands ()
  "Clear and regenerate all dynamic project commands."
  (interactive)
  (etm-dynamic-clear-commands)
  (etm-dynamic-generate-commands))

(defun etm-dynamic-list-commands ()
  "List all dynamically generated commands."
  (interactive)
  (if etm-dynamic--generated-commands
      (with-output-to-temp-buffer "*ETM Dynamic Commands*"
        (princ "Dynamically generated ETM commands:\n\n")
        (dolist (sym (sort
		      (copy-sequence etm-dynamic--generated-commands)
                      (lambda (a b) (string< (symbol-name a)
                                             (symbol-name b)))))
          (princ (format "  M-x %s\n" (symbol-name sym)))))
    (message
     "No dynamic commands generated. Run M-x etm-dynamic-generate-commands")))

;; Add/Remove Project Directories
;; ----------------------------------------

(defun etm-dynamic-add-project-dir (prefix directory)
  "Add DIRECTORY with PREFIX to project directories and regenerate commands.
Example: (etm-dynamic-add-project-dir \"work\" \"~/work\")"
  (interactive "sPrefix (e.g., work): \nDDirectory: ")
  (let ((expanded-dir (expand-file-name directory)))
    (unless (file-directory-p expanded-dir)
      (error "Directory does not exist: %s" expanded-dir))
    (add-to-list 'etm-dynamic-project-dirs (cons prefix expanded-dir))
    (etm-dynamic-regenerate-commands)))

;; Auto-initialization
;; ----------------------------------------

;; Generate commands when this module is loaded
(etm-dynamic-generate-commands)

(provide 'etm-layout-dynamic)

(when (not load-file-name)
  (message "etm-layout-dynamic.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

;;; etm-layout-dynamic.el ends here
