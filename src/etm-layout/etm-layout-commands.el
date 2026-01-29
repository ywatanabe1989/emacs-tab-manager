;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-30 16:32:53>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/etm-layout-commands.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Interactive commands for ETM layout management
;; Provides user-facing commands for layout preview and management

(require 'etm-layout-preview)

;; Marking System (dired-like)
;; ----------------------------------------

(defvar-local etm-layout-marked-layouts nil
  "List of marked layout names in the current buffer.")

(defun etm-layout-mark ()
  "Mark layout at point for later action."
  (interactive)
  (let ((layout-info (etm-layout-get-at-point)))
    (when layout-info
      (let ((name (plist-get layout-info :name)))
        (unless (member name etm-layout-marked-layouts)
          (push name etm-layout-marked-layouts))
        (etm-layout--refresh-marks)
        (forward-line 1)))))

(defun etm-layout-unmark ()
  "Unmark layout at point."
  (interactive)
  (let ((layout-info (etm-layout-get-at-point)))
    (when layout-info
      (let ((name (plist-get layout-info :name)))
        (setq etm-layout-marked-layouts
              (delete name etm-layout-marked-layouts))
        (etm-layout--refresh-marks)
        (forward-line 1)))))

(defun etm-layout-unmark-all ()
  "Unmark all layouts."
  (interactive)
  (setq etm-layout-marked-layouts nil)
  (etm-layout--refresh-marks))

(defun etm-layout-toggle-mark ()
  "Toggle mark on layout at point."
  (interactive)
  (let ((layout-info (etm-layout-get-at-point)))
    (when layout-info
      (let ((name (plist-get layout-info :name)))
        (if (member name etm-layout-marked-layouts)
            (setq etm-layout-marked-layouts
                  (delete name etm-layout-marked-layouts))
          (push name etm-layout-marked-layouts))
        (etm-layout--refresh-marks)
        (forward-line 1)))))

(defun etm-layout-execute ()
  "Execute deletion on marked layouts."
  (interactive)
  (if (null etm-layout-marked-layouts)
      (message "No layouts marked")
    (when (yes-or-no-p
           (format "Delete %d marked layout(s)? "
                   (length etm-layout-marked-layouts)))
      (dolist (name etm-layout-marked-layouts)
        (etm-layout--delete-internal name))
      (setq etm-layout-marked-layouts nil)
      (etm-layout-list))))

(defun etm-layout--delete-internal (layout-name)
  "Delete LAYOUT-NAME without confirmation (internal use)."
  (let* ((layout-info (cl-find layout-name
                               (etm-layout-scan-directory)
                               :key (lambda (l) (plist-get l :name))
                               :test #'string=))
         (file-path
	  (when layout-info (plist-get layout-info :file-path))))
    (when file-path
      (delete-file file-path)
      (let ((func-sym (intern (format "etm-open-%s" layout-name)))
            (alias-sym (intern layout-name)))
        (when (fboundp func-sym)
          (fmakunbound func-sym)
          (unintern func-sym nil))
        (when (fboundp alias-sym)
          (fmakunbound alias-sym)
          (unintern alias-sym nil)))
      (message "Deleted: %s" layout-name))))

(defun etm-layout--refresh-marks ()
  "Refresh display to show current marks."
  (let ((pos (point))
        (layouts (etm-layout-scan-directory)))
    (setq tabulated-list-entries
          (mapcar (lambda (layout)
                    (list layout
                          (etm-layout--format-entry-with-mark layout)))
                  layouts))
    (tabulated-list-print t)
    (goto-char pos)))

(defun etm-layout--format-entry-with-mark (layout-info)
  "Format LAYOUT-INFO with mark indicator."
  (let* ((name (plist-get layout-info :name))
         (marked (member name etm-layout-marked-layouts))
         (mark-char (if marked "D" " ")))
    (vector mark-char
            name
            (number-to-string (plist-get layout-info :window-count))
            (let ((dims (plist-get layout-info :dimensions)))
              (format "%dx%d" (car dims) (cdr dims)))
            (or (plist-get layout-info :host) "localhost"))))

;; Interactive Commands
;; ----------------------------------------

(defun etm-layout-list ()
  "Display a list of all saved ETM layouts with preview capability.
Supports dired-like marking: m=mark, u=unmark, U=unmark-all, x=execute."
  (interactive)
  (let ((layouts (etm-layout-scan-directory))
        (buffer (get-buffer-create "*ETM Layouts*")))
    (with-current-buffer buffer
      (etm-layout-list-mode)
      (setq etm-layout-marked-layouts nil)
      (setq tabulated-list-entries
            (mapcar (lambda (layout)
                      (list layout
                            (etm-layout--format-entry-with-mark layout)))
                    layouts))
      (tabulated-list-print)
      ;; Navigation & actions
      (local-set-key (kbd "RET") #'etm-layout-load-at-point)
      (local-set-key (kbd "SPC") #'etm-layout-preview-at-point)
      (local-set-key (kbd "p") #'etm-layout-preview-at-point)
      ;; Marking (dired-like)
      (local-set-key (kbd "m") #'etm-layout-mark)
      (local-set-key (kbd "u") #'etm-layout-unmark)
      (local-set-key (kbd "U") #'etm-layout-unmark-all)
      (local-set-key (kbd "t") #'etm-layout-toggle-mark)
      (local-set-key (kbd "x") #'etm-layout-execute)
      ;; Single delete
      (local-set-key (kbd "d") #'etm-layout-mark)  ; mark for deletion
      (local-set-key (kbd "D") #'etm-layout-delete-at-point)  ; immediate delete
      (local-set-key (kbd "q") #'quit-window)
      (local-set-key (kbd "g") #'etm-layout-list))
                                        ; refresh
    (switch-to-buffer buffer)
    (message
     "m=mark u=unmark x=execute RET=load p=preview D=delete q=quit")))

(defun etm-layout-load-at-point ()
  "Load the layout at point in the layout list."
  (interactive)
  (let ((layout-info (etm-layout-get-at-point)))
    (when layout-info
      (let ((layout-name (plist-get layout-info :name)))
        (if (fboundp (intern (format "etm-open-%s" layout-name)))
            (progn
              (call-interactively
	       (intern (format "etm-open-%s" layout-name)))
              (quit-window))
          (error "Layout function not found: etm-open-%s" layout-name))))))

(defun etm-layout-preview (layout-name)
  "Preview a specific LAYOUT-NAME without loading it."
  (interactive
   (list (completing-read "Layout to preview: "
                          (mapcar (lambda (l) (plist-get l :name))
                                  (etm-layout-scan-directory))
                          nil t)))
  (let ((layout-info (cl-find layout-name
                              (etm-layout-scan-directory)
                              :key (lambda (l) (plist-get l :name))
                              :test #'string=)))
    (if layout-info
        (etm-layout-show-preview layout-info)
      (error "Layout not found: %s" layout-name))))

(defun etm-layout-preview-quit ()
  "Close the layout preview window."
  (interactive)
  (when-let ((window (get-buffer-window etm-layout-preview-buffer)))
    (delete-window window))
  (when-let ((buffer (get-buffer etm-layout-preview-buffer)))
    (kill-buffer buffer)))

(defun etm-layout-delete-at-point ()
  "Delete the layout at point in the layout list."
  (interactive)
  (let ((layout-info (etm-layout-get-at-point)))
    (when layout-info
      (let ((layout-name (plist-get layout-info :name)))
        (etm-layout-delete layout-name)
        ;; Refresh the list
        (etm-layout-list)))))

(defun etm-layout-open-layouts-dir ()
  "Open the ETM layouts directory in dired."
  (interactive)
  (if (file-directory-p etm-layout-save-dir)
      (dired etm-layout-save-dir)
    (error "Layouts directory does not exist: %s" etm-layout-save-dir)))

;; Dynamic Lisp Project Layouts
;; ----------------------------------------

(defvar etm-lisp-project-dirs
  '("~/.emacs.d/lisp" "~/.dotfiles/.emacs.d/lisp")
  "Directories to scan for lisp projects.")

(defvar etm-lisp--project-cache nil
  "Cache of (name . path) alist for lisp projects.")

(defun etm-lisp (project-name)
  "Open a 1x3 layout for lisp PROJECT-NAME (dired, main file, shell).
Tab uses 'lisp-' prefix to avoid conflicts with package names.
Usage: M-x etm-lisp RET <project> RET"
  (interactive
   (list (completing-read "Lisp project: "
                          (mapcar #'car (etm-lisp--scan-dirs))
                          nil t)))
  (let* ((project-alist (etm-lisp--scan-dirs))
         (dir (cdr (assoc project-name project-alist)))
         (tab-name (format "lisp-%s" project-name))
         (main-file (etm-lisp--find-main-file dir project-name)))
    (unless dir
      (error "Project not found: %s" project-name))
    ;; Create layout with 1x3: dired | main-file | shell
    (--etm-layout-create-from-positions
     tab-name
     `((file ,dir 0 0 60 80 nil)
       (file ,(or main-file dir) 60 0 60 80 nil)
       (shell ,dir 120 0 60 80 nil))
     nil)))

(defun etm-lisp--scan-dirs ()
  "Scan `etm-lisp-project-dirs' for lisp projects.
Returns alist of (name . canonical-path), deduplicated via symlink resolution."
  (let ((projects (make-hash-table :test 'equal)))
    (dolist (base-dir etm-lisp-project-dirs)
      (let ((expanded (expand-file-name base-dir)))
        (when (file-directory-p expanded)
          (dolist (entry (directory-files expanded t "^[^.]"))
            (when (and (file-directory-p entry)
                       (or (directory-files entry nil "\\.el$" t)
                           (file-exists-p
                            (expand-file-name "Makefile" entry))))
              ;; Use canonical path to deduplicate symlinks
              (let* ((canonical (file-truename entry))
                     (name
		      (file-name-nondirectory
		       (directory-file-name entry))))
                (unless (gethash name projects)
                  (puthash name canonical projects))))))))
    ;; Convert hash to sorted alist
    (let ((result '()))
      (maphash (lambda (k v) (push (cons k v) result)) projects)
      (sort result (lambda (a b) (string< (car a) (car b)))))))

(defun etm-lisp--find-main-file (dir name)
  "Find main elisp file in DIR for project NAME."
  (let ((candidates (list (format "%s.el" name)
                          (format "%s-mode.el" name)
                          "init.el"
                          "main.el")))
    (cl-loop for file in candidates
             for path = (expand-file-name file dir)
             when (file-exists-p path)
             return path)))

(defun etm-layout-delete (layout-name)
  "Delete saved layout LAYOUT-NAME.
Removes the layout file and uninterns the associated functions."
  (interactive
   (list (completing-read "Delete layout: "
                          (mapcar (lambda (l) (plist-get l :name))
                                  (etm-layout-scan-directory))
                          nil t)))
  (let* ((layout-info (cl-find layout-name
                               (etm-layout-scan-directory)
                               :key (lambda (l) (plist-get l :name))
                               :test #'string=))
         (file-path
	  (when layout-info (plist-get layout-info :file-path))))
    (unless file-path
      (error "Layout not found: %s" layout-name))
    (when (yes-or-no-p (format "Delete layout '%s'? " layout-name))
      ;; Delete the file
      (delete-file file-path)
      ;; Unintern the function symbols
      (let ((func-sym (intern (format "etm-open-%s" layout-name)))
            (alias-sym (intern layout-name)))
        (when (fboundp func-sym)
          (fmakunbound func-sym)
          (unintern func-sym nil))
        (when (fboundp alias-sym)
          (fmakunbound alias-sym)
          (unintern alias-sym nil)))
      (message "Layout '%s' deleted" layout-name))))

;; Key Bindings
;; ----------------------------------------

(defun etm-layout-setup-preview-keys ()
  "Set up keybindings for layout preview in ETM command map."
  (define-key etm-command-map (kbd "p") #'etm-layout-preview)
  (define-key etm-command-map (kbd "P") #'etm-layout-list)
  (define-key etm-command-map (kbd "l") #'etm-layout-list)
  (define-key etm-command-map (kbd "L") #'etm-layout-open-layouts-dir))

;; Auto-setup keys when loaded with ETM
(with-eval-after-load 'etm-keys-command-map
  (etm-layout-setup-preview-keys))

(provide 'etm-layout-commands)

(when
    (not load-file-name)
  (message "etm-layout-commands.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
