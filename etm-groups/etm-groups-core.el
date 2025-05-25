;;; etm-groups-core.el --- Core functionality for ETM buffer groups -*- coding: utf-8; lexical-binding: t -*-
;; Author: ywatanabe
;; Time-stamp: <2025-01-25 15:48:00 (ywatanabe)>
;;; Commentary:
;; Core functionality for managing buffer groups within tabs
;;; Code:

(require 'etm-core-variables)
(require 'etm-core-helpers)
(require 'etm-core-tab-id)

;; Variables
(defvar etm-tab-buffer-groups (make-hash-table :test 'equal)
  "Hash table mapping tab IDs to their buffer groups.
Structure: tab-id -> (group-name -> (buffer-list))")

(defcustom etm-groups-default-names
  '("project" "documentation" "config" "tests" "temp")
  "Default group names available for quick selection."
  :type '(repeat string)
  :group 'etm)

;; Helper functions
(defun etm-groups--get-current-tab-id ()
  "Get a unique identifier for the current tab.
Uses tab name as the identifier."
  (if (and (boundp 'tab-bar-mode) tab-bar-mode)
      (let ((current-tab (cond
                          ((fboundp 'tab-bar-current-tab)
                           (tab-bar-current-tab))
                          ((fboundp 'tab-bar--current-tab)
                           (tab-bar--current-tab))
                          (t nil))))
        (if current-tab
            (or (alist-get 'name current-tab)
                (format "tab-%d" (or (etm-tab-get-current-tab-index) 0)))
          "default-tab"))
    "default-tab"))

(defun etm-groups--get-tab-groups ()
  "Get groups hash table for current tab."
  (let ((tab-id (etm-groups--get-current-tab-id)))
    (unless (gethash tab-id etm-tab-buffer-groups)
      (puthash tab-id (make-hash-table :test 'equal) etm-tab-buffer-groups))
    (gethash tab-id etm-tab-buffer-groups)))

(defun etm-groups--validate-group-name (name)
  "Validate that NAME is a valid group name."
  (unless (and (stringp name)
               (> (length name) 0)
               (not (string-match-p "[\n\t]" name)))
    (error "Invalid group name: %s" name)))

;; Core functions
(defun etm-groups-create (name)
  "Create a new buffer group with NAME in current tab."
  (etm-groups--validate-group-name name)
  (let ((groups (etm-groups--get-tab-groups)))
    (when (etm-groups-exists-p name)
      (error "Group '%s' already exists" name))
    (puthash name nil groups)
    (message "Created group: %s" name)))

(defun etm-groups-exists-p (name)
  "Check if group NAME exists in current tab."
  (let ((groups (etm-groups--get-tab-groups)))
    (not (eq (gethash name groups 'not-found) 'not-found))))

(defun etm-groups-delete (name)
  "Delete buffer group NAME from current tab."
  (let ((groups (etm-groups--get-tab-groups)))
    (unless (etm-groups-exists-p name)
      (error "Group '%s' does not exist" name))
    (remhash name groups)
    (message "Deleted group: %s" name)))

(defun etm-groups-add-buffer (group-name buffer-name)
  "Add BUFFER-NAME to GROUP-NAME in current tab."
  (let ((groups (etm-groups--get-tab-groups)))
    (unless (etm-groups-exists-p group-name)
      (error "Group '%s' does not exist" group-name))
    (let ((buffers (gethash group-name groups)))
      (unless (member buffer-name buffers)
        (puthash group-name (append buffers (list buffer-name)) groups))
      (message "Added '%s' to group '%s'" buffer-name group-name))))

(defun etm-groups-remove-buffer (group-name buffer-name)
  "Remove BUFFER-NAME from GROUP-NAME in current tab."
  (let ((groups (etm-groups--get-tab-groups)))
    (unless (etm-groups-exists-p group-name)
      (error "Group '%s' does not exist" group-name))
    (let ((buffers (gethash group-name groups)))
      (puthash group-name (delete buffer-name buffers) groups)
      (message "Removed '%s' from group '%s'" buffer-name group-name))))

(defun etm-groups-get-buffers (group-name)
  "Get list of buffers in GROUP-NAME for current tab."
  (let ((groups (etm-groups--get-tab-groups)))
    (gethash group-name groups)))

(defun etm-groups-list-all ()
  "List all group names in current tab."
  (let ((groups (etm-groups--get-tab-groups))
        (names nil))
    (maphash (lambda (name _buffers)
               (push name names))
             groups)
    (sort names #'string<)))

(defun etm-groups-find-buffer-groups (buffer-name)
  "Find all groups that contain BUFFER-NAME in current tab."
  (let ((groups (etm-groups--get-tab-groups))
        (containing-groups nil))
    (maphash (lambda (group-name buffers)
               (when (member buffer-name buffers)
                 (push group-name containing-groups)))
             groups)
    (sort containing-groups #'string<)))

;; Interactive commands
(defun etm-groups-create-interactive ()
  "Interactively create a new buffer group."
  (interactive)
  (let* ((existing (etm-groups-list-all))
         (suggestions (cl-remove-if (lambda (name) (member name existing))
                                    etm-groups-default-names))
         (name (completing-read "Group name: " suggestions nil nil)))
    (etm-groups-create name)))

(defun etm-groups-add-current-buffer ()
  "Add current buffer to a group."
  (interactive)
  (let* ((groups (etm-groups-list-all))
         (group (if groups
                    (completing-read "Add to group: " groups nil t)
                  (error "No groups exist. Create one first")))
         (buffer-name (buffer-name)))
    (etm-groups-add-buffer group buffer-name)))

(defun etm-groups-remove-current-buffer ()
  "Remove current buffer from a group."
  (interactive)
  (let* ((buffer-name (buffer-name))
         (groups (etm-groups-find-buffer-groups buffer-name)))
    (if groups
        (let ((group (completing-read "Remove from group: " groups nil t)))
          (etm-groups-remove-buffer group buffer-name))
      (message "Buffer '%s' is not in any group" buffer-name))))

(provide 'etm-groups-core)
;;; etm-groups-core.el ends here