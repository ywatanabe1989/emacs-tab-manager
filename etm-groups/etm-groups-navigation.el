;;; etm-groups-navigation.el --- Navigation between buffers in groups -*- coding: utf-8; lexical-binding: t -*-
;; Author: ywatanabe
;; Time-stamp: <2025-01-25 15:50:00 (ywatanabe)>
;;; Commentary:
;; Functions for navigating between buffers within groups
;;; Code:

(require 'cl-lib)
(require 'etm-groups-core)
(require 'etm-buffer-navigation)

;; Helper functions
(defun etm-groups--get-live-buffers (buffer-names)
  "Filter BUFFER-NAMES to only include live buffers."
  (cl-remove-if-not (lambda (name) (get-buffer name)) buffer-names))

(defun etm-groups--get-next-buffer-cyclic (current-buffer buffer-list)
  "Get next buffer after CURRENT-BUFFER in BUFFER-LIST, cycling at end."
  (let* ((live-buffers (etm-groups--get-live-buffers buffer-list))
         (pos (cl-position current-buffer live-buffers :test #'string=)))
    (when live-buffers
      (if (and pos (< (1+ pos) (length live-buffers)))
          (nth (1+ pos) live-buffers)
        (car live-buffers)))))

(defun etm-groups--get-previous-buffer-cyclic (current-buffer buffer-list)
  "Get previous buffer before CURRENT-BUFFER in BUFFER-LIST, cycling at start."
  (let* ((live-buffers (etm-groups--get-live-buffers buffer-list))
         (pos (cl-position current-buffer live-buffers :test #'string=)))
    (when live-buffers
      (if (and pos (> pos 0))
          (nth (1- pos) live-buffers)
        (car (last live-buffers))))))

;; Navigation functions
(defun etm-groups-next-buffer (group-name)
  "Switch to next buffer in GROUP-NAME."
  (let* ((buffers (etm-groups-get-buffers group-name))
         (current (buffer-name))
         (next (etm-groups--get-next-buffer-cyclic current buffers)))
    (if next
        (switch-to-buffer next)
      (message "No buffers in group '%s'" group-name))))

(defun etm-groups-previous-buffer (group-name)
  "Switch to previous buffer in GROUP-NAME."
  (let* ((buffers (etm-groups-get-buffers group-name))
         (current (buffer-name))
         (previous (etm-groups--get-previous-buffer-cyclic current buffers)))
    (if previous
        (switch-to-buffer previous)
      (message "No buffers in group '%s'" group-name))))

(defun etm-groups-switch-to-group (group-name)
  "Switch to first buffer in GROUP-NAME."
  (let* ((buffers (etm-groups-get-buffers group-name))
         (live-buffers (etm-groups--get-live-buffers buffers)))
    (if live-buffers
        (switch-to-buffer (car live-buffers))
      (message "No live buffers in group '%s'" group-name))))

;; Group cycling
(defvar etm-groups--current-group nil
  "Currently active group for cycling.")

(defun etm-groups--get-current-group ()
  "Get the current group based on current buffer."
  (let* ((buffer-name (buffer-name))
         (groups (etm-groups-find-buffer-groups buffer-name)))
    (or (and etm-groups--current-group
             (member etm-groups--current-group groups)
             etm-groups--current-group)
        (car groups))))

(defun etm-groups--get-next-group ()
  "Get the next group in the cycle."
  (let* ((all-groups (etm-groups-list-all))
         (current (etm-groups--get-current-group))
         (pos (cl-position current all-groups :test #'string=)))
    (when all-groups
      (if (and pos (< (1+ pos) (length all-groups)))
          (nth (1+ pos) all-groups)
        (car all-groups)))))

(defun etm-groups-cycle-next ()
  "Cycle to the next group and switch to its first buffer."
  (let ((next-group (etm-groups--get-next-group)))
    (when next-group
      (setq etm-groups--current-group next-group)
      (etm-groups-switch-to-group next-group))))

(defun etm-groups-cycle-previous ()
  "Cycle to the previous group and switch to its first buffer."
  (let* ((all-groups (etm-groups-list-all))
         (current (etm-groups--get-current-group))
         (pos (cl-position current all-groups :test #'string=))
         (prev-group (when all-groups
                       (if (and pos (> pos 0))
                           (nth (1- pos) all-groups)
                         (car (last all-groups))))))
    (when prev-group
      (setq etm-groups--current-group prev-group)
      (etm-groups-switch-to-group prev-group))))

;; Interactive commands
(defun etm-groups-next-buffer-interactive ()
  "Interactively navigate to next buffer in a group."
  (interactive)
  (let* ((current-groups (etm-groups-find-buffer-groups (buffer-name)))
         (group (if (= 1 (length current-groups))
                    (car current-groups)
                  (completing-read "Group: " current-groups nil t))))
    (if group
        (etm-groups-next-buffer group)
      (message "Current buffer is not in any group"))))

(defun etm-groups-previous-buffer-interactive ()
  "Interactively navigate to previous buffer in a group."
  (interactive)
  (let* ((current-groups (etm-groups-find-buffer-groups (buffer-name)))
         (group (if (= 1 (length current-groups))
                    (car current-groups)
                  (completing-read "Group: " current-groups nil t))))
    (if group
        (etm-groups-previous-buffer group)
      (message "Current buffer is not in any group"))))

(defun etm-groups-switch-interactive ()
  "Interactively switch to a group."
  (interactive)
  (let* ((groups (etm-groups-list-all))
         (group (completing-read "Switch to group: " groups nil t)))
    (etm-groups-switch-to-group group)))

(provide 'etm-groups-navigation)
;;; etm-groups-navigation.el ends here