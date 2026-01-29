;;; etm-groups.el --- Buffer grouping system for ETM -*- coding: utf-8; lexical-binding: t -*-
;; Author: ywatanabe
;; Time-stamp: <2025-01-25 15:54:00 (ywatanabe)>
;;; Commentary:
;; Main entry point for ETM buffer grouping functionality
;;; Code:

(require 'etm-groups-core)
(require 'etm-groups-navigation)
(require 'etm-groups-indicators)

;; Keybinding support
(defvar etm-groups-command-map
  (let ((map (make-sparse-keymap)))
    ;; Group management
    (define-key map (kbd "c") #'etm-groups-create-interactive)
    (define-key map (kbd "d") #'etm-groups-delete-interactive)
    (define-key map (kbd "a") #'etm-groups-add-current-buffer)
    (define-key map (kbd "r") #'etm-groups-remove-current-buffer)
    
    ;; Navigation
    (define-key map (kbd "n") #'etm-groups-next-buffer-interactive)
    (define-key map (kbd "p") #'etm-groups-previous-buffer-interactive)
    (define-key map (kbd "g") #'etm-groups-switch-interactive)
    (define-key map (kbd "N") #'etm-groups-cycle-next)
    (define-key map (kbd "P") #'etm-groups-cycle-previous)
    
    ;; Information
    (define-key map (kbd "l") #'etm-groups-list-interactive)
    (define-key map (kbd "?") #'etm-groups-show-buffer-groups)
    
    ;; Indicators
    (define-key map (kbd "i") #'etm-groups-toggle-indicators)
    map)
  "Keymap for ETM group commands.")

;; Additional interactive commands
(defun etm-groups-delete-interactive ()
  "Interactively delete a buffer group."
  (interactive)
  (let* ((groups (etm-groups-list-all))
         (group (completing-read "Delete group: " groups nil t)))
    (when (yes-or-no-p (format "Delete group '%s'? " group))
      (etm-groups-delete group))))

(defun etm-groups-list-interactive ()
  "List all groups and their buffers."
  (interactive)
  (let ((groups (etm-groups-list-all)))
    (if groups
        (with-output-to-temp-buffer "*ETM Groups*"
          (princ "ETM Buffer Groups\n")
          (princ "=================\n\n")
          (dolist (group groups)
            (let ((buffers (etm-groups-get-buffers group)))
              (princ (format "Group: %s\n" group))
              (if buffers
                  (dolist (buffer buffers)
                    (princ (format "  - %s%s\n" buffer
                                   (if (get-buffer buffer) "" " [killed]"))))
                (princ "  (empty)\n"))
              (princ "\n"))))
      (message "No groups defined"))))

(defun etm-groups-toggle-indicators ()
  "Toggle group indicators on/off."
  (interactive)
  (if etm-groups-indicators-enabled
      (etm-groups-disable-indicators)
    (etm-groups-enable-indicators)))

;; Cleanup functions
(defun etm-groups-cleanup-killed-buffers ()
  "Remove killed buffers from all groups in current tab."
  (interactive)
  (let ((groups (etm-groups--get-tab-groups))
        (removed-count 0))
    (maphash (lambda (group-name buffers)
               (let ((live-buffers (etm-groups--get-live-buffers buffers)))
                 (when (< (length live-buffers) (length buffers))
                   (setq removed-count (+ removed-count 
                                          (- (length buffers) 
                                             (length live-buffers))))
                   (puthash group-name live-buffers groups))))
             groups)
    (message "Removed %d killed buffer(s) from groups" removed-count)))

;; Auto-cleanup on buffer kill
(defun etm-groups--buffer-kill-hook ()
  "Clean up buffer from groups when killed."
  (let ((buffer-name (buffer-name)))
    (dolist (group (etm-groups-find-buffer-groups buffer-name))
      (etm-groups-remove-buffer group buffer-name))))

(add-hook 'kill-buffer-hook #'etm-groups--buffer-kill-hook)

;; Integration with ETM
(defun etm-groups-init ()
  "Initialize ETM groups system."
  (interactive)
  ;; Enable indicators by default
  (when etm-groups-indicators-enabled
    (etm-groups-enable-indicators))
  (message "ETM groups initialized"))

(provide 'etm-groups)
;;; etm-groups.el ends here