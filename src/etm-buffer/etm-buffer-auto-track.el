;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-25 21:15:05>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-buffer/etm-buffer-auto-track.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)


;;; Commentary:
;; Automatic buffer tracking for ETM tabs.
;; Tracks buffers created while a tab is active and allows killing
;; all tracked buffers when the tab is closed.
;; Supports vterm, dired, shell, eshell, term, and compilation buffers.

(require 'etm-core-variables)

;; Buffer-tab association storage
;; ----------------------------------------

(defvar etm-buffer-tab-association (make-hash-table :test 'equal)
  "Hash table mapping buffer names to their originating tab names.
This provides robust tracking independent of mode hooks.")

;; Core tracking functions
;; ----------------------------------------

(defun --etm-track-buffer-excluded-p (buffer-name)
  "Return non-nil if BUFFER-NAME matches any exclusion pattern."
  (cl-some (lambda (pattern)
             (string-match-p pattern buffer-name))
           etm-track-exclude-patterns))

(defun --etm-get-current-tab-name ()
  "Get the name of the current tab."
  (alist-get 'name (tab-bar--current-tab)))

(defun etm-track-buffer (&optional buffer tab-name)
  "Track BUFFER under TAB-NAME.
If BUFFER is nil, use current buffer.
If TAB-NAME is nil, use current tab."
  (let* ((buf (or buffer (current-buffer)))
         (buf-name (if (bufferp buf) (buffer-name buf) buf))
         (tab (or tab-name (--etm-get-current-tab-name))))
    (etm-message "track-buffer: Attempting to track '%s' in tab '%s'"
		         buf-name tab)
    (if (--etm-track-buffer-excluded-p buf-name)
        (etm-message "track-buffer: '%s' excluded by pattern" buf-name)
      (when buf-name
        (let ((tracked (gethash tab etm-tab-tracked-buffers)))
          (if (member buf-name tracked)
              (etm-message
	           "track-buffer: '%s' already tracked in '%s'" buf-name
	           tab)
            (puthash tab (cons buf-name tracked)
		             etm-tab-tracked-buffers)
            ;; Also store reverse mapping for robust lookup
            (puthash buf-name tab etm-buffer-tab-association)
            (etm-message
	         "track-buffer: SUCCESS - '%s' now tracked in tab '%s'"
	         buf-name tab)))))))

(defun etm-untrack-buffer (&optional buffer tab-name)
  "Stop tracking BUFFER under TAB-NAME."
  (let* ((buf (or buffer (current-buffer)))
         (buf-name (if (bufferp buf) (buffer-name buf) buf))
         (tab (or tab-name (--etm-get-current-tab-name))))
    (when buf-name
      (let ((tracked (gethash tab etm-tab-tracked-buffers)))
        (puthash tab (delete buf-name tracked) etm-tab-tracked-buffers)))))

(defun etm-get-tracked-buffers (&optional tab-name)
  "Get list of tracked buffer names for TAB-NAME."
  (let ((tab (or tab-name (--etm-get-current-tab-name))))
    (gethash tab etm-tab-tracked-buffers)))

(defun etm-clear-tracked-buffers (&optional tab-name)
  "Clear tracked buffers list for TAB-NAME without killing them."
  (let ((tab (or tab-name (--etm-get-current-tab-name))))
    (remhash tab etm-tab-tracked-buffers)))

;; Kill tracked buffers
;; ----------------------------------------

(defun etm-kill-tracked-buffers (&optional tab-name)
  "Kill all tracked buffers for TAB-NAME.
Returns the count of killed buffers."
  (interactive)
  (let* ((tab (or tab-name (--etm-get-current-tab-name)))
         (tracked (etm-get-tracked-buffers tab))
         (killed-count 0))
    (etm-message "kill-tracked: Tab '%s' has %d tracked buffers" tab
		         (length tracked))
    (etm-message "kill-tracked: Buffers to kill: %s" tracked)
    (dolist (buf-name tracked)
      (let ((buf (get-buffer buf-name)))
        (if (not buf)
            (etm-message
	         "kill-tracked: '%s' - buffer not found (already dead)"
	         buf-name)
          (if (member buf-name etm-protected-buffers)
              (etm-message "kill-tracked: '%s' - protected, skipping"
			               buf-name)
            (etm-message "kill-tracked: Killing '%s'" buf-name)
            (kill-buffer buf)
            (setq killed-count (1+ killed-count))))))
    (etm-clear-tracked-buffers tab)
    (etm-message "kill-tracked: Killed %d buffers from tab '%s'"
		         killed-count tab)
    (when (called-interactively-p 'any)
      (message "Killed %d tracked buffers from tab '%s'" killed-count
	           tab))
    killed-count))

;; Auto-tracking hooks
;; ----------------------------------------

(defun --etm-auto-track-buffer-hook ()
  "Hook function to auto-track newly created buffers."
  (let ((buf-name (buffer-name))
        (major-mode-name (symbol-name major-mode)))
    (etm-message "auto-track-hook: Triggered for '%s' (mode: %s)"
		         buf-name major-mode-name)
    (if (not etm-auto-track-buffers)
        (etm-message
	     "auto-track-hook: Skipped - etm-auto-track-buffers is nil")
      (if (minibufferp)
          (etm-message "auto-track-hook: Skipped - minibuffer")
        (if (--etm-track-buffer-excluded-p buf-name)
            (etm-message
	         "auto-track-hook: Skipped - excluded pattern for '%s'"
	         buf-name)
          (etm-message
	       "auto-track-hook: Calling etm-track-buffer for '%s'"
	       buf-name)
          (etm-track-buffer))))))

(defun --etm-auto-track-on-buffer-kill ()
  "Remove buffer from tracking when killed."
  (let ((buf-name (buffer-name)))
    (maphash (lambda (tab tracked)
               (when (member buf-name tracked)
                 (puthash tab (delete buf-name tracked)
			              etm-tab-tracked-buffers)))
             etm-tab-tracked-buffers)
    ;; Also remove from buffer-tab-association
    (remhash buf-name etm-buffer-tab-association)))

(defvar --etm-buffer-name-before-rename nil
  "Stores buffer name before rename for tracking update.")

(defun --etm-track-before-buffer-rename (&rest _args)
  "Store buffer name before rename.
Accepts but ignores ARGS passed by rename-buffer."
  (setq-local --etm-buffer-name-before-rename (buffer-name)))

(defun --etm-track-after-buffer-rename (&rest _args)
  "Update tracking after buffer is renamed.
Accepts but ignores ARGS passed by rename-buffer."
  (when (and (bound-and-true-p --etm-buffer-name-before-rename)
             (not
	          (string= --etm-buffer-name-before-rename (buffer-name))))
    (let* ((old-name --etm-buffer-name-before-rename)
           (new-name (buffer-name))
           (tab-name (gethash old-name etm-buffer-tab-association)))
      ;; Update buffer-tab-association
      (when tab-name
        (remhash old-name etm-buffer-tab-association)
        (puthash new-name tab-name etm-buffer-tab-association)
        (etm-message "track-rename: Updated '%s' -> '%s' in tab '%s'"
                     old-name new-name tab-name))
      ;; Update tab-tracked-buffers
      (maphash (lambda (tab tracked)
                 (when (member old-name tracked)
                   (puthash tab
                            (cons new-name (delete old-name tracked))
                            etm-tab-tracked-buffers)))
               etm-tab-tracked-buffers))
    (setq-local --etm-buffer-name-before-rename (buffer-name))))

;; Interactive commands
;; ----------------------------------------

(defun etm-list-tracked-buffers ()
  "Display all tracked buffers for the current tab."
  (interactive)
  (let* ((tab-name (--etm-get-current-tab-name))
         (tracked (etm-get-tracked-buffers tab-name)))
    (if tracked
        (progn
          (message "Tracked buffers in tab '%s':" tab-name)
          (dolist (buf-name tracked)
            (message "  %s%s" buf-name
                     (if (get-buffer buf-name) "" " (dead)"))))
      (message "No tracked buffers in tab '%s'" tab-name))))

(defun etm-list-all-tracked-buffers ()
  "Display tracked buffers for all tabs."
  (interactive)
  (if (= (hash-table-count etm-tab-tracked-buffers) 0)
      (message "No buffers tracked in any tab")
    (with-help-window "*ETM Tracked Buffers*"
      (princ "ETM Tracked Buffers by Tab\n")
      (princ "==========================\n\n")
      (maphash (lambda (tab tracked)
                 (princ (format "Tab: %s\n" tab))
                 (if tracked
                     (dolist (buf-name tracked)
                       (princ (format "  - %s%s\n" buf-name
                                      (if (get-buffer buf-name) ""
					                    " (dead)"))))
                   (princ "  (no tracked buffers)\n"))
                 (princ "\n"))
               etm-tab-tracked-buffers))))

;; Setup and teardown
;; ----------------------------------------

(defun etm-auto-track-enable ()
  "Enable automatic buffer tracking."
  (interactive)
  (setq etm-auto-track-buffers t)
  (add-hook 'find-file-hook #'--etm-auto-track-buffer-hook)
  (add-hook 'shell-mode-hook #'--etm-auto-track-buffer-hook)
  (add-hook 'eshell-mode-hook #'--etm-auto-track-buffer-hook)
  (add-hook 'term-mode-hook #'--etm-auto-track-buffer-hook)
  (add-hook 'vterm-mode-hook #'--etm-auto-track-buffer-hook)
  (add-hook 'compilation-mode-hook #'--etm-auto-track-buffer-hook)
  (add-hook 'dired-mode-hook #'--etm-auto-track-buffer-hook)
  (add-hook 'kill-buffer-hook #'--etm-auto-track-on-buffer-kill)
  ;; Track buffer renames
  (advice-add 'rename-buffer :before
	          #'--etm-track-before-buffer-rename)
  (advice-add 'rename-buffer :after #'--etm-track-after-buffer-rename)
  (etm-message
   "auto-track-enable: Hooks added for vterm, dired, shell, etc.")
  (message "ETM auto-tracking enabled"))

(defun etm-auto-track-disable ()
  "Disable automatic buffer tracking."
  (interactive)
  (setq etm-auto-track-buffers nil)
  (remove-hook 'find-file-hook #'--etm-auto-track-buffer-hook)
  (remove-hook 'shell-mode-hook #'--etm-auto-track-buffer-hook)
  (remove-hook 'eshell-mode-hook #'--etm-auto-track-buffer-hook)
  (remove-hook 'term-mode-hook #'--etm-auto-track-buffer-hook)
  (remove-hook 'vterm-mode-hook #'--etm-auto-track-buffer-hook)
  (remove-hook 'compilation-mode-hook #'--etm-auto-track-buffer-hook)
  (remove-hook 'dired-mode-hook #'--etm-auto-track-buffer-hook)
  (remove-hook 'kill-buffer-hook #'--etm-auto-track-on-buffer-kill)
  ;; Remove rename tracking
  (advice-remove 'rename-buffer #'--etm-track-before-buffer-rename)
  (advice-remove 'rename-buffer #'--etm-track-after-buffer-rename)
  (etm-message "auto-track-disable: All hooks removed")
  (message "ETM auto-tracking disabled"))

(defun etm-auto-track-setup ()
  "Setup auto-tracking based on `etm-auto-track-buffers' setting."
  (etm-message "auto-track-setup: etm-auto-track-buffers=%s"
	           etm-auto-track-buffers)
  (if etm-auto-track-buffers
      (etm-auto-track-enable)
    (etm-auto-track-disable)))

;; Retroactive tracking for existing buffers
;; ----------------------------------------

(defun etm-track-existing-vterm-buffers ()
  "Track all existing vterm buffers in current tab.
Useful when vterm buffers were created before tracking was enabled."
  (interactive)
  (let ((tab-name (--etm-get-current-tab-name))
        (count 0))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (bound-and-true-p vterm-mode)
                   (not (--etm-track-buffer-excluded-p (buffer-name))))
          (etm-track-buffer buf tab-name)
          (setq count (1+ count)))))
    (etm-message
     "track-existing-vterm: Tracked %d vterm buffers in tab '%s'"
     count tab-name)
    (message "Tracked %d existing vterm buffers in tab '%s'" count
	         tab-name)))

(defun etm-track-existing-dired-buffers ()
  "Track all existing dired buffers in current tab."
  (interactive)
  (let ((tab-name (--etm-get-current-tab-name))
        (count 0))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (derived-mode-p 'dired-mode)
                   (not (--etm-track-buffer-excluded-p (buffer-name))))
          (etm-track-buffer buf tab-name)
          (setq count (1+ count)))))
    (etm-message
     "track-existing-dired: Tracked %d dired buffers in tab '%s'"
     count tab-name)
    (message "Tracked %d existing dired buffers in tab '%s'" count
	         tab-name)))

(defun etm-track-buffers-by-prefix (prefix)
  "Track all buffers whose names start with PREFIX in current tab."
  (interactive "sBuffer name prefix: ")
  (let ((tab-name (--etm-get-current-tab-name))
        (count 0))
    (dolist (buf (buffer-list))
      (let ((buf-name (buffer-name buf)))
        (when (and (string-prefix-p prefix buf-name)
                   (not (--etm-track-buffer-excluded-p buf-name)))
          (etm-track-buffer buf tab-name)
          (setq count (1+ count)))))
    (etm-message
     "track-by-prefix: Tracked %d buffers with prefix '%s' in tab '%s'"
     count prefix tab-name)
    (message "Tracked %d buffers with prefix '%s' in tab '%s'" count
	         prefix tab-name)))

(defun etm-track-all-terminal-buffers ()
  "Track all terminal-like buffers (vterm, shell, eshell, term) in current tab."
  (interactive)
  (let ((tab-name (--etm-get-current-tab-name))
        (count 0))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (or (bound-and-true-p vterm-mode)
                       (derived-mode-p 'shell-mode)
                       (derived-mode-p 'eshell-mode)
                       (derived-mode-p 'term-mode))
                   (not (--etm-track-buffer-excluded-p (buffer-name))))
          (etm-track-buffer buf tab-name)
          (setq count (1+ count)))))
    (etm-message
     "track-all-terminal: Tracked %d terminal buffers in tab '%s'"
     count tab-name)
    (message "Tracked %d terminal buffers in tab '%s'" count tab-name)))

(defun etm-track-window-buffers ()
  "Track all buffers visible in current tab's windows."
  (interactive)
  (let ((tab-name (--etm-get-current-tab-name))
        (count 0))
    (dolist (window (window-list))
      (let* ((buf (window-buffer window))
             (buf-name (buffer-name buf)))
        (unless (or (--etm-track-buffer-excluded-p buf-name)
                    (gethash buf-name etm-buffer-tab-association))
          (etm-track-buffer buf tab-name)
          (setq count (1+ count)))))
    (message "Tracked %d window buffers in tab '%s'" count tab-name)))

;; Debug helpers
;; ----------------------------------------

(defun etm-debug-tracking-status ()
  "Show current tracking status for debugging."
  (interactive)
  (let ((tab-name (--etm-get-current-tab-name)))
    (with-help-window "*ETM Tracking Debug*"
      (princ "ETM Tracking Debug Status\n")
      (princ "=========================\n\n")
      (princ (format "etm-debug: %s\n" etm-debug))
      (princ
       (format "etm-auto-track-buffers: %s\n" etm-auto-track-buffers))
      (princ
       (format "etm-close-kills-tracked-buffers: %s\n"
	           etm-close-kills-tracked-buffers))
      (princ (format "Current tab: %s\n\n" tab-name))
      (princ "Hook status:\n")
      (princ (format "  vterm-mode-hook has tracker: %s\n"
                     (if (and (boundp 'vterm-mode-hook)
                              (member '--etm-auto-track-buffer-hook
				                      vterm-mode-hook))
                         "YES"
		               "NO")))
      (princ (format "  dired-mode-hook has tracker: %s\n"
                     (member '--etm-auto-track-buffer-hook
			                 dired-mode-hook)))
      (princ (format "  shell-mode-hook has tracker: %s\n"
                     (member '--etm-auto-track-buffer-hook
			                 shell-mode-hook)))
      (princ "\nTracked buffers for current tab:\n")
      (let ((tracked (etm-get-tracked-buffers tab-name)))
        (if tracked
            (dolist (buf-name tracked)
              (princ (format "  - %s %s\n" buf-name
                             (if (get-buffer buf-name) "" "(dead)"))))
          (princ "  (none)\n")))
      (princ "\nAll vterm buffers in Emacs:\n")
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (bound-and-true-p vterm-mode)
            (let* ((buf-name (buffer-name))
                   (assoc-tab
		            (gethash buf-name etm-buffer-tab-association)))
              (princ (format "  - %s (associated tab: %s)\n"
                             buf-name (or assoc-tab "NONE"))))))))))

;;; etm-buffer-auto-track.el ends here


(provide 'etm-buffer-auto-track)

(when
    (not load-file-name)
  (message "etm-buffer-auto-track.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))