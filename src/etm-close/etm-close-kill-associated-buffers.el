;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-25>
;;; File: etm-close/etm-close-kill-associated-buffers.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Close tab and kill associated buffers using buffer OBJECTS for reliable ID

(require 'etm-core-variables)
(require 'etm-buffer-numeric)

(defun etm-close-kill-associated-buffers ()
  "Kill all buffers associated with the current tab.
Uses buffer OBJECTS for reliable identification.
Shared buffers (like dired) are preserved if used by other tabs."
  (interactive)
  (let* ((current-tab (tab-bar--current-tab))
         (tab-name (alist-get 'name current-tab))
         (tab-id (alist-get 'etm-id current-tab))
         ;; Use tab-id if available, fall back to tab-name
         (lookup-key (or tab-id tab-name))
         (numeric-buffers (--etm-numeric-get-tab-buffers lookup-key))
         (buffers-to-process '())
         (killed-count 0)
         (skipped-shared '()))

    ;; Debug: Show what we're working with
    (etm-debug-message 'close
		       "Tab='%s' key='%s' has %d registered buffers"
                       tab-name lookup-key (length numeric-buffers))

    ;; Collect buffer OBJECTS from numeric buffer system
    (when numeric-buffers
      (dolist (entry numeric-buffers)
        (let* ((id (car entry))
               (buffer-obj (cdr entry)))
					; This is now a buffer object!
          (etm-debug-message 'close
			     "  ID=%d obj=#<%s> name='%s' live=%s"
                             id
                             (prin1-to-string buffer-obj)
                             (if (buffer-live-p buffer-obj)
                                 (buffer-name buffer-obj)
                               "DEAD")
                             (buffer-live-p buffer-obj))
          (when (buffer-live-p buffer-obj)
            (push buffer-obj buffers-to-process)))))

    ;; Process each buffer
    (dolist (buffer buffers-to-process)
      (let ((buffer-name (buffer-name buffer))
            (is-dired (with-current-buffer buffer
                        (derived-mode-p 'dired-mode)))
            (other-tab
	     (--etm-numeric-buffer-in-other-tabs-p buffer lookup-key)))

        (etm-debug-message 'close
			   "Processing: '%s' dired=%s other-tab=%s"
                           buffer-name is-dired (or other-tab "none"))

        (cond
         ;; Buffer is used by another tab - skip it entirely
         (other-tab
          (push (cons buffer-name other-tab) skipped-shared)
          (etm-debug-message 'close "  -> SKIP: also in tab '%s'"
			     other-tab))

         ;; Dired buffer only in this tab - bury it
         (is-dired
          (bury-buffer buffer)
          (etm-debug-message 'close "  -> BURY: dired buffer")
          (cl-incf killed-count))

         ;; Regular buffer only in this tab - kill it
         (t
          (kill-buffer buffer)
          (etm-debug-message 'close "  -> KILL")
          (cl-incf killed-count)))))

    ;; Clear numeric buffer entries for this tab
    (etm-numeric-clear-tab lookup-key)

    ;; Close the tab itself
    (tab-close)
    (tab-next)

    ;; Final message
    (if skipped-shared
        (message
	 "Closed '%s': %d processed, %d shared buffers preserved: %s"
         tab-name killed-count (length skipped-shared)
         (mapconcat
	  (lambda (x) (format "'%s'(in %s)" (car x) (cdr x)))
          skipped-shared ", "))
      (message "Closed tab '%s' and processed %d buffers"
               tab-name killed-count))))

(provide 'etm-close-kill-associated-buffers)

;;; etm-close-kill-associated-buffers.el ends here
