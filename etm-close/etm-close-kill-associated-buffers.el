;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-14 13:28:10>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/etm-close/etm-close-kill-associated-buffers.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(defun etm-close-kill-associated-buffers ()
  "Kill all buffers associated with the current tab."
  (interactive)
  (let* ((current-tab (tab-bar--current-tab))
         (tab-name (alist-get 'name current-tab))
         (tab-entry (assoc tab-name etm-registered-buffers))
         (buffers-to-kill '()))

    ;; Collect all buffers associated with this tab
    (when tab-entry
      (dolist (buffer-entry (cdr tab-entry))
        (let* ((buffer-id (cadr buffer-entry))
               (buffer (cl-find-if
                        (lambda (buf)
                          (with-current-buffer buf
                            (and (local-variable-p 'etm-buffer-id)
                                 (string= etm-buffer-id buffer-id))))
                        (buffer-list))))
          (when buffer
            (push buffer buffers-to-kill)))))

    ;; Kill all collected buffers
    (dolist (buffer buffers-to-kill)
      (kill-buffer buffer))

    ;; Close the tab itself
    (tab-close)
    (tab-next)
    (message "Killed all buffers in tab '%s'" tab-name)))

;; ;; Bind it to C-w

;; (global-set-key (kbd "C-w") 'etm-close-kill-associated-buffers)


(provide 'etm-close-kill-associated-buffers)

(when
    (not load-file-name)
  (message "etm-close-kill-associated-buffers.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))