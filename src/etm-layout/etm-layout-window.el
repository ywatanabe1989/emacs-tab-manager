;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-25 08:58:36>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/etm-layout/etm-layout-window.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

(require 'etm-core-helpers)

(defun --etm-layout-init-windows
    (tab-name num-left num-right)
  "Initialize tab layout with NUM-LEFT windows on left and NUM-RIGHT on right.
Split horizontally first, then vertically within each side."
  (etm-new tab-name)
  (split-window-horizontally)
  (dotimes
      (_
       (1- num-left))
    (split-window-vertically))
  (other-window num-left)
  (dotimes
      (_
       (1- num-right))
    (split-window-vertically))
  (other-window
   (- num-left)))

(defun --etm-layout-setup-window
    (n window-type path &optional host)
  "Setup window N with WINDOW-TYPE ('file or 'shell) at PATH with optional HOST."
  (let*
      ((selected-host
        (or host
            (--etm-ssh-select-host)))
       (is-remote
        (and selected-host
             (not
              (member selected-host etm-localhost-names))
             (not
              (string= selected-host etm-ignored-host))))
       (effective-path
        (if is-remote
            (if
                (eq window-type 'file)
                (format "/ssh:%s:%s"
                        selected-host
                        (--etm-ssh-rename-username path selected-host))
              (--etm-ssh-rename-username path selected-host))
          path))
       (target-window (selected-window)))
    (cond
     ((eq window-type 'file)
      (find-file effective-path))
     ((eq window-type 'shell)
      (let ((vterm-buffer (--etm-vterm-new (format "term-%d" n))))
        (set-window-buffer target-window vterm-buffer)
        (select-window target-window))
      (when is-remote
        (vterm-send-string
         (format
          "if [[ \"$(hostname)\" != *\"%s\"* ]]; then ssh -Y %s; fi\n"
          selected-host selected-host)))
      (let ((init-cmd (etm-vterm-get-init-command n)))
        (vterm-send-string
         (format "cd %s && clear%s\n" effective-path
                 (if init-cmd (concat "\n" init-cmd) ""))))))))

(defun --etm-layout-setup-window-with-host
    (n window-type path host)
  "Setup window N with WINDOW-TYPE ('file or 'shell) at PATH with specified HOST."
  (let*
      ((is-remote
        (and host
             (not
              (member host etm-localhost-names))
             (not
              (string= host etm-ignored-host))))
       (effective-path
        (if is-remote
            (if
                (eq window-type 'file)
                (format "/ssh:%s:%s"
                        host
                        (--etm-ssh-rename-username path host))
              (--etm-ssh-rename-username path host))
          path))
       (target-window (selected-window)))
    (cond
     ((eq window-type 'file)
      (find-file effective-path))
     ((eq window-type 'shell)
      (let ((vterm-buffer (--etm-vterm-new (format "term-%d" n))))
        (set-window-buffer target-window vterm-buffer)
        (select-window target-window))
      (message "Creating shell window %d for path: %s" n
               effective-path)
      (when is-remote
        (message "Sending ssh command to %s" host)
        (vterm-send-string
         (format
          "if [[ \"$(hostname)\" != *\"%s\"* ]]; then ssh -Y %s; fi\n"
          host host)))
      (message "Sending cd command to %s" effective-path)
      (sit-for 0.3)
      (let ((init-cmd (etm-vterm-get-init-command n)))
        (vterm-send-string
         (format "cd %s && clear%s\n" effective-path
                 (if init-cmd (concat "\n" init-cmd) ""))))))))

(defun --etm-layout-determine-effective-host (path-host selected-host)
  "Determine effective host: PATH-HOST overrides SELECTED-HOST, allowing nil."
  (if path-host
      path-host
    selected-host))

(defun --etm-layout-is-remote-host (host)
  "Check if HOST is remote (not localhost or ignored)."
  (and host
       (not (member host etm-localhost-names))
       (not (string= host etm-ignored-host))))

(defun --etm-layout-build-remote-path (host path window-type)
  "Build remote path for HOST and PATH based on WINDOW-TYPE."
  (if (eq window-type 'file)
      (format "/ssh:%s:%s" host (--etm-ssh-rename-username path host))
    (--etm-ssh-rename-username path host)))

(defun --etm-layout-get-effective-path (path window-type host)
  "Get effective path for PATH with WINDOW-TYPE and HOST."
  (if (--etm-layout-is-remote-host host)
      (--etm-layout-build-remote-path host path window-type)
    path))

(provide 'etm-layout-window)

(when
    (not load-file-name)
  (message "etm-layout-window.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
