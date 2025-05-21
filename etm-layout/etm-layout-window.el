;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-19 12:01:16>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/etm-layout/etm-layout-window.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


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
          path)))
    (cond
     ((eq window-type 'file)
      (find-file effective-path))
     ((eq window-type 'shell)
      (--etm-vterm-new
       (format "term-%d" n))
      (when is-remote
        (term-send-string
         (format
          "if [[ \"$(hostname)\" != *\"%s\"* ]]; then ssh -Y %s; fi\n"
          selected-host selected-host)))
      (term-send-string
       (format "cd %s && clear\n" effective-path))))))

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
          path)))
    (cond
     ((eq window-type 'file)
      (find-file effective-path))
     ((eq window-type 'shell)
      (--etm-vterm-new
       (format "term-%d" n))
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
      (vterm-send-string
       (format "cd %s && clear\n" effective-path))))))


(provide 'etm-layout-window)

(when
    (not load-file-name)
  (message "etm-layout-window.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))