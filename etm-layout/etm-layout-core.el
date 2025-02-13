;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 23:53:19>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-layout/etm-layout-core.el
;;; -*- coding: utf-8; lexical-binding: t -*-

(defun --etm-layout-cleanup
    (tab-name)
  "Perform cleanup operations after setting up the tab TAB-NAME."
  (etm-close-by-name "default"))

(defun --etm-layout-create
    (tab-name num-left num-right window-configs &optional host)
  "Create tab layout with specified window configurations.
WINDOW-CONFIGS is a list of (type . path) pairs for each window."
  (--etm-layout-init-windows tab-name num-left num-right)
  (let
      ((is-first-file t)
       (is-second-file t)
       (is-first-shell t))
    (dotimes
        (i
         (+ num-left num-right))
      (when
          (nth i window-configs)
        (let
            ((config
              (nth i window-configs)))
          (--etm-layout-setup-window i
                                     (car config)
                                     (cdr config)
                                     host)
          (when
              (and
               (and
                (and
                 (not is-first-file)
                 is-second-file)
                is-first-shell)
               (eq
                (car config)
                'file))
            (etm-buffer-set "semi-home")
            (setq is-second-file nil))
          (when
              (and is-first-file
                   (eq
                    (car config)
                    'file))
            (etm-buffer-set "home")
            (setq is-first-file nil))
          (when
              (and is-first-shell
                   (eq
                    (car config)
                    'shell))
            (etm-buffer-set "semi-home")
            (setq is-first-shell nil))
          (unless
              (= i
                 (1-
                  (+ num-left num-right)))
            (other-window 1))))))
  (--etm-layout-cleanup tab-name))

(provide 'etm-layout-core)

(when
    (not load-file-name)
  (message "etm-layout-core.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))