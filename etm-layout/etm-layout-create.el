;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-24 08:53:22>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/etm-layout/etm-layout-create.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'etm-layout-default)
(require 'etm-helpers)

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
       (is-first-shell t)
       (selected-host (or host
                          (gethash tab-name etm-layout-default-hosts)
                          (--my/ssh-select-host))))

    ;; Save the host as default if provided explicitly
    (when host
      (puthash tab-name host etm-layout-default-hosts))

    (dotimes
        (i
         (+ num-left num-right))
      (when
          (nth i window-configs)
        (let
            ((config
              (nth i window-configs)))
          (--etm-layout-setup-window-with-host i
                                               (car config)
                                               (cdr config)
                                               selected-host)
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

;; (defun --etm-layout-create
;;     (tab-name num-left num-right window-configs &optional host)
;;   "Create tab layout with specified window configurations.
;; WINDOW-CONFIGS is a list of (type . path) pairs for each window."
;;   (--etm-layout-init-windows tab-name num-left num-right)
;;   (let
;;       ((is-first-file t)
;;        (is-second-file t)
;;        (is-first-shell t)
;;        (selected-host (or host (--my/ssh-select-host))))
;;     (dotimes
;;         (i
;;          (+ num-left num-right))
;;       (when
;;           (nth i window-configs)
;;         (let
;;             ((config
;;               (nth i window-configs)))
;;           (--etm-layout-setup-window-with-host i
;;                                                (car config)
;;                                                (cdr config)
;;                                                selected-host)
;;           (when
;;               (and
;;                (and
;;                 (and
;;                  (not is-first-file)
;;                  is-second-file)
;;                 is-first-shell)
;;                (eq
;;                 (car config)
;;                 'file))
;;             (etm-buffer-set "semi-home")
;;             (setq is-second-file nil))
;;           (when
;;               (and is-first-file
;;                    (eq
;;                     (car config)
;;                     'file))
;;             (etm-buffer-set "home")
;;             (setq is-first-file nil))
;;           (when
;;               (and is-first-shell
;;                    (eq
;;                     (car config)
;;                     'shell))
;;             (etm-buffer-set "semi-home")
;;             (setq is-first-shell nil))
;;           (unless
;;               (= i
;;                  (1-
;;                   (+ num-left num-right)))
;;             (other-window 1))))))
;;   (--etm-layout-cleanup tab-name))

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
                        (--my/ssh-rename-username path host))
              (--my/ssh-rename-username path host))
          path)))
    (cond
     ((eq window-type 'file)
      (find-file effective-path))
     ((eq window-type 'shell)
      (--my/vterm-new
       (format "term-%d" n))
      (when is-remote
        (term-send-raw-string
         (format
          "if [[ \"$(hostname)\" != *\"%s\"* ]]; then ssh -Y %s; fi\n"
          host host)))
      (term-send-raw-string
       (format "cd %s && clear\n" effective-path))))))

(provide 'etm-layout-create)

(when
    (not load-file-name)
  (message "etm-layout-create.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))