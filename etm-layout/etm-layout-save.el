;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-05 14:27:01>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-layout/etm-layout-save.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(defun etm-layout-save
    (layout-name)
  "Save current layout to a new elisp file in the tab configuration directory."
  (interactive "sEnter layout name: ")
  (let*
      ((layout-name
        (format "%s" layout-name))
       (full-path
        (expand-file-name
         (format "etm-open-%s.el" layout-name)
         etm-layout-save-dir))
       ;; (host
       ;;  (read-string "Enter host (or leave empty): "))
       (host-input
        (read-string "Enter host (or leave empty): "))
       (host
        (if (string= host-input "")
            nil
          host-input))
       (captured-layout
        (--etm-layout-capture-current-layout layout-name host))
       (contents
        (--etm-layout-construct-contents layout-name captured-layout)))
    (with-current-buffer
        (find-file-noselect full-path)
      (erase-buffer)
      (insert contents)
      (save-buffer)
      (kill-buffer))
    (load-file full-path)))

(defun --etm-layout-construct-contents
    (layout-name captured-layout)
  "Construct contents for layout file.
LAYOUT-NAME is the name of the layout.
CAPTURED-LAYOUT is the layout configuration string."
  (format "(defun etm-open-%s ()
  \"Create tab layout for specific configuration.\"
  (interactive)
  %s)

(defalias '%s 'etm-open-%s)"
          layout-name
          captured-layout
          layout-name
          layout-name
          layout-name))

;; (defun --etm-layout-capture-current-layout
;;     (layout-name &optional host)
;;   "Capture the current tab layout and generate a function to recreate it."
;;   (let
;;       ((windows-info
;;         '())
;;        (num-left 0)
;;        (num-right 0)
;;        (passed-split nil)
;;        (host
;;         (or host
;;             (read-string "Enter host (or leave empty): "))))
;;     (walk-windows
;;      (lambda
;;        (window)
;;        (let*
;;            ((buffer
;;              (window-buffer window))
;;             (buffer-name
;;              (buffer-name buffer))
;;             (file
;;              (buffer-file-name buffer))
;;             (is-term
;;              (with-current-buffer buffer
;;                (or (derived-mode-p 'term-mode)
;;                    (derived-mode-p 'vterm-mode))))
;;             (is-dired
;;              (with-current-buffer buffer
;;                (eq major-mode 'dired-mode)))
;;             (type
;;              (cond
;;               (is-term 'shell)
;;               (is-dired 'file)
;;               (t 'file)))
;;             (path
;;              (cond
;;               (is-term default-directory)
;;               (is-dired
;;                (with-current-buffer buffer default-directory))
;;               (t
;;                (or file default-directory)))))
;;          (if
;;              (not passed-split)
;;              (if
;;                  (>
;;                   (window-pixel-left window)
;;                   (window-pixel-left
;;                    (frame-first-window)))
;;                  (setq passed-split t
;;                        num-right
;;                        (1+ num-right))
;;                (setq num-left
;;                      (1+ num-left)))
;;            (setq num-right
;;                  (1+ num-right)))
;;          (push
;;           (cons type path)
;;           windows-info)))
;;      nil 'visible)
;;     (let
;;         ((config-string
;;           (format "(--etm-layout-create \"%s\" %d %d\n  '(%s) \"%s\")"
;;                   layout-name
;;                   num-left
;;                   num-right
;;                   (mapconcat
;;                    (lambda
;;                      (conf)
;;                      (format "(%s . \"%s\")"
;;                              (car conf)
;;                              (cdr conf)))
;;                    (reverse windows-info)
;;                    "\n    ")
;;                   host)))
;;       (message config-string)
;;       config-string)))

;; (defun --etm-layout-capture-current-layout
;;     (layout-name &optional host)
;;   "Capture the current tab layout and generate a function to recreate it."
;;   (let
;;       ((windows-info
;;         '())
;;        (num-left 0)
;;        (num-right 0)
;;        (passed-split nil))
;;     (walk-windows
;;      (lambda
;;        (window)
;;        (let*
;;            ((buffer
;;              (window-buffer window))
;;             (buffer-name
;;              (buffer-name buffer))
;;             (file
;;              (buffer-file-name buffer))
;;             (is-term
;;              (with-current-buffer buffer
;;                (or (derived-mode-p 'term-mode)
;;                    (derived-mode-p 'vterm-mode))))
;;             (is-dired
;;              (with-current-buffer buffer
;;                (eq major-mode 'dired-mode)))
;;             (type
;;              (cond
;;               (is-term 'shell)
;;               (is-dired 'file)
;;               (t 'file)))
;;             (path
;;              (cond
;;               (is-term default-directory)
;;               (is-dired
;;                (with-current-buffer buffer default-directory))
;;               (t
;;                (or file default-directory)))))
;;          (if
;;              (not passed-split)
;;              (if
;;                  (>
;;                   (window-pixel-left window)
;;                   (window-pixel-left
;;                    (frame-first-window)))
;;                  (setq passed-split t
;;                        num-right
;;                        (1+ num-right))
;;                (setq num-left
;;                      (1+ num-left)))
;;            (setq num-right
;;                  (1+ num-right)))
;;          (push
;;           (cons type path)
;;           windows-info)))
;;      nil 'visible)
;;     (let
;;         ((config-string
;;           (format "(--etm-layout-create \"%s\" %d %d\n  '(%s) %s)"
;;                   layout-name
;;                   num-left
;;                   num-right
;;                   (mapconcat
;;                    (lambda
;;                      (conf)
;;                      (format "(%s . \"%s\")"
;;                              (car conf)
;;                              (cdr conf)))
;;                    (reverse windows-info)
;;                    "\n    ")
;;                   (if host
;;                       (format "\"%s\"" host)
;;                     "nil"))))
;;       (message config-string)
;;       config-string)))

(defun --etm-layout-capture-current-layout
    (layout-name &optional host)
  "Capture the current tab layout and generate a function to recreate it."
  (let
      ((windows-info
        '())
       (num-left 0)
       (num-right 0)
       (passed-split nil))
    (walk-windows
     (lambda
       (window)
       (let*
           ((buffer
             (window-buffer window))
            (buffer-name
             (buffer-name buffer))
            (file
             (buffer-file-name buffer))
            (is-term
             (with-current-buffer buffer
               (or (derived-mode-p 'term-mode)
                   (derived-mode-p 'vterm-mode))))
            (is-dired
             (with-current-buffer buffer
               (eq major-mode 'dired-mode)))
            (type
             (cond
              (is-term 'shell)
              (is-dired 'file)
              (t 'file)))
            (path
             (cond
              (is-term default-directory)
              (is-dired
               (with-current-buffer buffer default-directory))
              (t
               (or file default-directory))))
            (clean-path
             (if (string-match "^/ssh:[^:]+:\\(.*\\)" path)
                 (match-string 1 path)
               path)))
         (if
             (not passed-split)
             (if
                 (>
                  (window-pixel-left window)
                  (window-pixel-left
                   (frame-first-window)))
                 (setq passed-split t
                       num-right
                       (1+ num-right))
               (setq num-left
                     (1+ num-left)))
           (setq num-right
                 (1+ num-right)))
         (push
          (cons type clean-path)
          windows-info)))
     nil 'visible)
    (let
        ((config-string
          (format "(--etm-layout-create \"%s\" %d %d\n  '(%s) %s)"
                  layout-name
                  num-left
                  num-right
                  (mapconcat
                   (lambda
                     (conf)
                     (format "(%s . \"%s\")"
                             (car conf)
                             (cdr conf)))
                   (reverse windows-info)
                   "\n    ")
                  (if host
                      (format "\"%s\"" host)
                    "nil"))))
      (message config-string)
      config-string)))

(provide 'etm-layout-save)

(when
    (not load-file-name)
  (message "etm-layout-save.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))