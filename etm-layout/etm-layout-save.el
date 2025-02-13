;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 00:03:39>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-layout/etm-layout-save.el
;;; -*- coding: utf-8; lexical-binding: t -*-

(defun etm-layout-save
    (layout-name)
  "Save current layout to a new elisp file in the tab configuration directory."
  (interactive "sEnter layout name: ")
  (let*
      ((full-path
        (expand-file-name
         (format "etm-open-%s.el" layout-name)
         etm-layout-save-dir))
       (host
        (read-string "Enter host (or leave empty): "))
       (contents
        (concat ";;; -*- coding: utf-8; lexical-binding: t -*-\n"
                (format ";;; Author: %s\n"
                        (format-time-string "%Y-%m-%d %H:%M:%S"))
                (format ";;; Timestamp: <%s>\n"
                        (format-time-string "%Y-%m-%d %H:%M:%S"))
                (format ";;; File: %s\n\n" full-path)
                (format "(defun etm-open-%s ()\n" layout-name)
                "  \"Create tab layout for specific configuration.\"\n"
                "  (interactive)\n"
                (--etm-layout-capture-current-layout layout-name host)
                ")\n"
                (format "(defalias '%s 'etm-open-%s)\n" layout-name layout-name))))
    (message contents)
    (write-region contents nil full-path)
    (load-file full-path)))

(defun --etm-layout-capture-current-layout
    (layout-name &optional host)
  "Capture the current tab layout and generate a function to recreate it."
  (let
      ((windows-info
        '())
       (num-left 0)
       (num-right 0)
       (passed-split nil)
       (host
        (or host
            (read-string "Enter host (or leave empty): "))))
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
             (string-match-p "term-" buffer-name))
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
               (or file default-directory)))))
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
          (cons type path)
          windows-info)))
     nil 'visible)
    (let
        ((config-string
          (format "(--etm-layout-create \"%s\" %d %d\n  '(%s) \"%s\")"
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
                  host)))
      (message config-string)
      config-string)))

(provide 'etm-layout-save)

(when
    (not load-file-name)
  (message "etm-layout-save.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))