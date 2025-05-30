;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-25 08:59:31>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/etm-layout/etm-layout-save.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(defun etm-layout-save (layout-name)
  "Save current layout to a new elisp file in the tab configuration directory."
  (interactive "sEnter layout name: ")
  (let* ((layout-name (format "%s" layout-name))
         (full-path (expand-file-name
                     (format "etm-open-%s.el" layout-name)
                     etm-layout-save-dir))
         (host-input (read-string "Enter host (or leave empty): "))
         (host (if (string= host-input "") nil host-input))
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
          layout-name))

(defun --etm-layout-capture-current-layout
    (layout-name &optional host)
  "Capture the current tab layout and generate a function to recreate it."
  (unless (frame-parameter nil 'fullscreen)
    (toggle-frame-fullscreen)
    (sit-for 0.3))
  (let ((windows (window-list))
        (windows-data '()))
    (dolist (window windows)
      (let* ((buffer (window-buffer window))
             (is-term (with-current-buffer buffer
                        (or (derived-mode-p 'term-mode)
                            (derived-mode-p 'vterm-mode))))
             (is-dired (with-current-buffer buffer
                         (eq major-mode 'dired-mode)))
             (type (cond (is-term 'shell) (is-dired 'file) (t 'file)))
             (path (cond
                    (is-term default-directory)
                    (is-dired
                     (with-current-buffer buffer default-directory))
                    (t
                     (or (buffer-file-name buffer) default-directory))))
             (path-host nil)
             (clean-path path)
             (edges (window-edges window))
             (x (nth 0 edges))
             (y (nth 1 edges))
             (width (- (nth 2 edges) x))
             (height (- (nth 3 edges) y)))
        (when (string-match "^/ssh:\\([^:]+\\):\\(.*\\)" path)
          (setq path-host (match-string 1 path))
          (setq clean-path (match-string 2 path)))
        (push (list type clean-path x y width height
                    (--etm-layout-determine-effective-host path-host
                                                           (when host
                                                             host)))
              windows-data)))
    (setq windows-data
          (sort windows-data
                (lambda (a b)
                  (or (< (nth 3 a) (nth 3 b))
                      (and (= (nth 3 a) (nth 3 b))
                           (< (nth 2 a) (nth 2 b)))))))
    (format
     "(--etm-layout-create-from-positions \"%s\"\n  '(%s)\n  %s)"
     layout-name
     (mapconcat
      (lambda (win-data)
        (format "(%s \"%s\" %d %d %d %d %s)"
                (nth 0 win-data) (nth 1 win-data) (nth 2 win-data)
                (nth 3 win-data) (nth 4 win-data) (nth 5 win-data)
                (if (nth 6 win-data)
                    (format "\"%s\"" (nth 6 win-data))
                  "nil")))
      windows-data
      "\n   ")
     (if host (format "\"%s\"" host) "nil"))))


(provide 'etm-layout-save)

(when
    (not load-file-name)
  (message "etm-layout-save.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))