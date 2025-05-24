;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-19 12:01:15>
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

  ;; Force fullscreen mode to keep x, y coordinates
  (toggle-frame-fullscreen)
  (sit-for 0.3)

  (let ((windows (window-list))
        (windows-data '()))
    ;; Collect data for each window
    (dolist (window windows)
      (let* ((buffer (window-buffer window))
             (buffer-name (buffer-name buffer))
             (file (buffer-file-name buffer))
             (is-term (with-current-buffer buffer
                        (or (derived-mode-p 'term-mode)
                            (derived-mode-p 'vterm-mode))))
             (is-dired (with-current-buffer buffer
                         (eq major-mode 'dired-mode)))
             (type (cond
                    (is-term 'shell)
                    (is-dired 'file)
                    (t 'file)))
             (path (cond
                    (is-term default-directory)
                    (is-dired
                     (with-current-buffer buffer default-directory))
                    (t (or file default-directory))))
             (path-host nil)
             (clean-path path)
             (edges (window-edges window))
             (x (nth 0 edges))
             (y (nth 1 edges))
             (width (- (nth 2 edges) x))
             (height (- (nth 3 edges) y)))

        ;; Extract host from SSH paths
        (when (string-match "^/ssh:\\([^:]+\\):\\(.*\\)" path)
          (setq path-host (match-string 1 path))
          (setq clean-path (match-string 2 path)))

        (push (list type clean-path x y width height path-host)
              windows-data)))

    ;; Sort windows by position (top-left to bottom-right)
    (setq windows-data
          (sort windows-data
                (lambda (a b)
                  (or (< (nth 3 a) (nth 3 b))  ; First by y
                      (and (= (nth 3 a) (nth 3 b))
                           (< (nth 2 a) (nth 2 b)))))))
                                        ; Then by x
    ;; Generate layout recreation code
    (format
     "(--etm-layout-create-from-positions \"%s\"\n  '(%s)\n  %s)"
     layout-name
     (mapconcat
      (lambda (win-data)
        (format "(%s \"%s\" %d %d %d %d %s)"
                (nth 0 win-data)    ; type
                (nth 1 win-data)    ; path
                (nth 2 win-data)    ; x
                (nth 3 win-data)    ; y
                (nth 4 win-data)    ; width
                (nth 5 win-data)    ; height
                (if (and host (nth 6 win-data))
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