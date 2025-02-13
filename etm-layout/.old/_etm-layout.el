;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 23:09:14>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-layout.el

;; 1. User Interface Functions
;; ----------------------------------------

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
        (concat
         ";;; -*- coding: utf-8; lexical-binding: t -*-\n"
         (format ";;; Author: %s\n"
                 (format-time-string "%Y-%m-%d %H:%M:%S"))
         (format ";;; Timestamp: <%s>\n"
                 (format-time-string "%Y-%m-%d %H:%M:%S"))
         (format ";;; File: %s\n\n" full-path)
         (format "(defun etm-open-%s ()\n" layout-name)
         "  \"Create tab layout for specific configuration.\"\n"
         "  (interactive)\n"
         (--etm-layout-capture-current-layout layout-name host)
         ")"
         "\n"
         (format "(defalias '%s 'etm-open-%s)\n" layout-name layout-name)
         )))
    (message contents)
    (write-region contents nil full-path)
    (load-file full-path)))

;; 2. Core Layout Functions
;; ----------------------------------------

(defun --etm-layout-create
    (tab-name num-left num-right window-configs &optional host)
  "Create tab layout with specified window configurations.
        WINDOW-CONFIGS is a list of (type . path) pairs for each window."
  ;; (message "Creating tab layout: name=%s left=%d right=%d configs=%S"
  ;;          tab-name num-left num-right window-configs)
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
            (other-window 1)))))
    (--etm-cleanup tab-name)))

(defun --etm-layout-init-windows
    (tab-name num-left num-right)
  "Initialize tab layout with NUM-LEFT windows on left and NUM-RIGHT on right.
        Split horizontally first, then vertically within each side."
  ;; (message "Initializing windows: name=%s left=%d right=%d"
  ;;          tab-name num-left num-right)
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
            (--my/ssh-select-host)))
       (is-remote
        (and selected-host
             (not
              (member selected-host --my/local-host-names))
             (not
              (string= selected-host --my/host-ignored))))
       (effective-path
        (if is-remote
            (if
                (eq window-type 'file)
                (format "/ssh:%s:%s"
                        selected-host
                        (--my/ssh-rename-username path selected-host))
              (--my/ssh-rename-username path selected-host))
          path)))
    (cond
     ((eq window-type 'file)
      (find-file effective-path))
     ((eq window-type 'shell)
      (my/term-new
       (format "term-%d" n))
      (when is-remote
        (term-send-raw-string
         (format "if [[ \"$(hostname)\" != *\"%s\"* ]]; then ssh -Y %s; fi\n"
                 selected-host selected-host)))
      (term-send-raw-string
       (format "cd %s && clear\n" effective-path))))))

;; 3. Helper Functions
;; ----------------------------------------

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

(defun --etm-layout-load-all
    ()
  (dolist
      (file
       (directory-files etm-layout-save-dir t "\\.el$"))
    (load-file file)))

(--etm-layout-load-all)

(provide 'etm-layout)

(when
    (not load-file-name)
  (message "etm-layout.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))