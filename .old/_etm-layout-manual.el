;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 23:52:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/.old/_etm-layout-manual.el

(defvar --my/host-ignored "ignore")

;; Sub-modules
;; ----------------------------------------

;; (defun etm-new
;;     (tab-name)
;;   "Initialize a new tab with TAB-NAME, closing any existing tab with the same name."
;;   ;; (etm-close-by-name tab-name)
;;   (message
;;    (format "etm-%s" tab-name))
;;   (etm-new tab-name))

(defun --etm-base-1
    (elisp-script)
  "Set up the left side of the tab, executing ELISP-SCRIPT and splitting the window."
  (funcall elisp-script)
  (etm-buffer-set "home"))

(defun --etm-base-left
    (elisp-script)
  "Set up the left side of the tab, executing ELISP-SCRIPT and splitting the window."
  (funcall elisp-script)
  (etm-buffer-set "home")
  (split-window-right))

(defun --etm-base-right
    (elisp-script)
  "Set up the right side of the tab, executing ELISP-SCRIPT in a new window."
  (other-window 1)
  (funcall elisp-script)
  (etm-buffer-set "semi-home")
  (other-window -1))

(defun --etm-base-bottom
    (elisp-script)
  "Starting from the left windows, set up the bottom side of the tab, executing ELISP-SCRIPT and splitting the window."
  (split-window-vertically)
  (other-window 1)
  (funcall elisp-script)
  (other-window -1)
  )

(defun --etm-cleanup
    (tab-name)
  "Perform cleanup operations after setting up the tab TAB-NAME."
  (etm-close-by-name "default"))

;; Base
;; ----------------------------------------

(defun --etm-base-1-shell
    (tab-name path &optional host)
  "Set up a new tab with a split view of file explorer and terminal.
TAB-NAME is the name of the new tab.
LEFT-PATH is the path to open in the left window (dired).
RIGHT-PATH is the path to cd into in the right window (terminal).
HOST is the optional remote host to connect to.

Example usage:
(--etm-base-left-1-right-1 \"project-remote\" \"/path/to/project\" \"/home/user/project\" \"remote-host\")
(--etm-base-left-1-right-1 \"project-local\" \"/path/to/project\" \"/home/user/project\")"
  (let*
      ((selected-host
        (if
            (or
             (null host)
             (string= host ""))
            (--my/ssh-select-host)
          host))

       (effective-path
        (if
            (and selected-host
                 (not
                  (member selected-host --my/local-host-names))
                 (not
                  (string= selected-host --my/host-ignored)))
            (format "/ssh:%s:%s"
                    selected-host
                    (--my/ssh-rename-username path selected-host))
          path)))

    (etm-new tab-name)
    (--etm-base-1-shell tab-name)))

;; (defun --etm-base-left-1-right-1 (tab-name left-path right-path &optional host)
;;   "Set up a new tab with a split view of file explorer and terminal.
;; TAB-NAME is the name of the new tab.
;; LEFT-PATH is the path to open in the left window (dired).
;; RIGHT-PATH is the path to cd into in the right window (terminal).
;; HOST is the optional remote host to connect to.

;; Example usage:
;; (--etm-base-left-1-right-1 \"project-remote\" \"/path/to/project\" \"/home/user/project\" \"remote-host\")
;; (--etm-base-left-1-right-1 \"project-local\" \"/path/to/project\" \"/home/user/project\")"
;;   (let* ((selected-host (if (or (null host) (string= host ""))
;;                            (--my/ssh-select-host)
;;                          host))

;;          (effective-left-path (if (and selected-host
;;                                       (not (member selected-host --my/local-host-names))
;;                                       (not (string= selected-host --my/host-ignored)))
;;                                  (format "/ssh:%s:%s"
;;                                         selected-host
;;                                         (--my/ssh-rename-username left-path selected-host))
;;                                left-path))
;;          (effective-right-path (if (and selected-host
;;                                       (not (member selected-host --my/local-host-names))
;;                                       (not (string= selected-host --my/host-ignored)))
;;                                  (--my/ssh-rename-username right-path selected-host)
;;                                right-path)))

;;     (etm-new tab-name)
;;     (--etm-base-left (lambda ()
;;                           (find-file effective-left-path)))
;;     (--etm-base-right (lambda ()
;;       (my/term-new tab-name)
;;       (term-send-raw-string "pwd\n")
;;       (when (and selected-host (not (member selected-host --my/local-host-names)))
;;         (term-send-raw-string "hostname\n")
;;         (term-send-raw-string
;;          (format
;;           "if [[ \"$(hostname)\" != *\"%s\"* ]]; then
;;             ssh -Y %s
;;           fi\n"
;;           selected-host selected-host)))
;;       (term-send-raw-string
;;        (format
;;         "if [[ \"$(pwd)\" != \"%s\" ]]; then
;;           cd %s
;;         fi
;;         clear\n"
;;         effective-right-path effective-right-path))
;;       ))
;;     (--etm-cleanup tab-name)))

(defun --etm-base-left-1-right-1
    (tab-name left-path right-path &optional host)
  "Set up a new tab with a split view of file explorer and terminal.
TAB-NAME is the name of the new tab.
LEFT-PATH is the path to open in the left window (dired).
RIGHT-PATH is the path to cd into in the right window (terminal).
HOST is the optional remote host to connect to.

Example usage:
(--etm-base-left-1-right-1 \"project-remote\" \"/path/to/project\" \"/home/user/project\" \"remote-host\")
(--etm-base-left-1-right-1 \"project-local\" \"/path/to/project\" \"/home/user/project\")"
  (let*
      ((selected-host
        (if
            (or
             (null host)
             (string= host ""))
            (--my/ssh-select-host)
          host))
       (is-remote
        (and selected-host
             (not
              (member selected-host --my/local-host-names))
             (not
              (string= selected-host --my/host-ignored))))

       (effective-left-path
        (if is-remote
            (--my/ssh-rename-username
             (format "/ssh:%s:%s" selected-host left-path)
             selected-host)

          (if
              (string-match-p "README\\." left-path)
              (--my/path-find-readme-path-local
               (file-name-directory left-path))
            left-path)))

       (effective-right-path
        (if is-remote
            (--my/ssh-rename-username right-path selected-host)
          right-path)))

    (etm-new tab-name)
    (--etm-base-left
     (lambda
       ()
       (find-file effective-left-path)))
    (--etm-base-right
     (lambda
       ()
       (my/term-new tab-name)
       (term-send-raw-string "pwd\n")
       (when
           (and selected-host
                (not
                 (member selected-host --my/local-host-names)))
         (term-send-raw-string "hostname\n")
         (term-send-raw-string
          (format
           "if [[ \"$(hostname)\" != *\"%s\"* ]]; then
            ssh -Y %s
          fi\n"
           selected-host selected-host)))
       (term-send-raw-string
        (format
         "if [[ \"$(pwd)\" != \"%s\" ]]; then
          cd %s
        fi
        clear\n"
         effective-right-path effective-right-path))
       ))
    (--etm-cleanup tab-name)))

(defun --etm-base-left-2-right-1
    (tab-name upper-left-path lower-left-path right-path &optional host)
  (let*
      ((selected-host
        (if
            (or
             (null host)
             (string= host ""))
            (--my/ssh-select-host)
          host))

       (effective-lower-left-path
        (if
            (and selected-host
                 (not
                  (member selected-host --my/local-host-names))
                 (not
                  (string= selected-host --my/host-ignored)))
            (format "/ssh:%s:%s"
                    selected-host
                    (--my/ssh-rename-username lower-left-path selected-host))
          lower-left-path)))

    (--etm-base-left-1-right-1 tab-name upper-left-path right-path host)
    (--etm-base-bottom
     (lambda
       ()
       (find-file effective-lower-left-path)))
    ))

(defun --etm-base-left-1-right-2
    (tab-name left-path upper-right-path lower-right-path &optional host)
  (let*
      ((selected-host
        (if
            (or
             (null host)
             (string= host ""))
            (--my/ssh-select-host)
          host))
       (is-remote
        (and selected-host
             (not
              (member selected-host --my/local-host-names))
             (not
              (string= selected-host --my/host-ignored))))
       (effective-left-path
        (if is-remote
            (--my/ssh-rename-username
             (format "/ssh:%s:%s" selected-host left-path)
             selected-host)
          left-path))
       (effective-upper-right-path
        (if is-remote
            (--my/ssh-rename-username upper-right-path selected-host)
          upper-right-path))
       (effective-lower-right-path
        (if is-remote
            (--my/ssh-rename-username lower-right-path selected-host)
          lower-right-path)))
    (etm-new tab-name)
    (--etm-base-left
     (lambda
       ()
       (find-file effective-left-path)))
    (--etm-base-right
     (lambda
       ()
       (my/term-new
        (format "%s-upper" tab-name))
       (term-send-raw-string
        (format
         "cd %s && clear\n"
         effective-upper-right-path))))
    (--etm-base-bottom
     (lambda
       ()
       (my/term-new
        (format "%s-lower" tab-name))
       (term-send-raw-string
        (format
         "cd %s && clear\n"
         effective-lower-right-path))))
    (--etm-cleanup tab-name)))

;; Async Base
;; ----------------------------------------

(require 'async)

(defun --etm-base-left-1-right-1-async
    (tab-name left-path right-path &optional host)
  "Run tab-base function after a short delay."
  (run-with-idle-timer
   0.1 nil #'--etm-base-left-1-right-1 tab-name left-path right-path host))

(defun --etm-base-left-2-right-1-async
    (tab-name upper-left-path lower-left-path right-path &optional host)
  "Run tab-base function after a short delay."
  (run-with-idle-timer
   0.1 nil #'--etm-base-left-2-right-1 tab-name upper-left-path lower-left-path right-path host))

(defun --etm-base-left-1-right-2-async
    (tab-name left-path upper-right-path lower-right-path &optional host)
  "Run tab-base function after a short delay."
  (run-with-idle-timer
   0.1 nil #'--etm-base-left-1-right-2 tab-name left-path upper-right-path lower-right-path host))

(provide '_etm-layout-manual)

(when
    (not load-file-name)
  (message "_etm-layout-manual.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))