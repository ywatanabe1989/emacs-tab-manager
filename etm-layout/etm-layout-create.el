;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-20 01:00:23>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/etm-layout/etm-layout-create.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'etm-layout-default)
(require 'etm-core-helpers)
(require 'etm-core-ssh-helpers)
(require 'etm-core-ssh-connection)

;; Helper function for window counting

(defun window-count ()
  "Return the number of windows in the current frame."
  (length (window-list)))

;; 1. Layout cleanup
;; ----------------------------------------

(defun --etm-layout-cleanup-default-buffers ()
  "Perform cleanup operations after setting up the tab TAB-NAME."
  (sit-for 0.3)
  (etm-close-by-name "default")
  (etm-close-by-name "*scratch*")  )

;; 2. Window initialization
;; ----------------------------------------

(defun --etm-layout-init-windows (tab-name num-left num-right)
  "Initialize windows for tab layout.
TAB-NAME is the name for the new tab.
NUM-LEFT is the number of windows on the left side.
NUM-RIGHT is the number of windows on the right side."
  (etm-new tab-name)

  ;; Create window structure
  (when (> num-left 1)
    (dotimes (_ (1- num-left))
      (split-window-vertically)
      (other-window 1)))

  (when (> num-right 0)
    ;; Move to first window
    (other-window (- (window-count)))
    (split-window-horizontally)
    (other-window 1)

    (when (> num-right 1)
      (dotimes (_ (1- num-right))
        (split-window-vertically)
        (other-window 1))))

  ;; Return to first window
  (other-window (- (window-count))))

;; 3. Window setup helpers
;; ----------------------------------------

(defun --etm-layout-setup-window-with-host
    (tab-name n window-type path host)
  "Setup window N with WINDOW-TYPE ('file or 'shell) at PATH with specified HOST."
  (let* ((is-remote (and host
                         (not (member host etm-localhost-names))
                         (not (string= host etm-ignored-host))))
         (effective-path
          (if is-remote
              (if (eq window-type 'file)
                  (format "/ssh:%s:%s"
                          host
                          (--etm-ssh-rename-username path host))
                (--etm-ssh-rename-username path host))
            path)))
    (cond
     ((eq window-type 'file)
      (find-file effective-path))
     ((eq window-type 'shell)
      (--etm-vterm-new (format "%s-%02d" tab-name n))
      (let ((shell-buffer (current-buffer)))
        ;; Connect to remote host if needed
        (when is-remote
          (vterm-send-string
           (format "ssh -Y %s\n" host)))
        ;; Change directory immediately for local or after a delay for remote
        (unless is-remote
          (vterm-send-string
           (format "cd %s && clear\n" effective-path)))
        shell-buffer effective-path)))))

;; 4. Advanced layout creation
;; ----------------------------------------

(defun --etm-layout-initialize-tab (tab-name host)
  "Initialize tab with name TAB-NAME and optional HOST.
Returns the selected host for this tab."
  (etm-new tab-name)
  
  ;; Ensure fullscreen mode
  (unless (frame-parameter nil 'fullscreen)
    (toggle-frame-fullscreen))
  (sit-for 0.3)
  
  ;; Save the host as default if provided explicitly
  (when host
    (puthash tab-name host etm-layout-default-hosts))
  
  ;; Get or create SSH connection if host is specified
  (when (and host 
             (not (member host etm-localhost-names))
             (not (string= host etm-ignored-host)))
    (let ((connection-id (--etm-get-or-create-ssh-connection host)))
      ;; Store this connection for this tab
      (--etm-register-ssh-connection tab-name host connection-id)))
  
  ;; Return the selected host
  (let ((selected-host (or host
                           (gethash tab-name etm-layout-default-hosts)
                           (--etm-ssh-select-host))))
    ;; Create and register SSH connection for interactively selected host
    (when (and (not host) ; Only if host wasn't provided explicitly
               selected-host
               (not (member selected-host etm-localhost-names))
               (not (string= selected-host etm-ignored-host)))
      (let ((connection-id (--etm-get-or-create-ssh-connection selected-host)))
        (--etm-register-ssh-connection tab-name selected-host connection-id)))
    selected-host))

(defun --etm-layout-create-window-structure (window-specs main-window)
  "Create window structure based on WINDOW-SPECS starting from MAIN-WINDOW."
  (let ((windows (list)))
    ;; Start with one window
    (push main-window windows)
    ;; First create all horizontal splits (columns)
    (let ((column-positions
           (seq-uniq
            (mapcar (lambda (spec) (nth 2 spec)) window-specs))))
      (dolist (x-pos (cdr column-positions))
        (select-window main-window)
        (push (split-window-horizontally) windows)))
    ;; Then for each column, create the vertical splits
    (dolist (window windows)
      (let* ((window-edges (window-edges window))
             (x-pos (nth 0 window-edges))
             (rows-in-column (seq-filter
                              (lambda (spec) (= (nth 2 spec) x-pos))
                              window-specs))
             (row-positions
              (seq-uniq
               (mapcar (lambda (spec) (nth 3 spec)) rows-in-column))))
        (when (> (length row-positions) 1)
          (select-window window)
          (dolist (y-pos (cdr row-positions))
            (split-window-vertically)))))))

(defun --etm-layout-map-windows-to-positions ()
  "Create a hash table mapping window positions to windows."
  (let ((windows-by-position (make-hash-table :test 'equal)))
    (dolist (window (window-list))
      (let* ((edges (window-edges window))
             (x (nth 0 edges))
             (y (nth 1 edges)))
        (puthash (cons x y) window windows-by-position)))
    windows-by-position))

(defun --etm-layout-setup-file-window (path x y selected-host path-host)
  "Setup a file window at position X Y with PATH.
SELECTED-HOST is the default host, PATH-HOST is the specific host for this window."
  (let* ((effective-host (or path-host selected-host))
         (is-remote (and effective-host
                         (not (member effective-host etm-localhost-names))
                         (not (string= effective-host etm-ignored-host))))
         (effective-path
          (if is-remote
              (format "/ssh:%s:%s"
                      effective-host
                      (--etm-ssh-rename-username path effective-host))
            path)))
    (find-file effective-path)
    ;; Mark home or semi-home
    (if (and (= x 0) (= y 0))
        (etm-buffer-set "home")
      (etm-buffer-set "semi-home"))))

(defun --etm-layout-setup-shell-window (tab-name path x y selected-host path-host window-index)
  "Setup a shell window at position X Y with PATH.
TAB-NAME is the current tab, SELECTED-HOST is the default host, 
PATH-HOST is the specific host for this window, WINDOW-INDEX is for unique naming."
  (let* ((buffer-name (format "%s-%s-%02d-%02d"
                              tab-name
                              (format-time-string "%d:%S")
                              x y))
         (effective-host (or path-host selected-host))
         (is-remote (and effective-host
                         (not (member effective-host etm-localhost-names))
                         (not (string= effective-host etm-ignored-host))))
         (effective-path (if is-remote
                             (--etm-ssh-rename-username path effective-host)
                           path)))
    ;; Create vterm
    (--etm-vterm-new (format "term-%d" window-index))
    
    ;; Connect to remote host if needed
    (when is-remote
      ;; Check if we have a connection registered for this tab
      (let* ((connection-info (--etm-get-tab-ssh-connection tab-name))
             (connection-host (car-safe connection-info))
             (connection-id (cdr-safe connection-info)))
        
        ;; If we have a connection and it's for the same host, use ControlPath
        (if (and connection-info (string= connection-host effective-host))
            (vterm-send-string
             (format "ssh -o ControlPath=~/.ssh/%s %s\n" 
                     connection-id effective-host))
          ;; Otherwise create a regular connection
          (vterm-send-string
           (format "ssh -Y %s\n" effective-host))))
      
      (sit-for 0.3)
      (vterm-send-string
       (format "cd %s && clear \n" effective-path)))
    
    ;; For local paths:
    (unless is-remote
      (vterm-send-string
       (format "cd %s && clear\n" effective-path)))
    
    ;; Apply semi-home mark if applicable
    (when (and (= x 0) (= y 0))
      (etm-buffer-set "semi-home"))
    
    ;; Rename buffer with host info if remote
    (if is-remote
        (rename-buffer (format "%s@%s:%s"
                               buffer-name
                               effective-host
                               (file-name-nondirectory effective-path)))
      (rename-buffer buffer-name))))

(defun --etm-layout-create-from-positions
    (tab-name window-specs &optional host)
  "Create tab layout based on window positions with proper connection reuse.
WINDOW-SPECS is a list of (type path x y width height [path-host]) for each window."
  ;; Initialize tab and get selected host
  (let ((selected-host (--etm-layout-initialize-tab tab-name host))
        (main-window (selected-window))
        (windows (list)))
    ;; Create window structure
    (--etm-layout-create-window-structure window-specs main-window)
    ;; Map windows to their positions
    (let ((windows-by-position (--etm-layout-map-windows-to-positions))
          (window-index 0))
      ;; Now set up each window
      (dolist (spec window-specs)
        (let* ((type (nth 0 spec))
               (path (nth 1 spec))
               (x (nth 2 spec))
               (y (nth 3 spec))
               (path-host (and (> (length spec) 6) (nth 6 spec)))
               (window (gethash (cons x y) windows-by-position)))
          (when window
            (select-window window)
            ;; Handle different window types
            (cond
             ((eq type 'file)
              (--etm-layout-setup-file-window path x y selected-host path-host))
             ((eq type 'shell)
              (--etm-layout-setup-shell-window tab-name path x y selected-host path-host window-index)
              (cl-incf window-index)))))
        ;; Clean up
        (--etm-layout-cleanup-default-buffers)
        (select-window (frame-first-window))))))


(provide 'etm-layout-create)

(when
    (not load-file-name)
  (message "etm-layout-create.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))