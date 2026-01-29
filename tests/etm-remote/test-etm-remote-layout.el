;;; test-etm-remote-layout.el --- Tests for ETM remote layout integration -*- coding: utf-8; lexical-binding: t -*-

;; Author: Yuki Watanabe
;; Date: 2025-01-13
;; Version: 1.0.0

;;; Commentary:
;; Test suite for ETM remote layout integration functionality.
;; Tests layout save/load with remote connections, offline mode, and migration.

;;; Code:

(add-to-list 'load-path (expand-file-name "../.." (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../etm-core" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../etm-layout" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../etm-remote" (file-name-directory load-file-name)))

(require 'ert)
(require 'cl-lib)
(require 'etm-core-variables)
(require 'etm-layout-save)
(require 'etm-layout-open)
(require 'etm-remote-connection)

;; Mock variables for testing
(defvar test-etm-remote-layout-saved-content nil
  "Content saved during test.")

(defvar test-etm-remote-layout-offline-mode nil
  "Whether to simulate offline mode.")

(defvar test-etm-remote-layout-connections nil
  "Mock connection list.")

(defun test-etm-remote-layout-setup ()
  "Set up test environment."
  ;; Initialize test variables
  (setq test-etm-remote-layout-saved-content nil)
  (setq test-etm-remote-layout-offline-mode nil)
  (setq test-etm-remote-layout-connections '())
  (setq etm-remote-connections (make-hash-table :test 'equal))
  (setq etm-remote-global-connections (make-hash-table :test 'equal))
  (puthash "test-tab" (make-hash-table :test 'equal) etm-remote-connections)
  
  ;; Mock functions
  (fset 'tab-bar--current-tab
        (lambda () '((name . "test-tab"))))
  
  ;; Mock file operations
  (fset 'find-file-noselect
        (lambda (filename)
          (get-buffer-create "*test-layout-buffer*")))
  
  (fset 'with-current-buffer
        (lambda (buffer &rest body)
          (eval `(progn ,@body))))
  
  ;; Mock save operations
  (advice-add 'insert :override
              (lambda (content)
                (setq test-etm-remote-layout-saved-content
                      (concat test-etm-remote-layout-saved-content content))))
  
  ;; Mock user input functions
  (fset 'read-string (lambda (_prompt) ""))
  
  ;; Mock layout capture to avoid complex window operations
  (fset '--etm-layout-capture-current-layout
        (lambda (_name _host)
          "(--etm-layout-create-from-positions \"test\" '() nil)"))
  
  ;; Mock file save operations
  (fset 'save-buffer (lambda () nil))
  (fset 'kill-buffer (lambda (&optional _buffer) nil))
  (fset 'load-file (lambda (_file) nil))
  (fset 'erase-buffer (lambda () nil))
  
  ;; Mock save-current-buffer properly
  (defmacro save-current-buffer (&rest body)
    `(progn ,@body)))

(defun test-etm-remote-layout-teardown ()
  "Tear down test environment."
  (advice-remove 'insert (lambda (content)
                           (setq test-etm-remote-layout-saved-content
                                 (concat test-etm-remote-layout-saved-content content)))))

(ert-deftest test-etm-remote-layout-functions-exist ()
  "Test that remote layout functions are defined."
  (test-etm-remote-layout-setup)
  (unwind-protect
      (progn
        (require 'etm-remote-layout)
        (should (fboundp 'etm-remote-layout-save-connections))
        (should (fboundp 'etm-remote-layout-restore-connections))
        (should (fboundp 'etm-remote-layout-offline-mode))
        (should (fboundp 'etm-remote-layout-migrate)))
    (test-etm-remote-layout-teardown)))

(ert-deftest test-etm-remote-layout-save-with-connections ()
  "Test saving layout with remote connection information."
  (test-etm-remote-layout-setup)
  (unwind-protect
      (progn
        (require 'etm-remote-layout)
        
        ;; Create some remote connections
        (let ((conn1 (etm-remote-connect "ssh" "user" "host1.com"))
              (conn2 (etm-remote-connect "ssh" "user" "host2.com")))
          (setf (etm-remote-connection-status conn1) :connected)
          (setf (etm-remote-connection-status conn2) :connected)
          
          ;; Save layout with connections
          (let ((saved-data (etm-remote-layout-save-connections)))
            (should saved-data)
            (should (= (length saved-data) 2))
            (should (cl-some (lambda (conn)
                               (string= (plist-get conn :host) "host1.com"))
                             saved-data))
            (should (cl-some (lambda (conn)
                               (string= (plist-get conn :host) "host2.com"))
                             saved-data)))))
    (test-etm-remote-layout-teardown)))

(ert-deftest test-etm-remote-layout-restore-connections ()
  "Test restoring connections when loading a layout."
  (test-etm-remote-layout-setup)
  (unwind-protect
      (progn
        (require 'etm-remote-layout)
        
        ;; Mock connection data
        (let ((connection-data
               '((:method "ssh" :user "user" :host "host1.com" :port nil)
                 (:method "ssh" :user "user" :host "host2.com" :port 2222))))
          
          ;; Restore connections
          (etm-remote-layout-restore-connections connection-data)
          
          ;; Check connections were created
          (let ((tab-connections (etm-remote--get-tab-connections)))
            (should (gethash "host1.com" tab-connections))
            (should (gethash "host2.com" tab-connections))
            
            ;; Check connection properties
            (let ((conn1 (gethash "host1.com" tab-connections)))
              (should (equal (etm-remote-connection-method conn1) "ssh"))
              (should (equal (etm-remote-connection-user conn1) "user"))))))
    (test-etm-remote-layout-teardown)))

(ert-deftest test-etm-remote-layout-offline-mode ()
  "Test offline mode behavior."
  (test-etm-remote-layout-setup)
  (unwind-protect
      (progn
        (require 'etm-remote-layout)
        
        ;; Enable offline mode
        (etm-remote-layout-offline-mode t)
        (should etm-remote-layout-offline-enabled)
        
        ;; Test that connections are not attempted in offline mode
        (let ((connection-data
               '((:method "ssh" :user "user" :host "remote.com" :port nil))))
          
          ;; This should not create actual connections
          (etm-remote-layout-restore-connections connection-data)
          
          ;; Connections should be marked as offline
          (let* ((tab-connections (etm-remote--get-tab-connections))
                 (conn (gethash "remote.com" tab-connections)))
            (should conn)
            (should (eq (etm-remote-connection-status conn) :offline)))))
    (test-etm-remote-layout-teardown)))

(ert-deftest test-etm-remote-layout-migration ()
  "Test migration of old layout formats."
  (test-etm-remote-layout-setup)
  (unwind-protect
      (progn
        (require 'etm-remote-layout)
        
        ;; Test old format detection
        (let ((old-layout-content
               "(--etm-layout-create-from-positions \"test\"
  '((file \"/home/user/file.txt\" 0 0 80 40 nil)
    (shell \"/ssh:user@host1.com:/home/user\" 80 0 80 40 nil))
  nil)"))
          
          (should (etm-remote-layout-needs-migration-p old-layout-content))
          
          ;; Test migration
          (let ((migrated (etm-remote-layout-migrate old-layout-content)))
            (should migrated)
            (should (string-match "etm-remote-layout-connections" migrated)))))
    (test-etm-remote-layout-teardown)))

(ert-deftest test-etm-remote-layout-integration ()
  "Test full integration with layout save/load."
  (test-etm-remote-layout-setup)
  (unwind-protect
      (progn
        (require 'etm-remote-layout)
        
        ;; Create a connection
        (let ((conn (etm-remote-connect "ssh" "user" "test.com")))
          (setf (etm-remote-connection-status conn) :connected)
          
          ;; Test connection formatting
          (let ((formatted (etm-remote-layout-format-connections
                            (etm-remote-layout-save-connections))))
            (should formatted)
            (should (string-match "etm-remote-layout-connections" formatted))
            (should (string-match "test.com" formatted)))))
    (test-etm-remote-layout-teardown)))

;; Run tests if executed directly
;; (when (and (boundp 'load-file-name) load-file-name)
;;   (ert-run-tests-batch-and-exit))

(provide 'test-etm-remote-layout)
;;; test-etm-remote-layout.el ends here

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-remote/etm-remote-layout.el
;; --------------------------------------------------------------------------------
;; ;;; etm-remote-layout.el --- Layout integration for ETM remote support -*- coding: utf-8; lexical-binding: t -*-
;; 
;; ;; Author: Yuki Watanabe
;; ;; Date: 2025-01-13
;; ;; Version: 1.0.0
;; 
;; ;;; Commentary:
;; ;; This module extends ETM's layout save/load functionality to properly
;; ;; handle remote connections. It adds connection state persistence,
;; ;; offline mode support, and migration from older layout formats.
;; 
;; ;;; Code:
;; 
;; (require 'cl-lib)
;; (require 'etm-remote-connection)
;; (require 'etm-layout-save)
;; (require 'etm-layout-open)
;; 
;; (defcustom etm-remote-layout-save-connections t
;;   "Whether to save remote connection information with layouts."
;;   :type 'boolean
;;   :group 'etm-remote)
;; 
;; (defcustom etm-remote-layout-restore-connections t
;;   "Whether to restore remote connections when loading layouts."
;;   :type 'boolean
;;   :group 'etm-remote)
;; 
;; (defvar etm-remote-layout-offline-enabled nil
;;   "When non-nil, skip remote connection attempts during layout loading.")
;; 
;; (defvar etm-remote-layout-version "1.0"
;;   "Version of the remote layout format.")
;; 
;; (defun etm-remote-layout-save-connections ()
;;   "Extract and return connection information for the current tab."
;;   (when etm-remote-layout-save-connections
;;     (let ((connections '())
;;           (tab-connections (etm-remote--get-tab-connections)))
;;       (maphash
;;        (lambda (host conn)
;;          (when (memq (etm-remote-connection-status conn) '(:connected :connecting))
;;            (push (list :method (etm-remote-connection-method conn)
;;                        :user (etm-remote-connection-user conn)
;;                        :host (etm-remote-connection-host conn)
;;                        :port (etm-remote-connection-port conn))
;;                  connections)))
;;        tab-connections)
;;       connections)))
;; 
;; (defun etm-remote-layout-restore-connections (connection-data)
;;   "Restore connections from CONNECTION-DATA."
;;   (when (and etm-remote-layout-restore-connections
;;              connection-data)
;;     (dolist (conn-info connection-data)
;;       (let ((method (plist-get conn-info :method))
;;             (user (plist-get conn-info :user))
;;             (host (plist-get conn-info :host))
;;             (port (plist-get conn-info :port)))
;;         (condition-case err
;;             (let ((conn (etm-remote-connect method user host port)))
;;               (when etm-remote-layout-offline-enabled
;;                 ;; Mark as offline if in offline mode
;;                 (setf (etm-remote-connection-status conn) :offline)))
;;           (error
;;            (message "Failed to restore connection to %s: %s" host (error-message-string err))))))))
;; 
;; (defun etm-remote-layout-offline-mode (&optional enable)
;;   "Toggle offline mode for layout loading.
;; When ENABLE is non-nil, enable offline mode."
;;   (interactive "P")
;;   (setq etm-remote-layout-offline-enabled
;;         (if enable
;;             (not (null enable))
;;           (not etm-remote-layout-offline-enabled)))
;;   (message "Remote layout offline mode %s"
;;            (if etm-remote-layout-offline-enabled "enabled" "disabled")))
;; 
;; (defun etm-remote-layout-format-connections (connections)
;;   "Format CONNECTIONS data for inclusion in layout file."
;;   (when connections
;;     (format "\n;; Remote connections\n(setq etm-remote-layout-connections '%S)\n"
;;             connections)))
;; 
;; (defun etm-remote-layout-save-advice (orig-fun layout-name)
;;   "Advice for `etm-layout-save' to include connection information."
;;   (let ((result (funcall orig-fun layout-name)))
;;     ;; After the original save, append connection data
;;     (when etm-remote-layout-save-connections
;;       (let* ((connections (etm-remote-layout-save-connections))
;;              (full-path (expand-file-name
;;                          (format "etm-open-%s.el" layout-name)
;;                          etm-layout-save-dir)))
;;         (when connections
;;           (with-current-buffer (find-file-noselect full-path)
;;             (goto-char (point-max))
;;             (insert (etm-remote-layout-format-connections connections))
;;             (insert "\n;; Restore connections when layout is loaded\n")
;;             (insert "(when (bound-and-true-p etm-remote-layout-connections)\n")
;;             (insert "  (require 'etm-remote-layout)\n")
;;             (insert "  (etm-remote-layout-restore-connections etm-remote-layout-connections))\n")
;;             (save-buffer)
;;             (kill-buffer)))))
;;     result))
;; 
;; (defun etm-remote-layout-needs-migration-p (layout-content)
;;   "Check if LAYOUT-CONTENT needs migration to new format."
;;   (and (string-match "/ssh:" layout-content)
;;        (not (string-match "etm-remote-layout-connections" layout-content))))
;; 
;; (defun etm-remote-layout-migrate (layout-content)
;;   "Migrate old LAYOUT-CONTENT to include connection information."
;;   (when (etm-remote-layout-needs-migration-p layout-content)
;;     ;; Extract SSH paths from the layout
;;     (let ((connections '())
;;           (start 0))
;;       (while (string-match "/ssh:\\([^@]+\\)@\\([^:/]+\\)\\(?::\\([0-9]+\\)\\)?:" 
;;                            layout-content start)
;;         (let ((user (match-string 1 layout-content))
;;               (host (match-string 2 layout-content))
;;               (port (match-string 3 layout-content)))
;;           (cl-pushnew (list :method "ssh"
;;                             :user user
;;                             :host host
;;                             :port (when port (string-to-number port)))
;;                       connections
;;                       :test (lambda (a b)
;;                               (and (equal (plist-get a :host) (plist-get b :host))
;;                                    (equal (plist-get a :user) (plist-get b :user))))))
;;         (setq start (match-end 0)))
;;       
;;       ;; Add connection data to layout
;;       (when connections
;;         (concat layout-content
;;                 "\n\n;; Migrated remote connections\n"
;;                 (format "(setq etm-remote-layout-connections '%S)\n" connections)
;;                 "(when (bound-and-true-p etm-remote-layout-connections)\n"
;;                 "  (require 'etm-remote-layout)\n"
;;                 "  (etm-remote-layout-restore-connections etm-remote-layout-connections))\n")))))
;; 
;; (defun etm-remote-layout-migrate-file (layout-name)
;;   "Migrate layout file LAYOUT-NAME to new format."
;;   (interactive
;;    (list (completing-read "Layout to migrate: "
;;                           (etm-layout-list-available)
;;                           nil t)))
;;   (let* ((file-path (etm-layout-file-path layout-name))
;;          (content (with-temp-buffer
;;                     (insert-file-contents file-path)
;;                     (buffer-string)))
;;          (migrated (etm-remote-layout-migrate content)))
;;     (if migrated
;;         (progn
;;           ;; Backup original
;;           (copy-file file-path (concat file-path ".bak") t)
;;           ;; Write migrated content
;;           (with-temp-file file-path
;;             (insert migrated))
;;           (message "Layout '%s' migrated successfully" layout-name))
;;       (message "Layout '%s' does not need migration" layout-name))))
;; 
;; (defun etm-remote-layout-init ()
;;   "Initialize remote layout integration."
;;   ;; Install advice for layout save
;;   (advice-add 'etm-layout-save :around #'etm-remote-layout-save-advice)
;;   
;;   ;; Add hook for layout loading
;;   (add-hook 'etm-layout-load-hook #'etm-remote-layout-check-connections))
;; 
;; (defun etm-remote-layout-cleanup ()
;;   "Clean up remote layout integration."
;;   (advice-remove 'etm-layout-save #'etm-remote-layout-save-advice)
;;   (remove-hook 'etm-layout-load-hook #'etm-remote-layout-check-connections))
;; 
;; (defun etm-remote-layout-check-connections ()
;;   "Check and restore connections after layout load."
;;   (when (and (boundp 'etm-remote-layout-connections)
;;              etm-remote-layout-connections)
;;     (etm-remote-layout-restore-connections etm-remote-layout-connections)
;;     ;; Clear the variable to avoid re-processing
;;     (setq etm-remote-layout-connections nil)))
;; 
;; ;; Offline mode command
;; (defun etm-remote-layout-open-offline (layout-name)
;;   "Open LAYOUT-NAME in offline mode, skipping remote connections."
;;   (interactive
;;    (list (completing-read "Layout (offline): "
;;                           (etm-layout-list-available)
;;                           nil t)))
;;   (let ((etm-remote-layout-offline-enabled t))
;;     (etm-layout-open layout-name)))
;; 
;; (provide 'etm-remote-layout)
;; ;;; etm-remote-layout.el ends here
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-remote/etm-remote-layout.el
;; --------------------------------------------------------------------------------

;;; test-etm-remote-layout.el ends here
