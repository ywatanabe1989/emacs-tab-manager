;;; test-etm-remote-navigation.el --- Tests for ETM remote navigation -*- coding: utf-8; lexical-binding: t -*-

;; Author: Yuki Watanabe
;; Date: 2025-01-13
;; Version: 1.0.0

;;; Commentary:
;; Test suite for ETM remote navigation functionality.
;; Tests remote-aware buffer navigation and host selection.

;;; Code:

(add-to-list 'load-path (expand-file-name "../.." (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../etm-core" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../etm-buffer" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../etm-remote" (file-name-directory load-file-name)))

(require 'ert)
(require 'cl-lib)
(require 'etm-core-variables)
(require 'etm-buffer)
(require 'etm-remote-connection)

;; Mock functions for testing
(defvar test-etm-remote-nav-buffers nil
  "Mock buffer list for testing.")

(defvar test-etm-remote-nav-current-buffer nil
  "Mock current buffer for testing.")

(defun test-etm-remote-navigation-setup ()
  "Set up test environment."
  ;; Initialize test variables
  (setq test-etm-remote-nav-buffers '())
  (setq test-etm-remote-nav-current-buffer nil)
  (setq etm-remote-connections (make-hash-table :test 'equal))
  (setq etm-remote-global-connections (make-hash-table :test 'equal))
  (puthash "test-tab" (make-hash-table :test 'equal) etm-remote-connections)
  
  ;; Mock tab-bar functions
  (fset 'tab-bar--current-tab
        (lambda () '((name . "test-tab"))))
  
  ;; Mock buffer functions
  (fset 'buffer-list
        (lambda () test-etm-remote-nav-buffers))
  
  (fset 'current-buffer
        (lambda () test-etm-remote-nav-current-buffer))
  
  (fset 'switch-to-buffer
        (lambda (buffer)
          (setq test-etm-remote-nav-current-buffer buffer)))
  
  ;; Mock file-remote-p to work with our test buffers
  (fset 'file-remote-p
        (lambda (file &optional identification)
          (when file
            (cond
             ((string-match "^/ssh:user@\\([^:]+\\):" file)
              (if (eq identification 'host)
                  (match-string 1 file)
                (match-string 0 file)))
             (t nil)))))
  
  ;; Mock completing-read to return first choice
  (fset 'completing-read
        (lambda (prompt choices &rest _)
          (if (consp (car choices))
              (caar choices)
            (car choices))))
  
  ;; Initialize buffer-files association list
  (setq test-etm-remote-nav-buffer-files nil))

(defvar test-etm-remote-nav-buffer-files nil
  "Alist mapping buffers to their file names.")

(defun test-etm-remote-navigation-teardown ()
  "Tear down test environment."
  (setq test-etm-remote-nav-buffer-files nil))

(defun test-etm-remote-nav-create-mock-buffer (name file-name)
  "Create a mock buffer with NAME and FILE-NAME."
  (let ((buffer (get-buffer-create name)))
    ;; Store the buffer-file association
    (push (cons buffer file-name) test-etm-remote-nav-buffer-files)
    ;; Also set buffer-file-name in the actual buffer
    (with-current-buffer buffer
      (setq buffer-file-name file-name))
    buffer))

(ert-deftest test-etm-remote-navigation-functions-exist ()
  "Test that remote navigation functions are defined."
  (test-etm-remote-navigation-setup)
  (unwind-protect
      (progn
        (require 'etm-remote-navigation)
        (should (fboundp 'etm-remote-jump-to-host))
        (should (fboundp 'etm-remote-next-buffer))
        (should (fboundp 'etm-remote-prev-buffer))
        (should (fboundp 'etm-remote-list-buffers))
        (should (fboundp 'etm-remote-switch-to-local)))
    (test-etm-remote-navigation-teardown)))

(ert-deftest test-etm-remote-jump-to-host ()
  "Test jumping to a specific remote host."
  (test-etm-remote-navigation-setup)
  (unwind-protect
      (progn
        (require 'etm-remote-navigation)
        
        ;; Create test buffers
        (let ((local-buf (test-etm-remote-nav-create-mock-buffer 
                          "local.txt" "/home/user/local.txt"))
              (remote1-buf (test-etm-remote-nav-create-mock-buffer 
                            "remote1.txt" "/ssh:user@host1.com:/home/user/remote1.txt"))
              (remote2-buf (test-etm-remote-nav-create-mock-buffer 
                            "remote2.txt" "/ssh:user@host2.com:/home/user/remote2.txt")))
          
          (setq test-etm-remote-nav-buffers (list local-buf remote1-buf remote2-buf))
          (setq test-etm-remote-nav-current-buffer local-buf)
          
          ;; Test jumping to host1
          (etm-remote-jump-to-host "host1.com")
          (should (eq test-etm-remote-nav-current-buffer remote1-buf))
          
          ;; Test jumping to host2
          (etm-remote-jump-to-host "host2.com")
          (should (eq test-etm-remote-nav-current-buffer remote2-buf))))
    (test-etm-remote-navigation-teardown)))

(ert-deftest test-etm-remote-next-prev-buffer ()
  "Test cycling through remote buffers."
  (test-etm-remote-navigation-setup)
  (unwind-protect
      (progn
        (require 'etm-remote-navigation)
        
        ;; Create test buffers
        (let ((local-buf (test-etm-remote-nav-create-mock-buffer 
                          "local.txt" "/home/user/local.txt"))
              (remote1-buf (test-etm-remote-nav-create-mock-buffer 
                            "remote1.txt" "/ssh:user@host1.com:/home/user/remote1.txt"))
              (remote2-buf (test-etm-remote-nav-create-mock-buffer 
                            "remote2.txt" "/ssh:user@host1.com:/home/user/remote2.txt")))
          
          (setq test-etm-remote-nav-buffers (list local-buf remote1-buf remote2-buf))
          (setq test-etm-remote-nav-current-buffer remote1-buf)
          
          ;; Test next remote buffer
          (etm-remote-next-buffer)
          (should (eq test-etm-remote-nav-current-buffer remote2-buf))
          
          ;; Test wrap around
          (etm-remote-next-buffer)
          (should (eq test-etm-remote-nav-current-buffer remote1-buf))
          
          ;; Test previous remote buffer
          (etm-remote-prev-buffer)
          (should (eq test-etm-remote-nav-current-buffer remote2-buf))))
    (test-etm-remote-navigation-teardown)))

(ert-deftest test-etm-remote-list-buffers ()
  "Test listing remote buffers by host."
  (test-etm-remote-navigation-setup)
  (unwind-protect
      (progn
        (require 'etm-remote-navigation)
        
        ;; Create test buffers
        (let ((remote1-buf (test-etm-remote-nav-create-mock-buffer 
                            "remote1.txt" "/ssh:user@host1.com:/home/user/remote1.txt"))
              (remote2-buf (test-etm-remote-nav-create-mock-buffer 
                            "remote2.txt" "/ssh:user@host1.com:/home/user/remote2.txt"))
              (remote3-buf (test-etm-remote-nav-create-mock-buffer 
                            "remote3.txt" "/ssh:user@host2.com:/home/user/remote3.txt")))
          
          (setq test-etm-remote-nav-buffers (list remote1-buf remote2-buf remote3-buf))
          
          ;; Test listing buffers for host1
          (let ((host1-buffers (etm-remote-list-buffers "host1.com")))
            (should (= (length host1-buffers) 2))
            (should (memq remote1-buf host1-buffers))
            (should (memq remote2-buf host1-buffers)))
          
          ;; Test listing buffers for host2
          (let ((host2-buffers (etm-remote-list-buffers "host2.com")))
            (should (= (length host2-buffers) 1))
            (should (memq remote3-buf host2-buffers)))))
    (test-etm-remote-navigation-teardown)))

(ert-deftest test-etm-remote-switch-to-local ()
  "Test switching from remote to local buffer."
  (test-etm-remote-navigation-setup)
  (unwind-protect
      (progn
        (require 'etm-remote-navigation)
        
        ;; Create test buffers
        (let ((local-buf (test-etm-remote-nav-create-mock-buffer 
                          "local.txt" "/home/user/local.txt"))
              (remote-buf (test-etm-remote-nav-create-mock-buffer 
                           "remote.txt" "/ssh:user@host1.com:/home/user/remote.txt")))
          
          (setq test-etm-remote-nav-buffers (list local-buf remote-buf))
          (setq test-etm-remote-nav-current-buffer remote-buf)
          
          ;; Test switching to local
          (etm-remote-switch-to-local)
          (should (eq test-etm-remote-nav-current-buffer local-buf))))
    (test-etm-remote-navigation-teardown)))

;; Run tests if executed directly
;; (when (and (boundp 'load-file-name) load-file-name)
;;   (ert-run-tests-batch-and-exit))

(provide 'test-etm-remote-navigation)
;;; test-etm-remote-navigation.el ends here