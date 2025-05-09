;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-close-utils.el

(require 'ert)
(require 'etm-close-utils)

(ert-deftest test-etm-close-by-id
    ()
  (tab-bar-mode 1)
  (tab-bar-new-tab)
  (let
      ((num-tabs
        (length
         (tab-bar-tabs))))
    (etm-close-by-id 2)
    (should
     (=
      (length
       (tab-bar-tabs))
      (1- num-tabs)))))

(ert-deftest test-etm-close-and-next
    ()
  (tab-bar-mode 1)
  ;; Make sure to start with a clean state
  (let ((starting-tabs (tab-bar-tabs)))
    (when (> (length starting-tabs) 1)
      (dolist (tab (cdr starting-tabs))
        (tab-bar-close-tab-by-name (alist-get 'name tab)))))
  
  ;; Create new tabs for testing
  (tab-bar-new-tab)
  (tab-bar-rename-tab "tab1")
  (tab-bar-new-tab)
  (tab-bar-rename-tab "test-tab")
  (tab-bar-new-tab)
  (tab-bar-rename-tab "tab2")
  
  ;; Remember the tab we want to close
  (let ((tab-to-close "test-tab"))
    ;; Select the tab we want to close
    (tab-bar-select-tab-by-name tab-to-close)
    ;; Close it and move to next
    (etm-close-and-next)
    ;; Verify it's gone
    (should-not
     (member tab-to-close
             (mapcar (lambda (tab) (alist-get 'name tab))
                     (tab-bar-tabs))))))

(ert-deftest test-etm-close-by-name-and-prev
    ()
  (tab-bar-mode 1)
  (tab-bar-new-tab)
  (let
      ((prev-name
        (alist-get 'name
                   (tab-bar--current-tab))))
    (tab-bar-new-tab)
    (etm-close-by-name-and-prev)
    (should
     (string= prev-name
              (alist-get 'name
                         (tab-bar--current-tab))))))

(ert-deftest test-etm-close-others
    ()
  (skip-unless (fboundp 'tab-bar-mode))
  (tab-bar-mode 1)
  (unwind-protect
      (progn
        ;; Make sure we start with a clean slate - one tab
        (let ((initial-tabs (tab-bar-tabs)))
          (when (> (length initial-tabs) 1)
            (dolist (tab (cdr initial-tabs))
              (condition-case nil
                  (tab-bar-close-tab-by-name (alist-get 'name tab))
                (user-error nil)))))
        
        ;; Create multiple tabs for testing
        (tab-bar-new-tab)
        (tab-bar-rename-tab "tab1")
        (tab-bar-new-tab)
        (tab-bar-rename-tab "tab2")
        
        ;; Select the tab we want to keep
        (tab-bar-select-tab-by-name "tab2")
        (let ((current-tab (tab-bar--current-tab)))
          ;; Check we have more than one tab before proceeding
          (let ((initial-tab-count (length (tab-bar-tabs))))
            (should (> initial-tab-count 1))
            
            ;; Close all but the current tab
            (etm-close-others)
            
            ;; Verify we have just one tab left
            (should (= (length (tab-bar-tabs)) 1))
            
            ;; Verify that tab is the one we expected to keep
            (should (equal (alist-get 'name current-tab)
                           (alist-get 'name (tab-bar--current-tab)))))))
    ;; Clean up - ensure we have at least one tab left
    (when (= (length (tab-bar-tabs)) 0)
      (tab-bar-new-tab))))

(provide 'test-etm-close-utils)

(when
    (not load-file-name)
  (message "test-etm-close-utils.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))