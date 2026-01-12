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

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-close/etm-close-utils.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-05-09 19:45:10>
;; ;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-close/etm-close-utils.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; (defun etm-close-by-id (tab-id)
;;   "Close the tab with ID TAB-ID."
;;   (interactive "nTab ID to close: ")
;;   (tab-bar-close-tab (- tab-id 1)))
;; 
;; (defun etm-close-1 ()
;;   "Close tab with index 1."
;;   (interactive)
;;   (tab-bar-select-tab 1)
;;   (tab-close))
;; 
;; (defun etm-close-and-next ()
;;   "Close the current tab and move to the next one."
;;   (interactive)
;;   (let ((current-name (alist-get 'name (tab-bar--current-tab))))
;;     (tab-bar-close-tab)  ;; Use tab-bar-close-tab instead of tab-close
;;     (tab-next)))
;; 
;; (defun etm-close-by-name-and-prev ()
;;   "Close the current tab and move to the previous one."
;;   (interactive)
;;   (let* ((tabs (tab-bar-tabs))
;;         (current-index (tab-bar--current-tab-index))
;;         (prev-tab-index (1- current-index)))
;;     (tab-close)
;;     (when (and (>= prev-tab-index 0) 
;;                (< prev-tab-index (length (tab-bar-tabs))))
;;       (tab-bar-select-tab (1+ prev-tab-index)))))
;; 
;; (defun etm-close-others ()
;;   "Close all tabs except the current one."
;;   (interactive)
;;   (let ((current-tab (tab-bar--current-tab))
;;         (tabs (tab-bar-tabs))
;;         (tab-count (length (tab-bar-tabs))))
;;     (when (> tab-count 1)  ;; Only proceed if there's more than one tab
;;       (let ((tabs-to-close (delq current-tab (copy-sequence tabs))))
;;         (dolist (tab tabs-to-close)
;;           (condition-case nil
;;               (tab-bar-close-tab-by-name (alist-get 'name tab))
;;             (user-error nil))))
;;       (message "All other tabs closed."))))
;; 
;; (provide 'etm-close-utils)
;; 
;; (when (not load-file-name)
;;   (message "etm-close-utils.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; 
;; ;;; etm-close-utils.el ends here
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-close/etm-close-utils.el
;; --------------------------------------------------------------------------------

;;; test-etm-close-utils.el ends here
