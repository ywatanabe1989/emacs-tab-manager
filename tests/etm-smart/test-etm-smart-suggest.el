;;; test-etm-smart-suggest.el --- Tests for ETM smart suggestions -*- coding: utf-8; lexical-binding: t -*-

;; Author: ywatanabe
;; Date: 2025-05-26
;; Version: 1.0.0

;;; Commentary:
;; Test suite for ETM smart suggestion engine.
;; Tests suggestion generation, scoring, and filtering.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load paths for test environment
(add-to-list 'load-path (expand-file-name "../.." (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../etm-core" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../etm-smart" (file-name-directory load-file-name)))

;; Test helpers
(defvar test-etm-smart-suggest-patterns nil
  "Test pattern storage for suggestions.")

(defun test-etm-smart-suggest-setup ()
  "Set up test environment."
  (setq test-etm-smart-suggest-patterns (make-hash-table :test 'equal))
  (setq etm-smart-patterns test-etm-smart-suggest-patterns)
  
  ;; Mock buffer list
  (fset 'buffer-list
        (lambda () (list (get-buffer-create "main.el")
                         (get-buffer-create "test.el")
                         (get-buffer-create "utils.el")
                         (get-buffer-create "README.md")))))

(defun test-etm-smart-suggest-teardown ()
  "Tear down test environment."
  (setq test-etm-smart-suggest-patterns nil)
  (setq etm-smart-patterns nil)
  ;; Kill test buffers
  (dolist (buf '("main.el" "test.el" "utils.el" "README.md"))
    (when (get-buffer buf)
      (kill-buffer buf))))

;; Tests

(ert-deftest test-etm-smart-suggest-basic ()
  "Test basic suggestion generation."
  (test-etm-smart-suggest-setup)
  (unwind-protect
      (progn
        (require 'etm-smart-suggest)
        (require 'etm-smart-patterns)
        
        ;; Mock current buffer and tab
        (cl-letf (((symbol-function 'current-buffer)
                   (lambda () (get-buffer "main.el")))
                  ((symbol-function 'tab-bar--current-tab-name)
                   (lambda () "test-tab")))
          
          ;; Create some patterns
          (etm-smart-track-switch "main.el" "test.el")
          (etm-smart-track-switch "main.el" "test.el")
          (etm-smart-track-switch "main.el" "utils.el")
          (etm-smart-track-switch "test.el" "main.el")
          
          ;; Get suggestions from main.el
          (let ((suggestions (etm-smart-suggest-buffers 3)))
            (should suggestions)
            (should (<= (length suggestions) 3))
            
            ;; test.el should be suggested first (higher count)
            (let ((first-suggestion (car suggestions)))
              (should (equal (car first-suggestion) "test.el"))
              (should (> (cdr first-suggestion) 0.5)))
            
            ;; utils.el should also be suggested
            (should (member "utils.el" (mapcar #'car suggestions))))))
    (test-etm-smart-suggest-teardown)))

(ert-deftest test-etm-smart-suggest-with-context ()
  "Test context-aware suggestions."
  (test-etm-smart-suggest-setup)
  (unwind-protect
      (progn
        (require 'etm-smart-suggest)
        (require 'etm-smart-patterns)
        
        ;; Mock current context
        (cl-letf (((symbol-function 'current-buffer)
                   (lambda () (get-buffer "main.el")))
                  ((symbol-function 'tab-bar--current-tab-name)
                   (lambda () "test-tab"))
                  ((symbol-function 'project-current)
                   (lambda (&optional _) '(vc . "~/project-a"))))
          
          ;; Track patterns in different contexts
          ;; In project-a context
          (etm-smart-track-switch "main.el" "test.el")
          (etm-smart-track-switch "main.el" "test.el")
          
          ;; Change context
          (cl-letf (((symbol-function 'project-current)
                     (lambda (&optional _) '(vc . "~/project-b"))))
            ;; In project-b context
            (etm-smart-track-switch "main.el" "utils.el")
            (etm-smart-track-switch "main.el" "utils.el")
            (etm-smart-track-switch "main.el" "utils.el"))
          
          ;; Back in project-a context, test.el should be preferred
          (let ((suggestions (etm-smart-suggest-buffers 2)))
            (should (equal (caar suggestions) "test.el")))
          
          ;; In project-b context, utils.el should be preferred
          (cl-letf (((symbol-function 'project-current)
                     (lambda (&optional _) '(vc . "~/project-b"))))
            (let ((suggestions (etm-smart-suggest-buffers 2)))
              (should (equal (caar suggestions) "utils.el"))))))
    (test-etm-smart-suggest-teardown)))

(ert-deftest test-etm-smart-suggest-filtering ()
  "Test suggestion filtering and limits."
  (test-etm-smart-suggest-setup)
  (unwind-protect
      (progn
        (require 'etm-smart-suggest)
        (require 'etm-smart-patterns)
        
        (cl-letf (((symbol-function 'current-buffer)
                   (lambda () (get-buffer "main.el")))
                  ((symbol-function 'tab-bar--current-tab-name)
                   (lambda () "test-tab")))
          
          ;; Create many patterns
          (etm-smart-track-switch "main.el" "test.el")
          (etm-smart-track-switch "main.el" "utils.el")
          (etm-smart-track-switch "main.el" "README.md")
          
          ;; Test count limiting
          (let ((suggestions (etm-smart-suggest-buffers 2)))
            (should (<= (length suggestions) 2)))
          
          ;; Test minimum confidence filtering
          (let ((etm-smart-min-confidence 0.8))
            ;; With high threshold, might get fewer suggestions
            (let ((suggestions (etm-smart-suggest-buffers 5)))
              (dolist (suggestion suggestions)
                (should (>= (cdr suggestion) 0.8)))))
          
          ;; Test excluding current buffer
          (let ((suggestions (etm-smart-suggest-buffers 10)))
            (should-not (assoc "main.el" suggestions)))))
    (test-etm-smart-suggest-teardown)))

(ert-deftest test-etm-smart-suggest-no-patterns ()
  "Test suggestions when no patterns exist."
  (test-etm-smart-suggest-setup)
  (unwind-protect
      (progn
        (require 'etm-smart-suggest)
        
        (cl-letf (((symbol-function 'current-buffer)
                   (lambda () (get-buffer "main.el")))
                  ((symbol-function 'tab-bar--current-tab-name)
                   (lambda () "test-tab")))
          
          ;; No patterns tracked yet
          (let ((suggestions (etm-smart-suggest-buffers 5)))
            ;; Should still return some suggestions (fallback)
            (should suggestions)
            
            ;; Should suggest other open buffers
            (should (or (assoc "test.el" suggestions)
                        (assoc "utils.el" suggestions)
                        (assoc "README.md" suggestions))))))
    (test-etm-smart-suggest-teardown)))

(ert-deftest test-etm-smart-completing-read ()
  "Test enhanced completing-read with suggestions."
  (test-etm-smart-suggest-setup)
  (unwind-protect
      (progn
        (require 'etm-smart-suggest)
        (require 'etm-smart-patterns)
        
        ;; Mock user input
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (prompt collection &rest _)
                     ;; Return first choice
                     (if (consp collection)
                         (caar collection)
                       (car collection))))
                  ((symbol-function 'current-buffer)
                   (lambda () (get-buffer "main.el")))
                  ((symbol-function 'tab-bar--current-tab-name)
                   (lambda () "test-tab")))
          
          ;; Track some patterns
          (etm-smart-track-switch "main.el" "test.el")
          (etm-smart-track-switch "main.el" "test.el")
          
          ;; Test completing read
          (let ((selected (etm-smart-completing-read "Switch to buffer: ")))
            ;; Should select the suggested buffer
            (should (stringp selected))
            (should (or (equal selected "test.el")
                        (member selected '("utils.el" "README.md")))))))
    (test-etm-smart-suggest-teardown)))

(ert-deftest test-etm-smart-annotate-completion ()
  "Test completion annotation with suggestion metadata."
  (test-etm-smart-suggest-setup)
  (unwind-protect
      (progn
        (require 'etm-smart-suggest)
        (require 'etm-smart-patterns)
        
        (cl-letf (((symbol-function 'current-buffer)
                   (lambda () (get-buffer "main.el")))
                  ((symbol-function 'tab-bar--current-tab-name)
                   (lambda () "test-tab")))
          
          ;; Track patterns
          (etm-smart-track-switch "main.el" "test.el")
          (etm-smart-track-switch "main.el" "test.el")
          (etm-smart-track-switch "main.el" "test.el")
          
          ;; Get suggestions
          (etm-smart-suggest-buffers 5)
          
          ;; Test annotation
          (let ((annotation (etm-smart-annotate-completion "test.el")))
            (should (stringp annotation))
            ;; Should contain score or frequency info
            (should (or (string-match "%" annotation)
                        (string-match "frequent" annotation)
                        (string-match "[0-9]" annotation))))
          
          ;; Non-suggested buffer should have different annotation
          (let ((annotation (etm-smart-annotate-completion "some-other-buffer")))
            (should (or (null annotation)
                        (not (string-match "%" annotation)))))))
    (test-etm-smart-suggest-teardown)))

(ert-deftest test-etm-smart-suggestion-decay ()
  "Test time-based decay of suggestions."
  (test-etm-smart-suggest-setup)
  (unwind-protect
      (progn
        (require 'etm-smart-suggest)
        (require 'etm-smart-patterns)
        
        (cl-letf (((symbol-function 'current-buffer)
                   (lambda () (get-buffer "main.el")))
                  ((symbol-function 'tab-bar--current-tab-name)
                   (lambda () "test-tab")))
          
          ;; Create an old pattern
          (let* ((now (current-time))
                 (week-ago (time-subtract now (seconds-to-time (* 7 24 60 60)))))
            
            ;; Manually create pattern with old timestamp
            (puthash "test-tab"
                     (list (etm-smart-make-pattern
                            :from-buffer "main.el"
                            :to-buffer "old-buffer.el"
                            :count 10
                            :timestamps (list week-ago)
                            :score 0))
                     etm-smart-patterns)
            
            ;; Track a recent pattern
            (etm-smart-track-switch "main.el" "recent-buffer.el")
            
            ;; Get suggestions
            (let ((suggestions (etm-smart-suggest-buffers 2)))
              ;; Recent buffer should score higher despite lower count
              (when (and (assoc "recent-buffer.el" suggestions)
                         (assoc "old-buffer.el" suggestions))
                (should (> (cdr (assoc "recent-buffer.el" suggestions))
                           (cdr (assoc "old-buffer.el" suggestions)))))))))
    (test-etm-smart-suggest-teardown)))

;; Run tests if executed directly
;; (when (and (boundp 'load-file-name) load-file-name)
;;   (ert-run-tests-batch-and-exit))

(provide 'test-etm-smart-suggest)
;;; test-etm-smart-suggest.el ends here