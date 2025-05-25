;;; test-etm-smart-ui.el --- Tests for ETM smart UI integration -*- coding: utf-8; lexical-binding: t -*-

;; Author: ywatanabe
;; Date: 2025-05-26
;; Version: 1.0.0

;;; Commentary:
;; Test suite for ETM smart suggestions UI integration.
;; Tests completion interface, buffer display, and user interaction.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load paths for test environment
(add-to-list 'load-path (expand-file-name "../.." (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../etm-core" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../etm-smart" (file-name-directory load-file-name)))

;; Test helpers
(defvar test-etm-smart-ui-buffers nil
  "List of test buffers.")

(defvar test-etm-smart-ui-selected nil
  "Mock selected buffer.")

(defun test-etm-smart-ui-setup ()
  "Set up test environment."
  ;; Create test buffers
  (setq test-etm-smart-ui-buffers
        (list (get-buffer-create "main.el")
              (get-buffer-create "test.el")
              (get-buffer-create "utils.el")))
  
  ;; Mock functions
  (fset 'buffer-list (lambda () test-etm-smart-ui-buffers))
  (setq etm-smart-patterns (make-hash-table :test 'equal)))

(defun test-etm-smart-ui-teardown ()
  "Tear down test environment."
  ;; Kill test buffers
  (dolist (buf test-etm-smart-ui-buffers)
    (when (buffer-live-p buf)
      (kill-buffer buf)))
  (setq test-etm-smart-ui-buffers nil)
  (setq test-etm-smart-ui-selected nil))

;; Tests

(ert-deftest test-etm-smart-show-suggestions-buffer ()
  "Test displaying suggestions in a dedicated buffer."
  (test-etm-smart-ui-setup)
  (unwind-protect
      (progn
        (require 'etm-smart-ui)
        (require 'etm-smart-patterns)
        
        (cl-letf (((symbol-function 'current-buffer)
                   (lambda () (car test-etm-smart-ui-buffers)))
                  ((symbol-function 'tab-bar--current-tab-name)
                   (lambda () "test-tab"))
                  ((symbol-function 'display-buffer)
                   (lambda (buffer &rest _)
                     ;; Just verify buffer was created
                     buffer)))
          
          ;; Track some patterns
          (etm-smart-track-switch "main.el" "test.el")
          (etm-smart-track-switch "main.el" "test.el")
          (etm-smart-track-switch "main.el" "utils.el")
          
          ;; Show suggestions
          (etm-smart-show-suggestions)
          
          ;; Check suggestion buffer exists
          (let ((buffer (get-buffer "*ETM Suggestions*")))
            (should buffer)
            
            ;; Check buffer content
            (with-current-buffer buffer
              ;; Should contain suggested buffers
              (should (string-match "test\\.el" (buffer-string)))
              (should (string-match "utils\\.el" (buffer-string)))
              
              ;; Should show scores or percentages
              (should (or (string-match "[0-9]+%" (buffer-string))
                          (string-match "\\[.*\\]" (buffer-string))))
              
              ;; Should have instructions
              (should (or (string-match "Press" (buffer-string))
                          (string-match "Select" (buffer-string))))))
          
          ;; Clean up
          (when (get-buffer "*ETM Suggestions*")
            (kill-buffer "*ETM Suggestions*"))))
    (test-etm-smart-ui-teardown)))

(ert-deftest test-etm-smart-suggestion-keymap ()
  "Test keymap in suggestion buffer."
  (test-etm-smart-ui-setup)
  (unwind-protect
      (progn
        (require 'etm-smart-ui)
        
        (cl-letf (((symbol-function 'current-buffer)
                   (lambda () (car test-etm-smart-ui-buffers)))
                  ((symbol-function 'tab-bar--current-tab-name)
                   (lambda () "test-tab"))
                  ((symbol-function 'display-buffer)
                   (lambda (buffer &rest _) buffer))
                  ((symbol-function 'switch-to-buffer)
                   (lambda (buffer)
                     (setq test-etm-smart-ui-selected buffer))))
          
          ;; Track patterns
          (etm-smart-track-switch "main.el" "test.el")
          
          ;; Show suggestions
          (etm-smart-show-suggestions)
          
          (with-current-buffer "*ETM Suggestions*"
            ;; Test key bindings exist
            (should (keymapp etm-smart-suggestions-mode-map))
            
            ;; Test number keys
            (should (lookup-key etm-smart-suggestions-mode-map "1"))
            (should (lookup-key etm-smart-suggestions-mode-map "2"))
            
            ;; Test navigation keys
            (should (lookup-key etm-smart-suggestions-mode-map "n"))
            (should (lookup-key etm-smart-suggestions-mode-map "p"))
            
            ;; Test selection key
            (should (lookup-key etm-smart-suggestions-mode-map "\r"))
            
            ;; Test quit key
            (should (lookup-key etm-smart-suggestions-mode-map "q")))
          
          ;; Clean up
          (when (get-buffer "*ETM Suggestions*")
            (kill-buffer "*ETM Suggestions*"))))
    (test-etm-smart-ui-teardown)))

(ert-deftest test-etm-smart-minibuffer-annotations ()
  "Test minibuffer completion annotations."
  (test-etm-smart-ui-setup)
  (unwind-protect
      (progn
        (require 'etm-smart-ui)
        
        (cl-letf (((symbol-function 'current-buffer)
                   (lambda () (car test-etm-smart-ui-buffers)))
                  ((symbol-function 'tab-bar--current-tab-name)
                   (lambda () "test-tab")))
          
          ;; Track patterns
          (etm-smart-track-switch "main.el" "test.el")
          (etm-smart-track-switch "main.el" "test.el")
          
          ;; Test annotation function
          (let ((etm-smart-last-suggestions '(("test.el" . 0.85)
                                              ("utils.el" . 0.45))))
            
            ;; High-score suggestion
            (let ((annotation (etm-smart-annotate-completion "test.el")))
              (should annotation)
              (should (string-match "85%" annotation)))
            
            ;; Low-score suggestion
            (let ((annotation (etm-smart-annotate-completion "utils.el")))
              (should annotation)
              (should (string-match "45%" annotation)))
            
            ;; Non-suggested buffer
            (let ((annotation (etm-smart-annotate-completion "other.el")))
              (should (or (null annotation)
                          (equal annotation "")))))))
    (test-etm-smart-ui-teardown)))

(ert-deftest test-etm-smart-quick-switch ()
  "Test quick switch functionality."
  (test-etm-smart-ui-setup)
  (unwind-protect
      (progn
        (require 'etm-smart-ui)
        
        (cl-letf (((symbol-function 'current-buffer)
                   (lambda () (car test-etm-smart-ui-buffers)))
                  ((symbol-function 'tab-bar--current-tab-name)
                   (lambda () "test-tab"))
                  ((symbol-function 'switch-to-buffer)
                   (lambda (buffer)
                     (setq test-etm-smart-ui-selected buffer))))
          
          ;; Track patterns
          (etm-smart-track-switch "main.el" "test.el")
          (etm-smart-track-switch "main.el" "utils.el")
          
          ;; Test quick switch to first suggestion
          (etm-smart-quick-switch 1)
          (should (equal test-etm-smart-ui-selected "test.el"))
          
          ;; Reset
          (setq test-etm-smart-ui-selected nil)
          
          ;; Test quick switch to second suggestion
          (etm-smart-quick-switch 2)
          (should (or (equal test-etm-smart-ui-selected "utils.el")
                      (equal test-etm-smart-ui-selected "test.el")))))
    (test-etm-smart-ui-teardown)))

(ert-deftest test-etm-smart-mode-line-indicator ()
  "Test mode line indicator for smart suggestions."
  (test-etm-smart-ui-setup)
  (unwind-protect
      (progn
        (require 'etm-smart-ui)
        
        (cl-letf (((symbol-function 'current-buffer)
                   (lambda () (car test-etm-smart-ui-buffers)))
                  ((symbol-function 'tab-bar--current-tab-name)
                   (lambda () "test-tab")))
          
          ;; Enable mode line indicator
          (let ((etm-smart-show-mode-line t))
            
            ;; Track patterns
            (etm-smart-track-switch "main.el" "test.el")
            
            ;; Test mode line format
            (let ((indicator (etm-smart-mode-line-indicator)))
              (should (stringp indicator))
              
              ;; When suggestions available
              (when (etm-smart-suggest-buffers 1)
                (should (> (length indicator) 0))
                ;; Should indicate suggestion availability
                (should (or (string-match "→" indicator)
                            (string-match "suggest" indicator)
                            (string-match "[0-9]" indicator)))))
            
            ;; Disable mode line
            (let ((etm-smart-show-mode-line nil))
              (should (equal (etm-smart-mode-line-indicator) ""))))))
    (test-etm-smart-ui-teardown)))

(ert-deftest test-etm-smart-completing-read-integration ()
  "Test integration with completing-read."
  (test-etm-smart-ui-setup)
  (unwind-protect
      (progn
        (require 'etm-smart-ui)
        
        ;; Mock completing-read to capture arguments
        (let ((captured-collection nil)
              (captured-predicate nil))
          
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt collection predicate &rest _)
                       (setq captured-collection collection)
                       (setq captured-predicate predicate)
                       ;; Return first item
                       (if (functionp collection)
                           "test.el"
                         (car collection))))
                    ((symbol-function 'current-buffer)
                     (lambda () (car test-etm-smart-ui-buffers)))
                    ((symbol-function 'tab-bar--current-tab-name)
                     (lambda () "test-tab")))
            
            ;; Track patterns
            (etm-smart-track-switch "main.el" "test.el")
            (etm-smart-track-switch "main.el" "utils.el")
            
            ;; Call enhanced completing-read
            (etm-smart-completing-read "Switch to buffer: ")
            
            ;; Verify collection was modified/sorted
            (should captured-collection)
            
            ;; If collection is a list, check order
            (when (listp captured-collection)
              ;; Suggested buffers should be first
              (let ((first-items (seq-take captured-collection 2)))
                (should (or (member "test.el" first-items)
                            (member "utils.el" first-items))))))))
    (test-etm-smart-ui-teardown)))

;; Run tests if executed directly
;; (when (and (boundp 'load-file-name) load-file-name)
;;   (ert-run-tests-batch-and-exit))

(provide 'test-etm-smart-ui)
;;; test-etm-smart-ui.el ends here