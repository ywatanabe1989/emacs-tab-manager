;;; test-etm-groups-navigation.el --- Tests for ETM buffer groups navigation -*- coding: utf-8; lexical-binding: t -*-
;; Author: ywatanabe
;; Time-stamp: <2025-01-25 15:43:00 (ywatanabe)>
;;; Commentary:
;; Tests for navigating between buffers within groups
;;; Code:

(require 'ert)

;; Test helper functions
(defun test-etm-groups-nav-setup ()
  "Set up test environment for group navigation."
  (when (boundp 'etm-tab-buffer-groups)
    (clrhash etm-tab-buffer-groups))
  ;; Create test buffers
  (dolist (name '("main.el" "test.el" "utils.el" "README.md"))
    (with-current-buffer (get-buffer-create name)
      (setq buffer-file-name (expand-file-name name)))))

(defun test-etm-groups-nav-teardown ()
  "Clean up test environment."
  ;; Kill test buffers
  (dolist (name '("main.el" "test.el" "utils.el" "README.md"))
    (when (get-buffer name)
      (kill-buffer name)))
  (when (boundp 'etm-tab-buffer-groups)
    (clrhash etm-tab-buffer-groups)))

;; Navigation tests
(ert-deftest test-etm-groups-next-buffer-in-group ()
  "Test navigating to next buffer in group."
  (test-etm-groups-nav-setup)
  (unwind-protect
      (progn
        (require 'etm-groups-navigation)
        ;; Create group with buffers
        (etm-groups-create "project-foo")
        (etm-groups-add-buffer "project-foo" "main.el")
        (etm-groups-add-buffer "project-foo" "test.el")
        (etm-groups-add-buffer "project-foo" "utils.el")
        
        ;; Start in main.el
        (switch-to-buffer "main.el")
        ;; Navigate to next
        (etm-groups-next-buffer "project-foo")
        (should (string= (buffer-name) "test.el"))
        ;; Navigate again
        (etm-groups-next-buffer "project-foo")
        (should (string= (buffer-name) "utils.el"))
        ;; Should wrap around
        (etm-groups-next-buffer "project-foo")
        (should (string= (buffer-name) "main.el")))
    (test-etm-groups-nav-teardown)))

(ert-deftest test-etm-groups-previous-buffer-in-group ()
  "Test navigating to previous buffer in group."
  (test-etm-groups-nav-setup)
  (unwind-protect
      (progn
        (require 'etm-groups-navigation)
        ;; Create group with buffers
        (etm-groups-create "project-foo")
        (etm-groups-add-buffer "project-foo" "main.el")
        (etm-groups-add-buffer "project-foo" "test.el")
        (etm-groups-add-buffer "project-foo" "utils.el")
        
        ;; Start in main.el
        (switch-to-buffer "main.el")
        ;; Navigate to previous (should wrap to end)
        (etm-groups-previous-buffer "project-foo")
        (should (string= (buffer-name) "utils.el"))
        ;; Navigate again
        (etm-groups-previous-buffer "project-foo")
        (should (string= (buffer-name) "test.el")))
    (test-etm-groups-nav-teardown)))

(ert-deftest test-etm-groups-switch-to-group ()
  "Test switching to first buffer in a group."
  (test-etm-groups-nav-setup)
  (unwind-protect
      (progn
        (require 'etm-groups-navigation)
        ;; Create groups
        (etm-groups-create "project-foo")
        (etm-groups-add-buffer "project-foo" "main.el")
        (etm-groups-add-buffer "project-foo" "test.el")
        
        (etm-groups-create "docs")
        (etm-groups-add-buffer "docs" "README.md")
        
        ;; Start somewhere else
        (switch-to-buffer "*scratch*")
        
        ;; Switch to project group
        (etm-groups-switch-to-group "project-foo")
        (should (member (buffer-name) '("main.el" "test.el")))
        
        ;; Switch to docs group
        (etm-groups-switch-to-group "docs")
        (should (string= (buffer-name) "README.md")))
    (test-etm-groups-nav-teardown)))

(ert-deftest test-etm-groups-cycle-groups ()
  "Test cycling through different groups."
  (test-etm-groups-nav-setup)
  (unwind-protect
      (progn
        (require 'etm-groups-navigation)
        ;; Create multiple groups
        (etm-groups-create "project-a")
        (etm-groups-add-buffer "project-a" "main.el")
        
        (etm-groups-create "project-b")
        (etm-groups-add-buffer "project-b" "test.el")
        
        (etm-groups-create "docs")
        (etm-groups-add-buffer "docs" "README.md")
        
        ;; Start in project-a
        (switch-to-buffer "main.el")
        
        ;; Cycle to next group
        (etm-groups-cycle-next)
        (should (string= (buffer-name) "test.el"))
        
        ;; Cycle again
        (etm-groups-cycle-next)
        (should (string= (buffer-name) "README.md"))
        
        ;; Should wrap around
        (etm-groups-cycle-next)
        (should (string= (buffer-name) "main.el")))
    (test-etm-groups-nav-teardown)))

(ert-deftest test-etm-groups-navigation-with-killed-buffers ()
  "Test navigation handles killed buffers gracefully."
  (test-etm-groups-nav-setup)
  (unwind-protect
      (progn
        (require 'etm-groups-navigation)
        ;; Create group with buffers
        (etm-groups-create "project-foo")
        (etm-groups-add-buffer "project-foo" "main.el")
        (etm-groups-add-buffer "project-foo" "test.el")
        (etm-groups-add-buffer "project-foo" "utils.el")
        
        ;; Start in main.el
        (switch-to-buffer "main.el")
        
        ;; Kill test.el
        (kill-buffer "test.el")
        
        ;; Navigation should skip killed buffer
        (etm-groups-next-buffer "project-foo")
        (should (string= (buffer-name) "utils.el")))
    (test-etm-groups-nav-teardown)))

(provide 'test-etm-groups-navigation)
;;; test-etm-groups-navigation.el ends here

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-groups/etm-groups-navigation.el
;; --------------------------------------------------------------------------------
;; ;;; etm-groups-navigation.el --- Navigation between buffers in groups -*- coding: utf-8; lexical-binding: t -*-
;; ;; Author: ywatanabe
;; ;; Time-stamp: <2025-01-25 15:50:00 (ywatanabe)>
;; ;;; Commentary:
;; ;; Functions for navigating between buffers within groups
;; ;;; Code:
;; 
;; (require 'cl-lib)
;; (require 'etm-groups-core)
;; (require 'etm-buffer-navigation)
;; 
;; ;; Helper functions
;; (defun etm-groups--get-live-buffers (buffer-names)
;;   "Filter BUFFER-NAMES to only include live buffers."
;;   (cl-remove-if-not (lambda (name) (get-buffer name)) buffer-names))
;; 
;; (defun etm-groups--get-next-buffer-cyclic (current-buffer buffer-list)
;;   "Get next buffer after CURRENT-BUFFER in BUFFER-LIST, cycling at end."
;;   (let* ((live-buffers (etm-groups--get-live-buffers buffer-list))
;;          (pos (cl-position current-buffer live-buffers :test #'string=)))
;;     (when live-buffers
;;       (if (and pos (< (1+ pos) (length live-buffers)))
;;           (nth (1+ pos) live-buffers)
;;         (car live-buffers)))))
;; 
;; (defun etm-groups--get-previous-buffer-cyclic (current-buffer buffer-list)
;;   "Get previous buffer before CURRENT-BUFFER in BUFFER-LIST, cycling at start."
;;   (let* ((live-buffers (etm-groups--get-live-buffers buffer-list))
;;          (pos (cl-position current-buffer live-buffers :test #'string=)))
;;     (when live-buffers
;;       (if (and pos (> pos 0))
;;           (nth (1- pos) live-buffers)
;;         (car (last live-buffers))))))
;; 
;; ;; Navigation functions
;; (defun etm-groups-next-buffer (group-name)
;;   "Switch to next buffer in GROUP-NAME."
;;   (let* ((buffers (etm-groups-get-buffers group-name))
;;          (current (buffer-name))
;;          (next (etm-groups--get-next-buffer-cyclic current buffers)))
;;     (if next
;;         (switch-to-buffer next)
;;       (message "No buffers in group '%s'" group-name))))
;; 
;; (defun etm-groups-previous-buffer (group-name)
;;   "Switch to previous buffer in GROUP-NAME."
;;   (let* ((buffers (etm-groups-get-buffers group-name))
;;          (current (buffer-name))
;;          (previous (etm-groups--get-previous-buffer-cyclic current buffers)))
;;     (if previous
;;         (switch-to-buffer previous)
;;       (message "No buffers in group '%s'" group-name))))
;; 
;; (defun etm-groups-switch-to-group (group-name)
;;   "Switch to first buffer in GROUP-NAME."
;;   (let* ((buffers (etm-groups-get-buffers group-name))
;;          (live-buffers (etm-groups--get-live-buffers buffers)))
;;     (if live-buffers
;;         (switch-to-buffer (car live-buffers))
;;       (message "No live buffers in group '%s'" group-name))))
;; 
;; ;; Group cycling
;; (defvar etm-groups--current-group nil
;;   "Currently active group for cycling.")
;; 
;; (defun etm-groups--get-current-group ()
;;   "Get the current group based on current buffer."
;;   (let* ((buffer-name (buffer-name))
;;          (groups (etm-groups-find-buffer-groups buffer-name)))
;;     (or (and etm-groups--current-group
;;              (member etm-groups--current-group groups)
;;              etm-groups--current-group)
;;         (car groups))))
;; 
;; (defun etm-groups--get-next-group ()
;;   "Get the next group in the cycle."
;;   (let* ((all-groups (etm-groups-list-all))
;;          (current (etm-groups--get-current-group))
;;          (pos (cl-position current all-groups :test #'string=)))
;;     (when all-groups
;;       (if (and pos (< (1+ pos) (length all-groups)))
;;           (nth (1+ pos) all-groups)
;;         (car all-groups)))))
;; 
;; (defun etm-groups-cycle-next ()
;;   "Cycle to the next group and switch to its first buffer."
;;   (let ((next-group (etm-groups--get-next-group)))
;;     (when next-group
;;       (setq etm-groups--current-group next-group)
;;       (etm-groups-switch-to-group next-group))))
;; 
;; (defun etm-groups-cycle-previous ()
;;   "Cycle to the previous group and switch to its first buffer."
;;   (let* ((all-groups (etm-groups-list-all))
;;          (current (etm-groups--get-current-group))
;;          (pos (cl-position current all-groups :test #'string=))
;;          (prev-group (when all-groups
;;                        (if (and pos (> pos 0))
;;                            (nth (1- pos) all-groups)
;;                          (car (last all-groups))))))
;;     (when prev-group
;;       (setq etm-groups--current-group prev-group)
;;       (etm-groups-switch-to-group prev-group))))
;; 
;; ;; Interactive commands
;; (defun etm-groups-next-buffer-interactive ()
;;   "Interactively navigate to next buffer in a group."
;;   (interactive)
;;   (let* ((current-groups (etm-groups-find-buffer-groups (buffer-name)))
;;          (group (if (= 1 (length current-groups))
;;                     (car current-groups)
;;                   (completing-read "Group: " current-groups nil t))))
;;     (if group
;;         (etm-groups-next-buffer group)
;;       (message "Current buffer is not in any group"))))
;; 
;; (defun etm-groups-previous-buffer-interactive ()
;;   "Interactively navigate to previous buffer in a group."
;;   (interactive)
;;   (let* ((current-groups (etm-groups-find-buffer-groups (buffer-name)))
;;          (group (if (= 1 (length current-groups))
;;                     (car current-groups)
;;                   (completing-read "Group: " current-groups nil t))))
;;     (if group
;;         (etm-groups-previous-buffer group)
;;       (message "Current buffer is not in any group"))))
;; 
;; (defun etm-groups-switch-interactive ()
;;   "Interactively switch to a group."
;;   (interactive)
;;   (let* ((groups (etm-groups-list-all))
;;          (group (completing-read "Switch to group: " groups nil t)))
;;     (etm-groups-switch-to-group group)))
;; 
;; (provide 'etm-groups-navigation)
;; ;;; etm-groups-navigation.el ends here
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-groups/etm-groups-navigation.el
;; --------------------------------------------------------------------------------

;;; test-etm-groups-navigation.el ends here
