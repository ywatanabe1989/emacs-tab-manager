;;; test-etm-groups-indicators.el --- Tests for ETM buffer groups visual indicators -*- coding: utf-8; lexical-binding: t -*-
;; Author: ywatanabe
;; Time-stamp: <2025-01-25 15:45:00 (ywatanabe)>
;;; Commentary:
;; Tests for visual indicators showing buffer group membership
;;; Code:

(require 'ert)

;; Test helper functions
(defun test-etm-groups-indicators-setup ()
  "Set up test environment for group indicators."
  (when (boundp 'etm-tab-buffer-groups)
    (clrhash etm-tab-buffer-groups))
  (setq etm-groups-indicators-enabled t))

(defun test-etm-groups-indicators-teardown ()
  "Clean up test environment."
  (when (boundp 'etm-tab-buffer-groups)
    (clrhash etm-tab-buffer-groups))
  (setq etm-groups-indicators-enabled nil))

;; Indicator tests
(ert-deftest test-etm-groups-format-single-group ()
  "Test formatting indicator for buffer in single group."
  (test-etm-groups-indicators-setup)
  (unwind-protect
      (progn
        (require 'etm-groups-indicators)
        ;; Create group and add buffer
        (etm-groups-create "project-foo")
        (etm-groups-add-buffer "project-foo" "main.el")
        
        ;; Test indicator format
        (let ((indicator (etm-groups-format-buffer-indicator "main.el")))
          (should (stringp indicator))
          (should (string-match "project-foo" indicator))))
    (test-etm-groups-indicators-teardown)))

(ert-deftest test-etm-groups-format-multiple-groups ()
  "Test formatting indicator for buffer in multiple groups."
  (test-etm-groups-indicators-setup)
  (unwind-protect
      (progn
        (require 'etm-groups-indicators)
        ;; Add buffer to multiple groups
        (etm-groups-create "project-foo")
        (etm-groups-create "important")
        (etm-groups-add-buffer "project-foo" "main.el")
        (etm-groups-add-buffer "important" "main.el")
        
        ;; Test indicator shows both groups
        (let ((indicator (etm-groups-format-buffer-indicator "main.el")))
          (should (stringp indicator))
          (should (string-match "project-foo" indicator))
          (should (string-match "important" indicator))))
    (test-etm-groups-indicators-teardown)))

(ert-deftest test-etm-groups-format-no-groups ()
  "Test formatting indicator for buffer not in any group."
  (test-etm-groups-indicators-setup)
  (unwind-protect
      (progn
        (require 'etm-groups-indicators)
        ;; Buffer not in any group
        (let ((indicator (etm-groups-format-buffer-indicator "orphan.el")))
          (should (string= "" indicator))))
    (test-etm-groups-indicators-teardown)))

(ert-deftest test-etm-groups-indicators-disabled ()
  "Test that indicators return empty when disabled."
  (test-etm-groups-indicators-setup)
  (unwind-protect
      (progn
        (require 'etm-groups-indicators)
        (setq etm-groups-indicators-enabled nil)
        
        ;; Create group and add buffer
        (etm-groups-create "project-foo")
        (etm-groups-add-buffer "project-foo" "main.el")
        
        ;; Indicator should be empty when disabled
        (let ((indicator (etm-groups-format-buffer-indicator "main.el")))
          (should (string= "" indicator))))
    (test-etm-groups-indicators-teardown)))

(ert-deftest test-etm-groups-tab-line-format ()
  "Test integration with tab-line display."
  (test-etm-groups-indicators-setup)
  (unwind-protect
      (progn
        (require 'etm-groups-indicators)
        ;; Create group and add buffer
        (etm-groups-create "project-foo")
        (etm-groups-add-buffer "project-foo" "main.el")
        
        ;; Test tab-line format includes group
        (with-current-buffer (get-buffer-create "main.el")
          (let ((tab-line (etm-groups-tab-line-format)))
            (should (stringp tab-line))
            (should (string-match "project-foo" tab-line)))))
    (test-etm-groups-indicators-teardown)))

(ert-deftest test-etm-groups-mode-line-format ()
  "Test integration with mode-line display."
  (test-etm-groups-indicators-setup)
  (unwind-protect
      (progn
        (require 'etm-groups-indicators)
        ;; Create groups
        (etm-groups-create "project-foo")
        (etm-groups-create "docs")
        (etm-groups-add-buffer "project-foo" "main.el")
        (etm-groups-add-buffer "docs" "main.el")
        
        ;; Test mode-line format
        (with-current-buffer (get-buffer-create "main.el")
          (let ((mode-line (etm-groups-mode-line-format)))
            (should (stringp mode-line))
            ;; Should show abbreviated format for mode-line
            (should (or (string-match "\\[2\\]" mode-line)  ; Shows count
                        (string-match "p-f,doc" mode-line))))))  ; Or abbreviated names
    (test-etm-groups-indicators-teardown)))

(ert-deftest test-etm-groups-group-color-assignment ()
  "Test that groups get consistent colors assigned."
  (test-etm-groups-indicators-setup)
  (unwind-protect
      (progn
        (require 'etm-groups-indicators)
        ;; Create groups
        (etm-groups-create "project-foo")
        (etm-groups-create "project-bar")
        
        ;; Colors should be consistent
        (let ((color1a (etm-groups-get-group-color "project-foo"))
              (color1b (etm-groups-get-group-color "project-foo"))
              (color2 (etm-groups-get-group-color "project-bar")))
          ;; Same group should get same color
          (should (equal color1a color1b))
          ;; Different groups should get different colors
          (should-not (equal color1a color2))))
    (test-etm-groups-indicators-teardown)))

(provide 'test-etm-groups-indicators)
;;; test-etm-groups-indicators.el ends here

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-groups/etm-groups-indicators.el
;; --------------------------------------------------------------------------------
;; ;;; etm-groups-indicators.el --- Visual indicators for buffer groups -*- coding: utf-8; lexical-binding: t -*-
;; ;; Author: ywatanabe
;; ;; Time-stamp: <2025-01-25 15:52:00 (ywatanabe)>
;; ;;; Commentary:
;; ;; Visual indicators showing which groups buffers belong to
;; ;;; Code:
;; 
;; (require 'etm-groups-core)
;; (require 'etm-core-variables)
;; 
;; ;; Variables
;; (defcustom etm-groups-indicators-enabled t
;;   "Whether to show group indicators in tab-bar and mode-line."
;;   :type 'boolean
;;   :group 'etm)
;; 
;; (defcustom etm-groups-indicator-format " [%s]"
;;   "Format string for group indicators. %s is replaced with group info."
;;   :type 'string
;;   :group 'etm)
;; 
;; (defcustom etm-groups-indicator-separator ","
;;   "Separator between multiple group names."
;;   :type 'string
;;   :group 'etm)
;; 
;; (defcustom etm-groups-use-colors t
;;   "Whether to use colors for group indicators."
;;   :type 'boolean
;;   :group 'etm)
;; 
;; (defvar etm-groups--color-list
;;   '("deep sky blue" "spring green" "orange" "magenta" 
;;     "yellow" "cyan" "pink" "light green")
;;   "List of colors to assign to groups.")
;; 
;; (defvar etm-groups--color-assignments (make-hash-table :test 'equal)
;;   "Hash table mapping group names to colors.")
;; 
;; ;; Helper functions
;; (defun etm-groups-get-group-color (group-name)
;;   "Get consistent color for GROUP-NAME."
;;   (or (gethash group-name etm-groups--color-assignments)
;;       (let* ((used-colors (hash-table-values etm-groups--color-assignments))
;;              (available-colors (cl-set-difference etm-groups--color-list 
;;                                                   used-colors :test #'equal))
;;              (color (or (car available-colors)
;;                         (nth (mod (hash-table-count etm-groups--color-assignments)
;;                                   (length etm-groups--color-list))
;;                              etm-groups--color-list))))
;;         (puthash group-name color etm-groups--color-assignments)
;;         color)))
;; 
;; (defun etm-groups--format-group-name (group-name &optional abbreviate)
;;   "Format GROUP-NAME, optionally ABBREVIATE for compact display."
;;   (let ((formatted (if abbreviate
;;                        (etm-groups--abbreviate-name group-name)
;;                      group-name)))
;;     (if etm-groups-use-colors
;;         (propertize formatted 'face 
;;                     `(:foreground ,(etm-groups-get-group-color group-name)))
;;       formatted)))
;; 
;; (defun etm-groups--abbreviate-name (name)
;;   "Create abbreviation from NAME (e.g., 'project-foo' -> 'p-f')."
;;   (let ((parts (split-string name "-")))
;;     (mapconcat (lambda (part) 
;;                  (if (> (length part) 0) 
;;                      (substring part 0 1) 
;;                    ""))
;;                parts "-")))
;; 
;; ;; Indicator formatting
;; (defun etm-groups-format-buffer-indicator (buffer-name)
;;   "Format group indicator for BUFFER-NAME."
;;   (if (not etm-groups-indicators-enabled)
;;       ""
;;     (let ((groups (etm-groups-find-buffer-groups buffer-name)))
;;       (if groups
;;           (format etm-groups-indicator-format
;;                   (mapconcat #'etm-groups--format-group-name 
;;                              groups 
;;                              etm-groups-indicator-separator))
;;         ""))))
;; 
;; (defun etm-groups-format-buffer-indicator-compact (buffer-name)
;;   "Format compact group indicator for BUFFER-NAME (for mode-line)."
;;   (if (not etm-groups-indicators-enabled)
;;       ""
;;     (let ((groups (etm-groups-find-buffer-groups buffer-name)))
;;       (cond
;;        ((null groups) "")
;;        ((= 1 (length groups)) 
;;         (format etm-groups-indicator-format
;;                 (etm-groups--format-group-name (car groups) t)))
;;        (t
;;         (format " [%d]" (length groups)))))))
;; 
;; ;; Integration with tab-line
;; (defun etm-groups-tab-line-format ()
;;   "Format tab-line including group information."
;;   (let* ((buffer-name (buffer-name))
;;          (base-format (or (and (boundp 'tab-line-format) tab-line-format)
;;                           buffer-name))
;;          (group-indicator (etm-groups-format-buffer-indicator buffer-name)))
;;     (concat (format "%s" base-format) group-indicator)))
;; 
;; ;; Integration with mode-line
;; (defun etm-groups-mode-line-format ()
;;   "Format mode-line segment for group information."
;;   (let ((indicator (etm-groups-format-buffer-indicator-compact (buffer-name))))
;;     (unless (string= indicator "")
;;       indicator)))
;; 
;; ;; Setup functions
;; (defvar etm-groups--original-tab-line-format nil
;;   "Original tab-line-format before modification.")
;; 
;; (defun etm-groups-enable-indicators ()
;;   "Enable group indicators in UI."
;;   (interactive)
;;   (setq etm-groups-indicators-enabled t)
;;   ;; Add to mode-line if not already there
;;   (unless (memq 'etm-groups-mode-line-format mode-line-format)
;;     (setq-default mode-line-format 
;;                   (append mode-line-format '((:eval (etm-groups-mode-line-format))))))
;;   (message "Group indicators enabled"))
;; 
;; (defun etm-groups-disable-indicators ()
;;   "Disable group indicators in UI."
;;   (interactive)
;;   (setq etm-groups-indicators-enabled nil)
;;   (message "Group indicators disabled"))
;; 
;; ;; Interactive commands
;; (defun etm-groups-show-buffer-groups ()
;;   "Show which groups the current buffer belongs to."
;;   (interactive)
;;   (let* ((buffer-name (buffer-name))
;;          (groups (etm-groups-find-buffer-groups buffer-name)))
;;     (if groups
;;         (message "Buffer '%s' is in groups: %s" 
;;                  buffer-name
;;                  (mapconcat #'identity groups ", "))
;;       (message "Buffer '%s' is not in any group" buffer-name))))
;; 
;; (provide 'etm-groups-indicators)
;; ;;; etm-groups-indicators.el ends here
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-groups/etm-groups-indicators.el
;; --------------------------------------------------------------------------------

;;; test-etm-groups-indicators.el ends here
