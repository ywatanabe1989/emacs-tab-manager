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