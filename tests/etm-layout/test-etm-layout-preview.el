;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-25 10:15:00>
;;; File: test-etm-layout-preview.el

(require 'ert)
(require 'etm-layout)

;; Test Data
;; ----------------------------------------

(defconst test-layout-simple
  '((file "~/test/" 0 1 80 62 nil)
    (shell "~/test/" 80 1 80 62 nil))
  "Simple 2-window layout for testing.")

(defconst test-layout-complex
  '((file "~/project/" 0 1 80 30 "remote-host")
    (shell "~/project/" 80 1 80 30 "remote-host")
    (file "~/project/src/" 0 31 80 32 "remote-host")
    (shell "~/project/" 80 31 80 32 "remote-host"))
  "Complex 4-window layout with remote host.")

;; Mock Functions
;; ----------------------------------------

(defun mock-layout-file-content (name positions host)
  "Generate mock layout file content."
  (format "(defun etm-open-%s ()
  \"Create tab layout for specific configuration.\"
  (interactive)
  (--etm-layout-create-from-positions \"%s\"
    '%S
    %s))

(defalias '%s 'etm-open-%s)"
          name name positions 
          (if host (format "\"%s\"" host) "nil")
          name name))

;; Core Function Tests
;; ----------------------------------------

(ert-deftest test-etm-layout-parse-window-spec ()
  "Test parsing of window specifications."
  ;; Simple file window
  (let ((spec '(file "~/test/" 0 1 80 62 nil)))
    (should (equal (etm-layout-parse-window-spec spec)
                   '(:type file :path "~/test/" :x 0 :y 1 
                     :width 80 :height 62 :host nil))))
  
  ;; Shell window with host
  (let ((spec '(shell "~/project/" 80 31 80 32 "remote-host")))
    (should (equal (etm-layout-parse-window-spec spec)
                   '(:type shell :path "~/project/" :x 80 :y 31 
                     :width 80 :height 32 :host "remote-host")))))

(ert-deftest test-etm-layout-calculate-dimensions ()
  "Test calculation of layout dimensions."
  ;; 2-window horizontal layout
  (should (equal (etm-layout-calculate-dimensions test-layout-simple)
                 '(1 . 2))) ; 1 row, 2 columns
  
  ;; 4-window grid layout
  (should (equal (etm-layout-calculate-dimensions test-layout-complex)
                 '(2 . 2))) ; 2 rows, 2 columns
  
  ;; Single window
  (should (equal (etm-layout-calculate-dimensions 
                  '((file "~/" 0 1 160 62 nil)))
                 '(1 . 1))) ; 1 row, 1 column
  
  ;; 3-window vertical layout
  (should (equal (etm-layout-calculate-dimensions
                  '((file "~/" 0 1 160 20 nil)
                    (file "~/" 0 21 160 20 nil)
                    (file "~/" 0 41 160 21 nil)))
                 '(3 . 1)))) ; 3 rows, 1 column

(ert-deftest test-etm-layout-extract-info ()
  "Test extraction of layout information from file content."
  (let ((content (mock-layout-file-content "test" test-layout-simple nil)))
    (let ((info (etm-layout-extract-info content)))
      (should (equal (plist-get info :name) "test"))
      (should (equal (plist-get info :window-count) 2))
      (should (equal (plist-get info :dimensions) '(1 . 2)))
      (should (null (plist-get info :host)))
      (should (= (length (plist-get info :windows)) 2)))))

(ert-deftest test-etm-layout-render-preview ()
  "Test rendering of layout preview."
  ;; Simple 2-window layout
  (let ((preview (etm-layout-render-preview "test" test-layout-simple nil)))
    (should (string-match "Layout: test" preview))
    (should (string-match "\\[file\\].*~/test/" preview))
    (should (string-match "\\[shell\\].*~/test/" preview))
    (should (string-match "Total windows: 2" preview)))
  
  ;; Complex layout with host
  (let ((preview (etm-layout-render-preview "complex" test-layout-complex "remote-host")))
    (should (string-match "Layout: complex (Host: remote-host)" preview))
    (should (string-match "Total windows: 4" preview))
    (should (string-match "Dimensions: 2x2" preview))))

(ert-deftest test-etm-layout-render-window-grid ()
  "Test ASCII grid rendering for windows."
  ;; 2x1 grid
  (let ((windows (mapcar #'etm-layout-parse-window-spec test-layout-simple)))
    (let ((grid (etm-layout-render-window-grid windows '(1 . 2))))
      (should (string-match "┌─.*─┬─.*─┐" grid))
      (should (string-match "└─.*─┴─.*─┘" grid))))
  
  ;; 2x2 grid
  (let ((windows (mapcar #'etm-layout-parse-window-spec test-layout-complex)))
    (let ((grid (etm-layout-render-window-grid windows '(2 . 2))))
      (should (string-match "├─.*─┼─.*─┤" grid)))))

(ert-deftest test-etm-layout-scan-directory ()
  "Test scanning of layout directory."
  (let ((etm-layout-save-dir (make-temp-file "etm-test-" t)))
    (unwind-protect
        (progn
          ;; Create test layout files
          (with-temp-file (expand-file-name "etm-open-test1.el" etm-layout-save-dir)
            (insert (mock-layout-file-content "test1" test-layout-simple nil)))
          (with-temp-file (expand-file-name "etm-open-test2.el" etm-layout-save-dir)
            (insert (mock-layout-file-content "test2" test-layout-complex "host")))
          
          ;; Test scanning
          (let ((layouts (etm-layout-scan-directory)))
            (should (= (length layouts) 2))
            (should (cl-find "test1" layouts :key (lambda (l) (plist-get l :name)) :test #'equal))
            (should (cl-find "test2" layouts :key (lambda (l) (plist-get l :name)) :test #'equal))))
      ;; Cleanup
      (delete-directory etm-layout-save-dir t))))

(ert-deftest test-etm-layout-list-format ()
  "Test formatting of layout list entries."
  (let ((layout-info '(:name "test" :window-count 4 :dimensions (2 . 2) 
                       :host "remote" :timestamp 1234567890)))
    (let ((formatted (etm-layout-format-list-entry layout-info)))
      (should (vectorp formatted))
      (should (equal (aref formatted 0) "test"))
      (should (equal (aref formatted 1) "4"))
      (should (equal (aref formatted 2) "2x2"))
      (should (equal (aref formatted 3) "remote")))))

(ert-deftest test-etm-layout-preview-at-point ()
  "Test preview display for layout at point."
  (let ((test-layout-info '(:name "test" :windows nil :host nil)))
    (cl-letf (((symbol-function 'etm-layout-get-at-point)
               (lambda () test-layout-info))
              ((symbol-function 'etm-layout-show-preview)
               (lambda (info) 
                 (should (equal (plist-get info :name) "test"))
                 t)))
      (should (etm-layout-preview-at-point)))))

(ert-deftest test-etm-layout-preview-error-handling ()
  "Test error handling in preview functions."
  ;; Missing layout file
  (should-error (etm-layout-parse-file "/non/existent/file.el"))
  
  ;; Corrupted layout content
  (let ((bad-content "(defun etm-open-bad () (error))"))
    (should-error (etm-layout-extract-info bad-content)))
  
  ;; Empty window list
  (should (string-match "No windows defined" 
                        (etm-layout-render-preview "empty" nil nil))))

(provide 'test-etm-layout-preview)