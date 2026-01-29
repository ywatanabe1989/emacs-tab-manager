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

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/etm-layout-preview.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-05-25 10:25:00>
;; ;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-layout/etm-layout-preview.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; ;;; Commentary:
;; ;; Layout preview system for ETM
;; ;; Provides functionality to preview saved layouts without loading them
;; 
;; (require 'etm-core-variables)
;; (require 'tabulated-list)
;; (require 'cl-lib)
;; 
;; ;; Data Structures
;; ;; ----------------------------------------
;; 
;; (defun etm-layout-parse-window-spec (spec)
;;   "Parse window SPEC into a plist.
;; SPEC is a list (TYPE PATH X Y WIDTH HEIGHT HOST)."
;;   (list :type (nth 0 spec)
;;         :path (nth 1 spec)
;;         :x (nth 2 spec)
;;         :y (nth 3 spec)
;;         :width (nth 4 spec)
;;         :height (nth 5 spec)
;;         :host (nth 6 spec)))
;; 
;; ;; Layout Analysis
;; ;; ----------------------------------------
;; 
;; (defun etm-layout-calculate-dimensions (windows)
;;   "Calculate grid dimensions from WINDOWS list.
;; Returns (ROWS . COLUMNS)."
;;   (if (null windows)
;;       '(0 . 0)
;;     (let ((max-x 0)
;;           (max-y 0)
;;           (min-x most-positive-fixnum)
;;           (min-y most-positive-fixnum))
;;       ;; Find bounds
;;       (dolist (window windows)
;;         (let ((x (nth 2 window))
;;               (y (nth 3 window))
;;               (width (nth 4 window))
;;               (height (nth 5 window)))
;;           (setq min-x (min min-x x))
;;           (setq min-y (min min-y y))
;;           (setq max-x (max max-x (+ x width)))
;;           (setq max-y (max max-y (+ y height)))))
;;       ;; Calculate grid dimensions
;;       (let ((rows 1)
;;             (cols 1))
;;         ;; Count columns by X positions
;;         (let ((x-positions (seq-uniq (mapcar (lambda (w) (nth 2 w)) windows))))
;;           (setq cols (length x-positions)))
;;         ;; Count rows by Y positions
;;         (let ((y-positions (seq-uniq (mapcar (lambda (w) (nth 3 w)) windows))))
;;           (setq rows (length y-positions)))
;;         (cons rows cols)))))
;; 
;; (defun etm-layout-extract-info (content)
;;   "Extract layout information from file CONTENT string."
;;   (condition-case err
;;       (with-temp-buffer
;;         (insert content)
;;         (goto-char (point-min))
;;         (let ((name nil)
;;               (windows nil)
;;               (host nil))
;;           ;; Extract layout name
;;           (when (re-search-forward "defun etm-open-\\([^[:space:]()]+\\)" nil t)
;;             (setq name (match-string 1)))
;;           ;; Find the windows list
;;           (goto-char (point-min))
;;           (when (re-search-forward "--etm-layout-create-from-positions" nil t)
;;             ;; Skip past layout name string
;;             (re-search-forward "\"[^\"]+\"" nil t)
;;             ;; Find the quoted list
;;             (when (re-search-forward "'(" nil t)
;;               (backward-char 2)  ; Go back to before the quote
;;               (condition-case nil
;;                   (let ((sexp (read (current-buffer))))
;;                     (when (and (eq (car sexp) 'quote)
;;                                (listp (cadr sexp)))
;;                       (setq windows (cadr sexp))))
;;                 (error nil)))
;;             ;; Find host - it's the last argument
;;             (when windows
;;               (forward-sexp)  ; Skip past the windows list
;;               (skip-chars-forward " \n\t")
;;               (let ((next-sexp (condition-case nil
;;                                    (read (current-buffer))
;;                                  (error nil))))
;;                 (cond
;;                  ((stringp next-sexp) (setq host next-sexp))
;;                  ((null next-sexp) (setq host nil))))))
;;           ;; Build info plist
;;           (list :name name
;;                 :window-count (length windows)
;;                 :dimensions (etm-layout-calculate-dimensions windows)
;;                 :host host
;;                 :windows windows)))
;;     (error
;;      (signal 'error (list "Failed to parse layout content" err)))))
;; 
;; ;; File Operations
;; ;; ----------------------------------------
;; 
;; (defun etm-layout-parse-file (file-path)
;;   "Parse layout FILE-PATH and return layout info."
;;   (condition-case err
;;       (with-temp-buffer
;;         (insert-file-contents file-path)
;;         (let ((info (etm-layout-extract-info (buffer-string))))
;;           (plist-put info :file-path file-path)
;;           (plist-put info :timestamp (nth 5 (file-attributes file-path)))
;;           info))
;;     (error
;;      (signal 'file-error (list "Cannot parse layout file" file-path err)))))
;; 
;; (defun etm-layout-scan-directory ()
;;   "Scan layout directory and return list of layout info plists."
;;   (let ((layout-files (directory-files etm-layout-save-dir t "^etm-open-.*\\.el$"))
;;         (layouts '()))
;;     (dolist (file layout-files)
;;       (condition-case nil
;;           (push (etm-layout-parse-file file) layouts)
;;         (error nil))) ; Skip problematic files
;;     (nreverse layouts)))
;; 
;; ;; Rendering Functions
;; ;; ----------------------------------------
;; 
;; (defun etm-layout-render-window-grid (windows dimensions)
;;   "Render ASCII grid for WINDOWS with DIMENSIONS (rows . cols)."
;;   (let* ((rows (car dimensions))
;;          (cols (cdr dimensions))
;;          (col-width 21)
;;          (grid-lines '()))
;;     ;; Top border
;;     (push (concat "┌" (make-string col-width ?─)
;;                   (mapconcat (lambda (_) (concat "┬" (make-string col-width ?─)))
;;                              (number-sequence 2 cols) "")
;;                   "┐") grid-lines)
;;     ;; Window rows
;;     (dotimes (row rows)
;;       ;; Window content
;;       (let ((row-windows (seq-filter (lambda (w) 
;;                                        (= row (cl-position (plist-get w :y)
;;                                                            (seq-uniq (mapcar (lambda (w) (plist-get w :y))
;;                                                                              windows)))))
;;                                      windows)))
;;         (push (concat "│ " 
;;                       (mapconcat (lambda (win)
;;                                    (let ((type (plist-get win :type))
;;                                          (path (plist-get win :path)))
;;                                      (format "[%-5s] %-12s"
;;                                              (symbol-name type)
;;                                              (file-name-nondirectory 
;;                                               (directory-file-name path)))))
;;                                  row-windows
;;                                  " │ ")
;;                       " │") grid-lines))
;;       ;; Add empty lines for height
;;       (push (concat "│" (make-string (1+ col-width) ? )
;;                     (mapconcat (lambda (_) (concat "│" (make-string (1+ col-width) ? )))
;;                                (number-sequence 2 cols) "")
;;                     "│") grid-lines)
;;       ;; Middle border (not for last row)
;;       (when (< row (1- rows))
;;         (push (concat "├" (make-string col-width ?─)
;;                       (mapconcat (lambda (_) (concat "┼" (make-string col-width ?─)))
;;                                  (number-sequence 2 cols) "")
;;                       "┤") grid-lines)))
;;     ;; Bottom border
;;     (push (concat "└" (make-string col-width ?─)
;;                   (mapconcat (lambda (_) (concat "┴" (make-string col-width ?─)))
;;                              (number-sequence 2 cols) "")
;;                   "┘") grid-lines)
;;     (mapconcat #'identity (nreverse grid-lines) "\n")))
;; 
;; (defun etm-layout-render-preview (name windows host)
;;   "Render preview for layout NAME with WINDOWS and HOST."
;;   (if (null windows)
;;       (format "Layout: %s\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\nNo windows defined"
;;               name)
;;     (let* ((dimensions (etm-layout-calculate-dimensions windows))
;;            (parsed-windows (mapcar #'etm-layout-parse-window-spec windows))
;;            (grid (etm-layout-render-window-grid parsed-windows dimensions)))
;;       (concat
;;        (format "Layout: %s%s\n"
;;                name
;;                (if host (format " (Host: %s)" host) ""))
;;        "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
;;        grid "\n"
;;        (format "Dimensions: %dx%d windows\n" (cdr dimensions) (car dimensions))
;;        (format "Total windows: %d\n" (length windows))
;;        (when host (format "Remote host: %s\n" host))))))
;; 
;; ;; List Mode Functions
;; ;; ----------------------------------------
;; 
;; (defun etm-layout-format-list-entry (layout-info)
;;   "Format LAYOUT-INFO for tabulated list display."
;;   (vector (plist-get layout-info :name)
;;           (number-to-string (plist-get layout-info :window-count))
;;           (let ((dims (plist-get layout-info :dimensions)))
;;             (format "%dx%d" (car dims) (cdr dims)))
;;           (or (plist-get layout-info :host) "localhost")))
;; 
;; ;; Interactive Functions
;; ;; ----------------------------------------
;; 
;; (defvar etm-layout-preview-buffer "*ETM Layout Preview*"
;;   "Buffer name for layout preview.")
;; 
;; (defun etm-layout-get-at-point ()
;;   "Get layout info at point in layout list buffer."
;;   (when (derived-mode-p 'etm-layout-list-mode)
;;     (tabulated-list-get-id)))
;; 
;; (defun etm-layout-show-preview (layout-info)
;;   "Show preview for LAYOUT-INFO in a popup window."
;;   (let ((preview (etm-layout-render-preview
;;                   (plist-get layout-info :name)
;;                   (plist-get layout-info :windows)
;;                   (plist-get layout-info :host))))
;;     (with-current-buffer (get-buffer-create etm-layout-preview-buffer)
;;       (erase-buffer)
;;       (insert preview)
;;       (goto-char (point-min))
;;       (display-buffer (current-buffer)
;;                       '((display-buffer-below-selected)
;;                         (window-height . fit-window-to-buffer))))))
;; 
;; (defun etm-layout-preview-at-point ()
;;   "Preview the layout at point."
;;   (interactive)
;;   (let ((layout-info (etm-layout-get-at-point)))
;;     (when layout-info
;;       (etm-layout-show-preview layout-info)
;;       t)))
;; 
;; ;; Major Mode Definition
;; ;; ----------------------------------------
;; 
;; (define-derived-mode etm-layout-list-mode tabulated-list-mode "ETM-Layouts"
;;   "Major mode for ETM layout list."
;;   (setq tabulated-list-format
;;         [("Name" 20 t)
;;          ("Windows" 8 t)
;;          ("Grid" 8 t)
;;          ("Host" 15 t)])
;;   (setq tabulated-list-padding 2)
;;   (setq tabulated-list-sort-key (cons "Name" nil))
;;   (tabulated-list-init-header))
;; 
;; (provide 'etm-layout-preview)
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/etm-layout-preview.el
;; --------------------------------------------------------------------------------

;;; test-etm-layout-preview.el ends here
