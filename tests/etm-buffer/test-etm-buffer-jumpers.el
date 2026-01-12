;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-buffer-jumpers.el

(require 'ert)
(require 'etm-buffer-jumpers)

(ert-deftest test-etm-navigation-jump-by-buffer-type-existing
    ()
  (with-temp-buffer
    (let
        ((etm-registered-buffers
          '(("tab1" .
             (("home" . "test-buffer")))))
         (current-tab
          '((name . "tab1"))))
      (rename-buffer "test-buffer")
      (should
       (etm-navigation-jump-by-buffer-type "home"))
      (should
       (string=
        (buffer-name)
        "test-buffer")))))

(ert-deftest test-etm-navigation-jump-by-buffer-type-nonexistent
    ()
  (let
      ((etm-registered-buffers nil))
    (with-current-buffer
        (get-buffer-create "*Messages*")
      (let
          ((message-text nil))
        (setq message-text
              (etm-navigation-jump-by-buffer-type "home"))
        (should
         (string= message-text
                  "No home buffer set for current tab"))))))

(ert-deftest test-etm-buffer-define-buffer-type-jumper-function
    ()
  (etm-buffer-define-buffer-type-jumper-function "test")
  (should
   (fboundp 'etm-navigation-jump-by-buffer-type-test)))

(ert-deftest test-etm-buffer-define-buffer-type-jumper-functions
    ()
  (let
      ((etm-registered-buffer-types
        '("home" "semi-home")))
    (etm-buffer-define-buffer-type-jumper-functions)
    (should
     (fboundp 'etm-navigation-jump-by-buffer-type-home))
    (should
     (fboundp 'etm-navigation-jump-by-buffer-type-semi-home))))

(provide 'test-etm-buffer-jumpers)

(when
    (not load-file-name)
  (message "test-etm-buffer-jumpers.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-buffer/etm-buffer-jumpers.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-04-24 08:39:04>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/etm-buffer/etm-buffer-jumpers.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; (require 'etm-core-variables)
;; (require 'etm-buffer-checkers)
;; (require 'etm-buffer-navigation)
;; 
;; ;; Define jump functions
;; 
;; (defun etm-buffer-define-buffer-type-jumper-function
;;     (type)
;;   "Define a buffer jump function for the given TYPE.
;; Example: For type 'home', creates `etm-navigation-jump-by-buffer-type-home'."
;;   (eval
;;    `(defun ,(intern
;;              (format "etm-navigation-jump-by-buffer-type-%s"
;;                      (if
;;                          (symbolp type)
;;                          (symbol-name type)
;;                        type)))
;;         ()
;;       ,(format "Jump to %s buffer of current tab." type)
;;       (interactive)
;;       (etm-navigation-jump-by-buffer-type ,type))))
;; 
;; (defun etm-buffer-define-buffer-type-jumper-functions
;;     ()
;;   "Define buffer jump functions for all registered buffer types.
;; Examples:
;; `etm-navigation-jump-by-buffer-type-home'
;; `etm-navigation-jump-by-buffer-type-semi-home'
;; `etm-navigation-jump-by-buffer-type-results'"
;;   (dolist
;;       (type etm-registered-buffer-types)
;;     (etm-buffer-define-buffer-type-jumper-function type)))
;; 
;; (etm-buffer-define-buffer-type-jumper-functions)
;; 
;; (provide 'etm-buffer-jumpers)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-buffer-jumpers.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-buffer/etm-buffer-jumpers.el
;; --------------------------------------------------------------------------------

;;; test-etm-buffer-jumpers.el ends here
