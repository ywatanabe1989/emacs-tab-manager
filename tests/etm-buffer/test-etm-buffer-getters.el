;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-buffer-getters.el

(require 'ert)
(require 'etm-buffer-getters)

(ert-deftest test---etm-buffer-get-basic
    ()
  (let
      ((etm-registered-buffers
        '(("tab1" .
           (("home" . "buffer1"))))))
    (should
     (string=
      (--etm-buffer-get "home"
                      '((name . "tab1")))
      "buffer1"))))

(ert-deftest test---etm-buffer-get-nonexistent
    ()
  (let
      ((etm-registered-buffers
        '(("tab1" .
           (("home" . "buffer1"))))))
    (should
     (null
      (--etm-buffer-get "results"
                      '((name . "tab1")))))))

(ert-deftest test---etm-buffer-get-wrong-tab
    ()
  (let
      ((etm-registered-buffers
        '(("tab1" .
           (("home" . "buffer1"))))))
    (should
     (null
      (--etm-buffer-get "home"
                      '((name . "tab2")))))))

(provide 'test---etm-buffer-getters)

(provide 'test---etm-buffer-getters)

(when
    (not load-file-name)
  (message "test---etm-buffer-getters.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-buffer/etm-buffer-getters.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-05-14 12:42:55>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/etm-buffer/etm-buffer-getters.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (require 'etm-core-variables)
;; (require 'etm-buffer-checkers)
;; 
;; (defun --etm-buffer-get (type &optional tab)
;;   "Get buffer of TYPE from TAB."
;;   (interactive
;;    (list
;;     (completing-read "Type: "
;;                      (append etm-registered-buffer-types
;;                              etm-custom-buffer-types))))
;;   (unless tab
;;     (setq tab
;;           (tab-bar--current-tab)))
;;   (let* ((tab-name
;;           (alist-get 'name tab))
;;          (tab-entry
;;           (assoc tab-name etm-registered-buffers))
;;          (buffer-name
;;           (cdr (assoc type (cdr tab-entry)))))
;; 
;;     ;; Return the buffer name
;;     buffer-name))
;; 
;; 
;; (provide 'etm-buffer-getters)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-buffer-getters.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-buffer/etm-buffer-getters.el
;; --------------------------------------------------------------------------------

;;; test-etm-buffer-getters.el ends here
