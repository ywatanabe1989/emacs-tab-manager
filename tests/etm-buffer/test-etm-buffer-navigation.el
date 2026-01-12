;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/etm-buffer/test-etm-buffer-navigation.el

(require 'ert)
(require 'etm-buffer-navigation)

(ert-deftest test-etm-buffer-navigation-loadable
    ()
  (should
   (featurep 'etm-buffer-navigation)))

(ert-deftest test-etm-buffer-navigation-functions-exist
    ()
  (should
   (fboundp 'etm-navigation-jump-by-buffer-type))
  (should
   (fboundp 'etm-navigation-jump-by-index))
  (should
   (fboundp 'etm-navigation-jump-by-name))
  (should
   (fboundp 'etm-navigation-move)))

(provide 'test-etm-buffer-navigation)

(when
    (not load-file-name)
  (message "test-etm-navigation.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-buffer/etm-buffer-navigation.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-09-30 19:51:56>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/etm-buffer/etm-buffer-navigation.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (require 'etm-core-variables)
;; 
;; (defun etm-navigation-jump-by-buffer-type
;;     (type)
;;   "Jump to buffer of TYPE in current tab."
;;   (interactive
;;    (list
;;     (completing-read "Jump to buffer type: "
;;                      (append etm-registered-buffer-types
;;                              etm-custom-buffer-types))))
;;   (let
;;       ((buf
;;         (--etm-buffer-get type)))
;;     (if buf
;;         (switch-to-buffer buf)
;;       (message "No %s buffer set for current tab" type))))
;; 
;; (defun etm-navigation-jump-by-index
;;     (index)
;;   "Jump to tab at INDEX."
;;   (interactive "p")
;;   (tab-bar-select-tab index))
;; 
;; (defun etm-navigation-jump-to-1 ()
;;   "Jump to tab 1."
;;   (interactive)
;;   (etm-navigation-jump-by-index 1))
;; 
;; (defun etm-navigation-jump-to-2 ()
;;   "Jump to tab 2."
;;   (interactive)
;;   (etm-navigation-jump-by-index 2))
;; 
;; (defun etm-navigation-jump-to-3 ()
;;   "Jump to tab 3."
;;   (interactive)
;;   (etm-navigation-jump-by-index 3))
;; 
;; (defun etm-navigation-jump-to-4 ()
;;   "Jump to tab 4."
;;   (interactive)
;;   (etm-navigation-jump-by-index 4))
;; 
;; (defun etm-navigation-jump-to-5 ()
;;   "Jump to tab 5."
;;   (interactive)
;;   (etm-navigation-jump-by-index 5))
;; 
;; (defun etm-navigation-jump-to-6 ()
;;   "Jump to tab 6."
;;   (interactive)
;;   (etm-navigation-jump-by-index 6))
;; 
;; (defun etm-navigation-jump-to-7 ()
;;   "Jump to tab 7."
;;   (interactive)
;;   (etm-navigation-jump-by-index 7))
;; 
;; (defun etm-navigation-jump-to-8 ()
;;   "Jump to tab 8."
;;   (interactive)
;;   (etm-navigation-jump-by-index 8))
;; 
;; (defun etm-navigation-jump-to-9 ()
;;   "Jump to tab 9."
;;   (interactive)
;;   (etm-navigation-jump-by-index 9))
;; 
;; (defun etm-navigation-jump-by-name
;;     (name)
;;   "Jump to tab with NAME."
;;   (interactive
;;    (list
;;     (completing-read "Tab name: "
;;                      (mapcar
;;                       (lambda
;;                         (tab)
;;                         (alist-get 'name tab))
;;                       (tab-bar-tabs)))))
;;   (let*
;;       ((tabs
;;         (tab-bar-tabs))
;;        (tab-index
;;         (cl-position name tabs
;;                      :test
;;                      (lambda
;;                        (name tab)
;;                        (string= name
;;                                 (alist-get 'name tab))))))
;;     (when tab-index
;;       (tab-bar-select-tab
;;        (1+ tab-index)))))
;; 
;; (defun etm-navigation-move
;;     (&optional step)
;;   "Move current tab STEP positions."
;;   (interactive
;;    (list
;;     (if current-prefix-arg
;;         (prefix-numeric-value current-prefix-arg)
;;       (read-number "Move steps: " 1))))
;;   (tab-move step))
;; 
;; 
;; (provide 'etm-buffer-navigation)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-buffer-navigation.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-buffer/etm-buffer-navigation.el
;; --------------------------------------------------------------------------------

;;; test-etm-buffer-navigation.el ends here
