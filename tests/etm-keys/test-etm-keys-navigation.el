;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-keys-navigation.el

(require 'ert)

(ert-deftest test-etm-keys-navigation-loadable
    ()
  (require 'etm-keys-navigation)
  (should
   (featurep 'etm-keys-navigation)))

(ert-deftest test-etm-keys-navigation-m1-bound
    ()
  (should
   (commandp
    (lookup-key global-map
                (kbd "M-1")))))

(ert-deftest test-etm-keys-navigation-m2-bound
    ()
  (should
   (commandp
    (lookup-key global-map
                (kbd "M-2")))))

(ert-deftest test-etm-keys-navigation-m3-bound
    ()
  (should
   (commandp
    (lookup-key global-map
                (kbd "M-3")))))

(ert-deftest test-etm-keys-navigation-m4-bound
    ()
  (should
   (commandp
    (lookup-key global-map
                (kbd "M-4")))))

(ert-deftest test-etm-keys-navigation-m5-bound
    ()
  (should
   (commandp
    (lookup-key global-map
                (kbd "M-5")))))

(ert-deftest test-etm-keys-navigation-m6-bound
    ()
  (should
   (commandp
    (lookup-key global-map
                (kbd "M-6")))))

(ert-deftest test-etm-keys-navigation-m7-bound
    ()
  (should
   (commandp
    (lookup-key global-map
                (kbd "M-7")))))

(ert-deftest test-etm-keys-navigation-m8-bound
    ()
  (should
   (commandp
    (lookup-key global-map
                (kbd "M-8")))))

(ert-deftest test-etm-keys-navigation-m9-bound
    ()
  (should
   (commandp
    (lookup-key global-map
                (kbd "M-9")))))

(provide 'test-etm-keys-navigation)

(when
    (not load-file-name)
  (message "test-etm-keys-navigation.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-keys/etm-keys-navigation.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-09-30 19:53:23>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/etm-keys/etm-keys-navigation.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (require 'etm-buffer-navigation)
;; (require 'etm-keys-command-map)
;; 
;; (global-set-key (kbd "M-1") #'etm-navigation-jump-to-1)
;; 
;; (global-set-key (kbd "M-2") #'etm-navigation-jump-to-2)
;; 
;; (global-set-key (kbd "M-3") #'etm-navigation-jump-to-3)
;; 
;; (global-set-key (kbd "M-4") #'etm-navigation-jump-to-4)
;; 
;; (global-set-key (kbd "M-5") #'etm-navigation-jump-to-5)
;; 
;; (global-set-key (kbd "M-6") #'etm-navigation-jump-to-6)
;; 
;; (global-set-key (kbd "M-7") #'etm-navigation-jump-to-7)
;; 
;; (global-set-key (kbd "M-8") #'etm-navigation-jump-to-8)
;; 
;; (global-set-key (kbd "M-9") #'etm-navigation-jump-to-9)
;; 
;; ;; (global-set-key
;; ;;  (kbd "M-1")
;; ;;  (lambda
;; ;;    ()
;; ;;    (interactive)
;; ;;    (etm-navigation-jump-by-index 1)))
;; 
;; ;; (global-set-key
;; ;;  (kbd "M-2")
;; ;;  (lambda
;; ;;    ()
;; ;;    (interactive)
;; ;;    (etm-navigation-jump-by-index 2)))
;; 
;; ;; (global-set-key
;; ;;  (kbd "M-3")
;; ;;  (lambda
;; ;;    ()
;; ;;    (interactive)
;; ;;    (etm-navigation-jump-by-index 3)))
;; 
;; ;; (global-set-key
;; ;;  (kbd "M-4")
;; ;;  (lambda
;; ;;    ()
;; ;;    (interactive)
;; ;;    (etm-navigation-jump-by-index 4)))
;; 
;; ;; (global-set-key
;; ;;  (kbd "M-5")
;; ;;  (lambda
;; ;;    ()
;; ;;    (interactive)
;; ;;    (etm-navigation-jump-by-index 5)))
;; 
;; ;; (global-set-key
;; ;;  (kbd "M-6")
;; ;;  (lambda
;; ;;    ()
;; ;;    (interactive)
;; ;;    (etm-navigation-jump-by-index 6)))
;; 
;; ;; (global-set-key
;; ;;  (kbd "M-7")
;; ;;  (lambda
;; ;;    ()
;; ;;    (interactive)
;; ;;    (etm-navigation-jump-by-index 7)))
;; 
;; ;; (global-set-key
;; ;;  (kbd "M-8")
;; ;;  (lambda
;; ;;    ()
;; ;;    (interactive)
;; ;;    (etm-navigation-jump-by-index 8)))
;; 
;; ;; (global-set-key
;; ;;  (kbd "M-9")
;; ;;  (lambda
;; ;;    ()
;; ;;    (interactive)
;; ;;    (etm-navigation-jump-by-index 9)))
;; 
;; 
;; (provide 'etm-keys-navigation)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-keys-navigation.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-keys/etm-keys-navigation.el
;; --------------------------------------------------------------------------------

;;; test-etm-keys-navigation.el ends here
