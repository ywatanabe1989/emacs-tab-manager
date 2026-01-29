;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-keys-buffer.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-keys-buffer

;;; Code:

(require 'ert)
(require 'etm-keys-buffer)

;; Add your tests here
;; (ert-deftest test-etm-keys-buffer-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-keys/etm-keys-buffer.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-02-13 16:42:11>
;; ;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-keys/etm-keys-buffer.el
;; 
;; (require 'etm-buffer-navigation)
;; (require 'etm-buffer)
;; (require 'etm-keys-command-map)
;; 
;; ;; Setters
;; ;; ----------------------------------------
;; 
;; (define-key etm-command-map
;;             (kbd "H")
;;             #'etm-buffer-set-home)
;; 
;; (define-key etm-command-map
;;             (kbd "S")
;;             #'etm-buffer-set-semi-home)
;; 
;; (define-key etm-command-map
;;             (kbd "R")
;;             #'etm-buffer-set-results)
;; 
;; ;; Jumpers
;; ;; ----------------------------------------
;; 
;; (define-key etm-command-map
;;             (kbd "h")
;;             #'etm-navigation-jump-by-buffer-type-home)
;; 
;; (define-key etm-command-map
;;             (kbd "s")
;;             #'etm-navigation-jump-by-buffer-type-semi-home)
;; 
;; (define-key etm-command-map
;;             (kbd "r")
;;             #'etm-navigation-jump-by-buffer-type-results)
;; 
;; ;; Killer
;; ;; ----------------------------------------
;; 
;; (define-key etm-command-map
;;             (kbd "k")
;;             'etm-buffer-kill-or-bury)
;; 
;; ;; List buffers
;; ;; ----------------------------------------
;; 
;; (define-key etm-command-map
;;             (kbd "L")
;;             'etm-list-registered-buffers)
;; 
;; (define-key etm-command-map
;;             (kbd "A")
;;             'etm-list-all-tabs-buffers)
;; 
;; ;; Ibuffer integration
;; ;; ----------------------------------------
;; 
;; (define-key etm-command-map
;;             (kbd "I")
;;             'etm-ibuffer)
;; 
;; (define-key etm-command-map
;;             (kbd "i")
;;             'etm-ibuffer-hybrid)
;; 
;; (provide 'etm-keys-buffer)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-keys-buffer.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))

;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-keys/etm-keys-buffer.el
;; --------------------------------------------------------------------------------

;;; test-etm-keys-buffer.el ends here
