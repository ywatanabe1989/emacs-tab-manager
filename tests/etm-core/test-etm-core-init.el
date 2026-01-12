;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 09:06:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/etm-core/test-etm-core-init.el

(require 'ert)

(ert-deftest test-etm-init-loads
    ()
  (require 'etm-core-init)
  (should
   (featurep 'etm-core-init)))

(ert-deftest test-etm-init-function
    ()
  ;; First load the required modules
  (require 'etm-core-init)
  (require 'etm-layout)
  (require 'etm-layout-load)
  (require 'etm-core-variables)
  
  ;; The function should be defined
  (should (fboundp 'etm-init))
  
  ;; Call etm-init function
  (etm-init)
  
  ;; Verify settings are correctly applied
  (should tab-bar-mode)
  (should (eq tab-bar-show etm-show-tab-bar))
  (should tab-bar-tab-hints)
  (should tab-bar-name-truncated)
  (should-not tab-bar-auto-width)
  (should (eq tab-bar-new-tab-to 'right))
  (should-not tab-bar-close-button-show))

(provide 'test-etm-core-init)

(when
    (not load-file-name)
  (message "test-etm-core-init.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-core/etm-core-init.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-05-10 08:46:30>
;; ;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-core/etm-core-init.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; (require 'etm-core-variables)
;; (require 'etm-layout)
;; (require 'etm-layout-load)
;; (require 'etm-groups)
;; (require 'etm-buffer-numeric)
;; (require 'etm-buffer-auto-track)
;; 
;; ;;;###autoload
;; 
;; (defun etm-init
;;     ()
;;   "Initialize Emacs Tab Manager."
;;   (interactive)
;;   (--etm-layout-load-all)
;;   (tab-bar-mode t)
;;   (setq tab-bar-show etm-show-tab-bar
;;         tab-bar-tab-hints t
;;         tab-bar-name-truncated t
;;         tab-bar-auto-width nil
;;         tab-bar-new-tab-to 'right
;;         tab-bar-close-button-show nil)
;;   (custom-set-faces
;;    '(tab-bar
;;      ((t
;;        (:background "gray20" :foreground "white"))))
;;    '(tab-bar-tab
;;      ((t
;;        (:inherit tab-bar :background "dark green" :foreground "gray60"))))
;;    '(tab-bar-tab-inactive
;;      ((t
;;        (:inherit tab-bar :background "gray20" :foreground "gray80")))))
;;   ;; Initialize groups system
;;   (etm-groups-init)
;;   ;; Initialize numeric buffer system
;;   (etm-numeric-setup-hooks)
;;   ;; Initialize automatic buffer tracking
;;   (etm-auto-track-setup))
;; 
;; (provide 'etm-core-init)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-core-init.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))

;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-core/etm-core-init.el
;; --------------------------------------------------------------------------------

;;; test-etm-core-init.el ends here
