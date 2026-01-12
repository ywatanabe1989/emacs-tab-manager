;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-keys-groups.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-keys-groups

;;; Code:

(require 'ert)
(require 'etm-keys-groups)

;; Add your tests here
;; (ert-deftest test-etm-keys-groups-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-keys/etm-keys-groups.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-01-25 15:58:00>
;; ;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-keys/etm-keys-groups.el
;; 
;; (require 'etm-groups)
;; (require 'etm-keys-command-map)
;; 
;; ;; Bind group commands under 'g' prefix
;; (define-key etm-command-map (kbd "g") etm-groups-command-map)
;; 
;; ;; Quick access keys for common group operations
;; (define-key etm-command-map (kbd "G") #'etm-groups-switch-interactive)
;; (define-key etm-command-map (kbd "A") #'etm-groups-add-current-buffer)
;; 
;; (provide 'etm-keys-groups)
;; 
;; (when (not load-file-name)
;;   (message "etm-keys-groups.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-keys/etm-keys-groups.el
;; --------------------------------------------------------------------------------

;;; test-etm-keys-groups.el ends here
