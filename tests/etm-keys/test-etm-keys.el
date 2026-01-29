;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-24>
;;; Test file for: etm-keys.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for etm-keys

;;; Code:

(require 'ert)
(require 'etm-keys)

;; Add your tests here
;; (ert-deftest test-etm-keys-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-keys/etm-keys.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-02-12 23:32:27>
;; ;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-keys/etm-keys.el
;; 
;; (require 'etm-keys-command-map)
;; (require 'etm-keys-buffer)
;; (require 'etm-keys-layout)
;; (require 'etm-keys-navigation)
;; (require 'etm-keys-numeric-buffers)
;; (require 'etm-keys-groups)
;; 
;; (provide 'etm-keys)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-keys.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-keys/etm-keys.el
;; --------------------------------------------------------------------------------

;;; test-etm-keys.el ends here
