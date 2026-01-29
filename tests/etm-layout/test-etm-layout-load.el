;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-layout-load.el

(require 'ert)

(ert-deftest test-etm-layout-load-loadable
    ()
  (require 'etm-layout-load)
  (should
   (featurep 'etm-layout-load)))

(ert-deftest test-etm-layout-load-function-exists
    ()
  (should
   (fboundp '--etm-layout-load-all)))

(provide 'test-etm-layout-load)

(when
    (not load-file-name)
  (message "test-etm-layout-load.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/etm-layout-load.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-05-19 12:01:15>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/etm-layout/etm-layout-load.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (defun --etm-load-file-silent
;;     (file-path)
;;   "Load Emacs Lisp file at FILE-PATH silently.
;; Suppresses all messages, warnings and outputs during loading.
;; Only error messages will be shown if any.
;; 
;; Arguments:
;; - FILE-PATH: Path to the Emacs Lisp file to load"
;;   (let
;;       ((inhibit-message t)
;;        (message-log-max nil))
;;     (with-temp-message ""
;;       (with-temp-buffer
;;         ;; Temporarily redirect stderr
;;         (let
;;             ((standard-output
;;               (current-buffer))
;;              (warning-minimum-level :error))
;;           (load-file file-path))))))
;; 
;; (defun --etm-layout-load-all
;;     ()
;;   (dolist
;;       (file
;;        (directory-files etm-layout-save-dir t "\\.el$"))
;;     (--etm-load-file-silent file)))
;; 
;; (--etm-layout-load-all)
;; 
;; 
;; (provide 'etm-layout-load)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-layout-load.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/etm-layout-load.el
;; --------------------------------------------------------------------------------

;;; test-etm-layout-load.el ends here
