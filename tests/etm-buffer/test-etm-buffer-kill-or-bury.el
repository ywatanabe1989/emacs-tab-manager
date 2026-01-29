;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-buffer-kill-or-bury.el

(require 'ert)
(require 'etm-buffer-kill-or-bury)

(ert-deftest test-etm-buffer-kill-or-bury-registered
    ()
  (with-temp-buffer
    (let*
        ((buffer-name
          (buffer-name))
         (etm-registered-buffers
          `(("tab1" .
             (("home" . ,buffer-name))))))
      (etm-buffer-kill-or-bury)
      (should
       (buffer-live-p
        (get-buffer buffer-name))))))

(ert-deftest test-etm-buffer-kill-or-bury-protected
    ()
  (with-temp-buffer
    (let*
        ((buffer-name
          (buffer-name))
         (etm-protected-buffers
          (list buffer-name)))
      (etm-buffer-kill-or-bury)
      (should
       (buffer-live-p
        (get-buffer buffer-name))))))

(ert-deftest test-etm-buffer-kill-or-bury-unregistered
    ()
  (with-temp-buffer
    (let*
        ((buffer-name
          (buffer-name))
         (etm-registered-buffers nil)
         (etm-protected-buffers nil))
      (etm-buffer-kill-or-bury)
      (should-not
       (buffer-live-p
        (get-buffer buffer-name))))))

(provide 'test-etm-buffer-kill-or-bury)

(when
    (not load-file-name)
  (message "test-etm-buffer-kill-or-bury.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-buffer/etm-buffer-kill-or-bury.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-05-30 05:57:58>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/etm-buffer/etm-buffer-kill-or-bury.el
;; 
;; ;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)
;; 
;; 
;; (require 'etm-core-variables)
;; (require 'etm-buffer-checkers)
;; 
;; ;; Kill or Bury
;; ;; ----------------------------------------
;; 
;; (defun etm-buffer-kill-or-bury ()
;;   "Kill buffer if not registered in any tab, otherwise bury it."
;;   (interactive)
;;   (let ((buf (current-buffer)))
;;     (if (or (--etm-buffer-registered-p (buffer-name buf))
;;             (--etm-buffer-protected-p (buffer-name buf)))
;;         (progn
;;           (--etm-flash-mode-line "black" 3)
;;           (bury-buffer)
;;           (message "Buried"))
;;       (progn
;;         (--etm-flash-mode-line "darkgreen" 3)
;;         (kill-buffer)
;;         (message "Killed")))))
;; 
;; ;; Flash Mode Line
;; ;; ----------------------------------------
;; 
;; (defun --etm-flash-mode-line (color &optional count)
;;   "Flash the mode line with COLOR for COUNT times.
;; If COUNT is nil or not specified, flash once."
;;   (let ((original-color (face-background 'mode-line))
;;         (flash-count (or count 1)))
;;     (dotimes (ii flash-count)
;;       (set-face-background 'mode-line color)
;;       (force-mode-line-update)
;;       (sit-for 0.01)
;;       (set-face-background 'mode-line original-color)
;;       (force-mode-line-update)
;;       (when (< ii (1- flash-count))
;;         (sit-for 0.01)))))
;; 
;; 
;; (provide 'etm-buffer-kill-or-bury)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-buffer-kill-or-bury.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-buffer/etm-buffer-kill-or-bury.el
;; --------------------------------------------------------------------------------

;;; test-etm-buffer-kill-or-bury.el ends here
