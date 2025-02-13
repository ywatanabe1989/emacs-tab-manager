;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-22 22:17:05
;;; Timestamp: <2025-01-22 22:17:05>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/inits/03-visu-070-tab-01-close.el

;; 1. Main Tab Control Functions
;; ----------------------------------------

(defun my/tab-reset ()
  "Close all tabs in the current Emacs session."
  (interactive)
  (my/tab-close-all)
  (sleep-for 1)
  (my/tab-startup))

(defun my/tab-close-all ()
  "Close all tabs in the current Emacs session."
  (interactive)
  (if (fboundp 'tab-bar-mode)
      (progn
        (tab-bar-mode 1)
        (while (> (length (tab-bar-tabs)) 1)
          (tab-bar-close-tab))
        (message "All tabs closed except the current one."))
    (message "Tab functionality not available in this Emacs version.")))

;; 2. Core Tab Operations
;; ----------------------------------------

(defun my/tab-close-others ()
  "Close all tabs except the current one."
  (interactive)
  (let* ((current-tab-name (alist-get 'name (tab-bar--current-tab)))
         (tabs (tab-bar-tabs)))
    (dolist (tab tabs)
      (let ((tab-name (alist-get 'name tab)))
        (unless (string= tab-name current-tab-name)
          (my/tab-close-by-tab-name tab-name))))))

(defun my/tab-close-by-tab-name (tab-name)
  "Close the specified tab if it exists and is not the only tab; otherwise, return nil."
  (interactive "sTab name to close: ")
  (let* ((tabs (tab-bar-tabs))
         (tab-index (cl-position tab-name tabs
                                 :test (lambda (name tab)
                                         (string= name (alist-get 'name tab))))))
    (if (and tab-index (> (length tabs) 1))
        (progn
          (tab-bar-close-tab (1+ tab-index))
          t)
      nil)))

;; 3. Helper Functions
;; ----------------------------------------

(defun my/tab-close-by-tab-name-by-id (tab-id)
  (interactive "sTab ID to close: ")
  (tab-bar-close-tab (- tab-id 1)))

(defun my/tab-remove-1 ()
  (interactive)
  (my/tab-jump-to 1)
  (tab-close))

(defun my/tab-close-by-tab-name-and-move-to-next-tab ()
  "Close the current tab and move to the next one."
  (interactive)
  (tab-close)
  (tab-next))

(defun my/tab-close-by-tab-name-and-move-to-previous-tab ()
  "Close the current tab and move to the previous one."
  (interactive)
  (let ((prev-tab-index (1- (tab-bar--current-tab-index))))
    (tab-close)
    (when (>= prev-tab-index 0)
      (tab-bar-select-tab (1+ prev-tab-index)))))

;; (defun my/tab-close-by-tab-name (tab-name)
;;   "Close the specified tab if it exists and is not the only tab; otherwise, return nil."
;;   (interactive "sTab name to close: ")
;;   (let* ((tabs (tab-bar-tabs))
;;          (tab-index (cl-position tab-name tabs
;;                                  :test (lambda (name tab)
;;                                          (string= name (alist-get 'name tab))))))
;;     (if (and tab-index (> (length tabs) 1))
;;         (progn
;;           (tab-bar-close-tab (1+ tab-index))
;;           t)
;;       nil)))

;; (defun my/tab-close-by-tab-name-by-id (tab-id)
;;   (interactive "sTab ID to close: ")
;;   (tab-bar-close-tab (- tab-id 1))
;;   )

;; (defun my/tab-close-all ()
;;   "Close all tabs in the current Emacs session."
;;   (interactive)
;;   (if (fboundp 'tab-bar-mode)
;;       (progn
;;         (tab-bar-mode 1)  ; Ensure tab-bar-mode is enabled
;;         (while (> (length (tab-bar-tabs)) 1)
;;           (tab-bar-close-tab))
;;         (message "All tabs closed except the current one."))
;;     (message "Tab functionality not available in this Emacs version.")))


;; (defun my/tab-close-others ()
;;   "Close all tabs except the current one."
;;   (interactive)
;;   (let* ((current-tab-name (alist-get 'name (tab-bar--current-tab)))
;;          (tabs (tab-bar-tabs)))
;;     (dolist (tab tabs)
;;       (let ((tab-name (alist-get 'name tab)))
;;         (unless (string= tab-name current-tab-name)
;;           (my/tab-close-by-tab-name tab-name))))))

;; (defun my/tab-reset ()
;;   "Close all tabs in the current Emacs session."
;;   (interactive)
;;   (my/tab-close-all)
;;   (sleep-for 1)
;;   (my/tab-startup))

;; (defun my/tab-remove-1 ()
;;   (interactive)
;;   (my/tab-jump-to 1)
;;   (tab-close))

;; (defun my/tab-close-by-tab-name-and-move-to-next-tab ()
;;   "Close the current tab and move to the next one."
;;   (interactive)
;;   (tab-close)
;;   (tab-next))

;; (defun my/tab-close-by-tab-name-and-move-to-previous-tab ()
;;   "Close the current tab and move to the previous one."
;;   (interactive)
;;   (let ((prev-tab-index (1- (tab-bar--current-tab-index))))
;;     (tab-close)
;;     (when (>= prev-tab-index 0)
;;       (tab-bar-select-tab (1+ prev-tab-index)))))


(when (not load-file-name)
  (message "%s loaded." (file-name-nondirectory (or load-file-name buffer-file-name))))