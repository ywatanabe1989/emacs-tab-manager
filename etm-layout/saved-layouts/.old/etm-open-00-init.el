;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-12 18:45:10>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/inits/--remove-this-prefix-03-visu-070-tab-00-init.el

;; 1. Main Tab Configuration
;; ----------------------------------------

(defvar my/tab-buffer-alist nil
  "Alist buffer types.")

;; (use-package tab-bar
;;   :ensure t
;;   :init
;;   (tab-bar-mode t)
;;   :custom
;;   (tab-bar-show t)
;;   (tab-bar-tab-hints t)
;;   (tab-bar-name-truncated t)
;;   (tab-bar-auto-width nil)
;;   (tab-bar-new-tab-to 'right)
;;   (setq tab-bar-close-button-show nil)
;;   (custom-set-faces
;;    '(tab-bar ((t (:background "gray20" :foreground "white"))))
;;    '(tab-bar-tab ((t (:inherit tab-bar :background "gray40" :foreground "dark orange"))))
;;    '(tab-bar-tab-inactive ((t (:inherit tab-bar :background "gray20" :foreground "gray80")))))
;;   )

(use-package tab-bar
  :ensure t
  :init
  (tab-bar-mode t)
  :custom
  (tab-bar-show t)
  (tab-bar-tab-hints t)
  (tab-bar-name-truncated t)
  (tab-bar-auto-width nil)
  (tab-bar-new-tab-to 'right)
  (setq tab-bar-close-button-show nil)
  (custom-set-faces
   '(tab-bar
     ((t
       (:background "gray20" :foreground "white"))))
   '(tab-bar-tab
     ((t
       (:inherit tab-bar :background "dark green" :foreground "dark orange"))))
   '(tab-bar-tab-inactive
     ((t
       (:inherit tab-bar :background "gray20" :foreground "gray80")))))
  )

;; 2. Core Functions
;; ----------------------------------------

(defun my/tab-describe-buffer-alist
    ()
  "Print each (tab-name . buffer-alist) pair in `my/tab-buffer-alist`."
  (interactive)
  (message "%s"
           (mapconcat
            (lambda
              (pair)
              (format "%s: %s"
                      (car pair)
                      (mapconcat
                       (lambda
                         (bp)
                         (format "%s: %s"
                                 (car bp)
                                 (cdr bp)))
                       (cdr pair)
                       ", ")))
            my/tab-buffer-alist
            ", ")))

(defun my/tab-is-any-buffer
    (type)
  "Check if the current buffer is of a given type (home, semi-home, or results) in any tab."
  (let
      ((current-buffer-name
        (buffer-name))
       (found nil))
    (dolist
        (tab-entry my/tab-buffer-alist found)
      (when
          (string=
           (cdr
            (assoc type
                   (cdr tab-entry)))
           current-buffer-name)
        (setq found t)))
    found))

;; 3. Keybindings
;; ----------------------------------------

(define-prefix-command 'my/tab-bar-mode)
(global-set-key
 (kbd "M-t")
 'my/tab-bar-mode)
(define-key my/tab-bar-mode
            (kbd "0")
            'my/tab-close-by-tab-name-and-move-to-next-tab)
(define-key my/tab-bar-mode
            (kbd "1")
            'tab-close-other)
(define-key my/tab-bar-mode
            (kbd "2")
            'my/tab-new)
(define-key my/tab-bar-mode
            (kbd "n")
            'my/tab-new)
(define-key my/tab-bar-mode
            (kbd "m")
            'my/tab-move)
(define-key my/tab-bar-mode
            (kbd "r")
            'tab-rename)
(define-key my/tab-bar-mode
            (kbd "d")
            'my/tab-describe-buffer-alist)
(define-key my/tab-bar-mode
            (kbd "h")
            (lambda
              ()
              (interactive)
              (my/tab-set-buffer "home")))
(define-key my/tab-bar-mode
            (kbd "s")
            (lambda
              ()
              (interactive)
              (my/tab-set-buffer "semi-home")))
(define-key my/tab-bar-mode
            (kbd "r")
            (lambda
              ()
              (interactive)
              (my/tab-set-buffer "results")))
(define-key image-mode-map
            (kbd "h")
            (lambda
              ()
              (interactive)
              (my/tab-set-buffer "home")))
(define-key image-mode-map
            (kbd "s")
            (lambda
              ()
              (interactive)
              (my/tab-set-buffer "semi-home")))
(define-key image-mode-map
            (kbd "r")
            (lambda
              ()
              (interactive)
              (my/tab-set-buffer "results")))
(global-set-key
 (kbd "M-t M-t")
 'my/tab-close-by-tab-name-and-move-to-next-tab)
(global-set-key
 (kbd "M-w")
 'my/tab-close-by-tab-name-and-move-to-previous-tab)

;; (defvar my/tab-buffer-alist nil
;;   "Alist buffer types.")

;; (use-package tab-bar
;;   :ensure t
;;   :init
;;   (tab-bar-mode t)
;;   :custom
;;   (tab-bar-show t)
;;   (tab-bar-tab-hints t)
;;   (tab-bar-name-truncated t)
;;   (tab-bar-auto-width nil)
;;   (tab-bar-new-tab-to 'right)
;;   (setq tab-bar-close-button-show nil)
;;   (custom-set-faces
;;    '(tab-bar ((t (:background "gray20" :foreground "white"))))
;;    '(tab-bar-tab ((t (:inherit tab-bar :background "gray40" :foreground "dark orange"))))
;;    '(tab-bar-tab-inactive ((t (:inherit tab-bar :background "gray20" :foreground "gray80")))))
;;   )

;; ;; Functions
;; (defun my/tab-describe-buffer-alist ()
;;   "Print each (tab-name . buffer-alist) pair in `my/tab-buffer-alist`."
;;   (interactive)
;;   (message "%s" (mapconcat (lambda (pair)
;;                              (format "%s: %s" (car pair) (mapconcat (lambda (bp)
;;                                                                       (format "%s: %s" (car bp) (cdr bp)))
;;                                                                     (cdr pair) ", ")))
;;                            my/tab-buffer-alist
;;                            ", ")))

;; (defun my/tab-is-any-buffer (type)
;;   "Check if the current buffer is of a given type (home, semi-home, or results) in any tab."
;;   (let ((current-buffer-name (buffer-name))
;;         (found nil))
;;     (dolist (tab-entry my/tab-buffer-alist found)
;;       (when (string= (cdr (assoc type (cdr tab-entry))) current-buffer-name)
;;         (setq found t)))
;;     found))

;; ;; Bindings
;; ;; tab-bar-mode
;; (define-prefix-command 'my/tab-bar-mode)
;; (global-set-key (kbd "M-t") 'my/tab-bar-mode)
;; ;; (define-key my/tab-bar-mode (kbd "0") 'tab-close)
;; (define-key my/tab-bar-mode (kbd "0") 'my/tab-close-by-tab-name-and-move-to-next-tab)

;; (define-key my/tab-bar-mode (kbd "1") 'tab-close-other)
;; (define-key my/tab-bar-mode (kbd "2") 'my/tab-new)
;; (define-key my/tab-bar-mode (kbd "n") 'my/tab-new) ; create
;; (define-key my/tab-bar-mode (kbd "m") 'my/tab-move)
;; (define-key my/tab-bar-mode (kbd "r") 'tab-rename)
;; (define-key my/tab-bar-mode (kbd "d") 'my/tab-describe-buffer-alist)
;; (define-key my/tab-bar-mode (kbd "h") (lambda () (interactive) (my/tab-set-buffer "home")))
;; (define-key my/tab-bar-mode (kbd "s") (lambda () (interactive) (my/tab-set-buffer "semi-home")))
;; (define-key my/tab-bar-mode (kbd "r") (lambda () (interactive) (my/tab-set-buffer "results")))

;; (define-key image-mode-map (kbd "h") (lambda () (interactive) (my/tab-set-buffer "home")))
;; (define-key image-mode-map (kbd "s") (lambda () (interactive) (my/tab-set-buffer "semi-home")))
;; (define-key image-mode-map (kbd "r") (lambda () (interactive) (my/tab-set-buffer "results")))

;; (global-set-key (kbd "M-t M-t") 'my/tab-close-by-tab-name-and-move-to-next-tab)
;; ;; (global-set-key (kbd "M-w") 'my/tab-close-by-tab-name-and-move-to-next-tab)
;; (global-set-key (kbd "M-w") 'my/tab-close-by-tab-name-and-move-to-previous-tab)

(when
    (not load-file-name)
  (message "%s loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

(provide '--remove-this-prefix-03-visu-070-tab-00-init)

(when
    (not load-file-name)
  (message "--remove-this-prefix-03-visu-070-tab-00-init.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))