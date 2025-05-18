;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 08:44:10>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-tabs/etm-tabs-new-and-rename.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;;###autoload
(defun etm-new
    (arg)
  "Create a new tab with name ARG."
  (interactive
   (list
    (read-string "Enter tab name: ")))
  (tab-new)
  (tab-rename
   (message "%s" arg)))

;;;###autoload
(defun etm-rename
    (arg)
  "Rename current tab to ARG."
  (interactive
   (list
    (read-string "Enter tab name: ")))
  (tab-rename
   (message "%s" arg)))


;; (defun etm-open-async (layout-name)
;;   "Asynchronously open a layout by name."
;;   (interactive
;;    (list (completing-read 
;;           "Open layout: " 
;;           (mapcar (lambda (file)
;;                     (string-remove-prefix 
;;                      "etm-open-" 
;;                      (string-remove-suffix ".el" (file-name-nondirectory file))))
;;                   (directory-files etm-layout-save-dir t "etm-open-.*\\.el$")))))
;;   (run-with-idle-timer 0.1 nil
;;                        (lambda (name)
;;                          (let ((layout-func (intern (concat "etm-open-" name))))
;;                            (when (fboundp layout-func)
;;                              (funcall layout-func))))
;;                        layout-name)
;;   (message "Opening %s layout..." layout-name))

(defun etm-open-async (layout-name)
  "Asynchronously open a layout by name."
  (interactive
   (list (completing-read 
          "Open layout: " 
          (mapcar (lambda (file)
                    (string-remove-prefix 
                     "etm-open-" 
                     (string-remove-suffix ".el" (file-name-nondirectory file))))
                  (directory-files etm-layout-save-dir t "etm-open-.*\\.el$")))))
  (message "Opening %s layout..." layout-name)
  ;; Check if function is already available
  (let ((layout-func (intern (concat "etm-open-" layout-name))))
    (if (fboundp layout-func)
        ;; Function already loaded - run with timer
        (run-with-idle-timer 0.1 nil layout-func)
      ;; Function not loaded yet - load file then run
      (let ((layout-file (expand-file-name (concat "etm-open-" layout-name ".el") 
                                          etm-layout-save-dir)))
        (if (file-exists-p layout-file)
            (run-with-idle-timer 
             0.1 nil
             (lambda (file func-name)
               (load file)
               (when (fboundp (intern func-name))
                 (funcall (intern func-name))))
             layout-file
             (concat "etm-open-" layout-name))
          (message "Layout %s not found!" layout-name))))))


;; (defun etm-startup
;;     ()
;;   (interactive)
;;   (tab-rename "default")

;;   ;; Main
;;   ;; (etm-neurovista "spartan")

;;   ;; ;; Removes the first tab
;;   ;; (etm-remove-1)
;;   ;; (etm-navigation-jump-by-index 1)
;;   ;; (etm-close-by-name "default")
;;   )

;; ;; (add-hook 'after-init-hook #'etm-startup)

(provide 'etm-tabs-new-and-rename)

(when
    (not load-file-name)
  (message "etm-tabs-new-and-rename.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
