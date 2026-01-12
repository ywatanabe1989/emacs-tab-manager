;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-09-30 19:49:16>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/etm-tabs/etm-tabs-new-and-rename.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

;;;###autoload

(defun --etm-get-existing-tab-names ()
  "Get list of all existing tab names."
  (mapcar (lambda (tab) (alist-get 'name tab))
          (tab-bar-tabs)))

(defun --etm-make-unique-tab-name (base-name)
  "Make BASE-NAME unique by adding <N> suffix if needed."
  (let ((existing-names (--etm-get-existing-tab-names))
        (unique-name base-name)
        (counter 2))
    (while (member unique-name existing-names)
      (setq unique-name (format "%s<%d>" base-name counter))
      (setq counter (1+ counter)))
    unique-name))

(defun etm-new (arg)
  "Create a new tab with name ARG.
If a tab with the same name exists, adds <N> suffix to make it unique."
  (interactive
   (list
    (read-string "Enter tab name: ")))
  (let ((unique-name (--etm-make-unique-tab-name arg)))
    (tab-new)
    (tab-rename unique-name)
    (when (not (string= arg unique-name))
      (message "Tab renamed to '%s' (original '%s' already exists)"
               unique-name arg))
    unique-name))

;;;###autoload

(defun etm-rename
    (arg)
  "Rename current tab to ARG."
  (interactive
   (list
    (read-string "Enter tab name: ")))
  (tab-rename
   (message "%s" arg)))

(defun etm-open-async (layout-name)
  "Asynchronously open a layout by name."
  (interactive
   (list (completing-read
          "Open layout: "
          (mapcar (lambda (file)
                    (string-remove-prefix
                     "etm-open-"
                     (string-remove-suffix ".el"
                                           (file-name-nondirectory
                                            file))))
                  (directory-files etm-layout-save-dir t
                                   "etm-open-.*\\.el$")))))
  (message "Opening %s layout..." layout-name)
  ;; Check if function is already available
  (let ((layout-func (intern (concat "etm-open-" layout-name))))
    (if (fboundp layout-func)
        ;; Function already loaded - run with timer
        (run-with-idle-timer 0.1 nil layout-func)
      ;; Function not loaded yet - load file then run
      (let ((layout-file (expand-file-name
                          (concat "etm-open-" layout-name ".el")
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
