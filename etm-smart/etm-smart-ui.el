;;; etm-smart-ui.el --- UI integration for ETM smart suggestions -*- coding: utf-8; lexical-binding: t -*-

;; Author: ywatanabe
;; Date: 2025-05-26
;; Version: 1.0.0

;;; Commentary:
;; This module provides UI integration for ETM smart suggestions,
;; including minibuffer enhancements, overlays, and visual feedback.

;;; Code:

(require 'cl-lib)
(require 'etm-smart-suggest)
(require 'etm-smart-patterns)

;;; Variables

(defvar etm-smart-ui-suggestion-overlay nil
  "Overlay for displaying inline suggestions.")

(defvar etm-smart-ui-use-overlay t
  "Whether to use overlay hints for suggestions.")

(defvar etm-smart-ui-overlay-delay 0.5
  "Delay before showing suggestion overlay.")

(defvar etm-smart-ui-overlay-timer nil
  "Timer for delayed overlay display.")

(defface etm-smart-suggestion-face
  '((t :inherit completions-annotations :slant italic))
  "Face for smart suggestion annotations.")

(defface etm-smart-suggestion-overlay-face
  '((t :inherit shadow :box (:line-width -1 :style released-button)))
  "Face for smart suggestion overlay.")

(defvar etm-smart-ui-suggestion-keymap
  (let ((map (make-sparse-keymap)))
    (dotimes (i 9)
      (let ((n (1+ i)))
        (define-key map (kbd (format "%d" n))
          (lambda ()
            (interactive)
            (etm-smart-suggest-switch-to-number n)))))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "RET") #'etm-smart-ui-select-suggestion-at-point)
    map)
  "Keymap for ETM Smart Suggestions buffer.")

(defun etm-smart-ui-select-suggestion-at-point ()
  "Select the suggestion at point in the suggestions buffer."
  (interactive)
  (let ((buffer-name (get-text-property (point) 'buffer-name)))
    (when buffer-name
      (switch-to-buffer buffer-name)
      (quit-window))))

;;; Minibuffer Enhancement

(defun etm-smart-ui-setup-minibuffer ()
  "Set up smart suggestions in minibuffer."
  (when (and (boundp 'completing-read-function)
             (eq this-command 'switch-to-buffer))
    (setq-local completion-extra-properties
                (plist-put completion-extra-properties
                           :annotation-function
                           #'etm-smart-annotate-completion))))

(add-hook 'minibuffer-setup-hook #'etm-smart-ui-setup-minibuffer)

;;; Overlay Management

(defun etm-smart-ui-show-overlay ()
  "Show suggestion overlay near point."
  (interactive)
  (when etm-smart-ui-use-overlay
    (etm-smart-ui-cancel-overlay-timer)
    (setq etm-smart-ui-overlay-timer
          (run-with-timer etm-smart-ui-overlay-delay nil
                          #'etm-smart-ui-display-overlay))))

(defun etm-smart-ui-display-overlay ()
  "Actually display the overlay with suggestions."
  (let ((suggestions (etm-smart-suggest-buffers 1)))
    (when suggestions
      (etm-smart-ui-remove-overlay)
      (let* ((suggestion (caar suggestions))
             (score (cdar suggestions))
             (text (format " → %s [%d%%]" 
                           (truncate-string-to-width suggestion 20 nil nil "…")
                           (round (* 100 score))))
             (ov (make-overlay (point) (point))))
        (overlay-put ov 'after-string 
                     (propertize text 'face 'etm-smart-suggestion-overlay-face))
        (overlay-put ov 'evaporate t)
        (setq etm-smart-ui-suggestion-overlay ov)))))

(defun etm-smart-ui-remove-overlay ()
  "Remove suggestion overlay if present."
  (when (overlayp etm-smart-ui-suggestion-overlay)
    (delete-overlay etm-smart-ui-suggestion-overlay)
    (setq etm-smart-ui-suggestion-overlay nil)))

(defun etm-smart-ui-cancel-overlay-timer ()
  "Cancel pending overlay timer."
  (when etm-smart-ui-overlay-timer
    (cancel-timer etm-smart-ui-overlay-timer)
    (setq etm-smart-ui-overlay-timer nil)))

;;; Tab Bar Integration

(defun etm-smart-ui-tab-bar-indicator ()
  "Generate tab bar indicator for smart suggestions."
  (when (and (fboundp 'tab-bar-mode)
             tab-bar-mode)
    (let ((suggestions (etm-smart-suggest-buffers 1)))
      (if suggestions
          (format "[→%s]" (truncate-string-to-width 
                           (caar suggestions) 8 nil nil "…"))
        ""))))

;;; Transient Suggestion Display

(defvar etm-smart-ui-transient-timer nil
  "Timer for transient suggestion display.")

(defun etm-smart-ui-show-transient (message &optional seconds)
  "Show transient MESSAGE for SECONDS (default 2)."
  (when etm-smart-ui-transient-timer
    (cancel-timer etm-smart-ui-transient-timer))
  (message "%s" message)
  (setq etm-smart-ui-transient-timer
        (run-with-timer (or seconds 2) nil
                        (lambda () (message "")))))

;;; Completion System Integration

(defun etm-smart-ui-completion-setup ()
  "Set up smart completion integration."
  ;; Integrate with ivy if available
  (when (fboundp 'ivy-read)
    (advice-add 'ivy-read :filter-args #'etm-smart-ui-ivy-setup))
  
  ;; Integrate with helm if available
  (when (fboundp 'helm)
    (advice-add 'helm :before #'etm-smart-ui-helm-setup))
  
  ;; Integrate with vertico if available
  (when (fboundp 'vertico-mode)
    (add-hook 'vertico-mode-hook #'etm-smart-ui-vertico-setup)))

(defun etm-smart-ui-ivy-setup (args)
  "Set up Ivy integration. ARGS are ivy-read arguments."
  (when (equal (car args) "Switch to buffer: ")
    ;; Modify collection to prioritize suggestions
    (let ((suggestions (etm-smart-suggest-buffers)))
      (when suggestions
        (let ((collection (cadr args)))
          (setf (cadr args)
                (etm-smart-ui-prioritize-collection collection suggestions))))))
  args)

(defun etm-smart-ui-helm-setup (&rest _)
  "Set up Helm integration."
  ;; Add smart source to helm
  (when (boundp 'helm-source-buffers-list)
    (setq helm-source-buffers-list
          (append '((candidates . etm-smart-ui-helm-candidates))
                  helm-source-buffers-list))))

(defun etm-smart-ui-vertico-setup ()
  "Set up Vertico integration."
  (when (boundp 'vertico-sort-function)
    (setq-local vertico-sort-function #'etm-smart-ui-vertico-sort)))

(defun etm-smart-ui-prioritize-collection (collection suggestions)
  "Prioritize COLLECTION based on SUGGESTIONS."
  (let ((suggestion-names (mapcar #'car suggestions))
        (remaining collection))
    ;; Remove suggestions from remaining
    (setq remaining (cl-remove-if (lambda (item)
                                    (member (if (consp item) (car item) item)
                                            suggestion-names))
                                  remaining))
    ;; Prepend suggestions
    (append suggestion-names remaining)))

;;; Visual Feedback

(defun etm-smart-ui-flash-suggestion (buffer-name)
  "Flash visual feedback when switching to suggested BUFFER-NAME."
  (let ((msg (format "Smart switch → %s" buffer-name)))
    (etm-smart-ui-show-transient msg 1.5)))

(defun etm-smart-ui-pulse-line ()
  "Pulse current line to indicate smart action."
  (when (fboundp 'pulse-momentary-highlight-one-line)
    (pulse-momentary-highlight-one-line (point))))

;;; Smart Actions

(defun etm-smart-ui-accept-suggestion ()
  "Accept the top suggestion."
  (interactive)
  (let ((suggestions (etm-smart-suggest-buffers 1)))
    (when suggestions
      (let ((buffer-name (caar suggestions)))
        (etm-smart-learn-from-feedback (car suggestions) t)
        (switch-to-buffer buffer-name)
        (etm-smart-ui-flash-suggestion buffer-name)
        (etm-smart-ui-pulse-line)))))

(defun etm-smart-ui-cycle-suggestions ()
  "Cycle through suggestions interactively."
  (interactive)
  (let ((suggestions (etm-smart-suggest-buffers 5)))
    (when suggestions
      (let* ((names (mapcar #'car suggestions))
             (current (buffer-name))
             (next-idx (mod (1+ (or (cl-position current names :test #'equal) -1))
                            (length names))))
        (switch-to-buffer (nth next-idx names))
        (message "Smart suggestion %d/%d: %s" 
                 (1+ next-idx) (length names) (nth next-idx names))))))

;;; Smart Keybindings

(defvar etm-smart-ui-map (make-sparse-keymap)
  "Keymap for smart UI commands.")

(define-key etm-smart-ui-map (kbd "C-c C-s") #'etm-smart-ui-accept-suggestion)
(define-key etm-smart-ui-map (kbd "C-c C-n") #'etm-smart-ui-cycle-suggestions)
(define-key etm-smart-ui-map (kbd "C-c C-?") #'etm-smart-show-suggestions)

;;; Mode Line Integration

(defvar etm-smart-ui-mode-line-string ""
  "String to display in mode line.")

(defun etm-smart-ui-update-mode-line ()
  "Update mode line with smart suggestion indicator."
  (setq etm-smart-ui-mode-line-string
        (if etm-smart-show-mode-line
            (etm-smart-mode-line-indicator)
          "")))

(add-hook 'buffer-list-update-hook #'etm-smart-ui-update-mode-line)

;;; Initialization

(defun etm-smart-ui-init ()
  "Initialize smart UI integration."
  (interactive)
  ;; Set up completion system integration
  (etm-smart-ui-completion-setup)
  
  ;; Add mode line indicator if not already present
  (unless (memq 'etm-smart-ui-mode-line-string mode-line-misc-info)
    (push '(etm-smart-ui-mode-line-string etm-smart-ui-mode-line-string)
          mode-line-misc-info))
  
  ;; Set up hooks for overlay display
  (add-hook 'post-command-hook #'etm-smart-ui-maybe-show-overlay)
  
  (message "ETM Smart UI initialized"))

(defun etm-smart-ui-cleanup ()
  "Clean up smart UI integration."
  (interactive)
  ;; Remove overlays
  (etm-smart-ui-remove-overlay)
  (etm-smart-ui-cancel-overlay-timer)
  
  ;; Remove from mode line
  (setq mode-line-misc-info
        (delq 'etm-smart-ui-mode-line-string mode-line-misc-info))
  
  ;; Remove hooks
  (remove-hook 'minibuffer-setup-hook #'etm-smart-ui-setup-minibuffer)
  (remove-hook 'post-command-hook #'etm-smart-ui-maybe-show-overlay)
  (remove-hook 'buffer-list-update-hook #'etm-smart-ui-update-mode-line))

(defun etm-smart-ui-maybe-show-overlay ()
  "Maybe show overlay after certain commands."
  (when (and etm-smart-ui-use-overlay
             (memq this-command '(previous-line next-line
                                  forward-char backward-char
                                  move-beginning-of-line move-end-of-line)))
    ;; Cancel any pending timer and start a new one
    (etm-smart-ui-cancel-overlay-timer)
    (setq etm-smart-ui-overlay-timer
          (run-with-idle-timer etm-smart-ui-overlay-delay nil
                               #'etm-smart-ui-display-overlay))))

(provide 'etm-smart-ui)
;;; etm-smart-ui.el ends here