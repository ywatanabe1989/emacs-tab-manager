<!-- ---
!-- Timestamp: 2025-02-26 16:36:29
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/README.md
!-- --- -->

# Emacs Tab Manager (ETM)

[![Build Status](https://github.com/ywatanabe1989/emacs-tab-manager/workflows/tests/badge.svg)](https://github.com/ywatanabe1989/emacs-header-footer/actions)

Extension for [tab-bar.el](https://github.com/emacs-mirror/emacs/blob/master/lisp/tab-bar.el)

## Features
  - Buffer type system (home, semi-home, results by default)
    - Register buffers with types per tab
    - Navigate between typed buffers
    - Smart buffer killing (kill/hide based on registration)
  - Layout management
    - Save/load window configurations
    - Remote host support with path mirroring

## Installation

```elisp
;; Add to load path
(add-to-list 'load-path "/path/to/etm")
(require 'etm)
(etm-init) ;; Enhanced tab-bar mode enabled
```

## Usage

### Buffer Management
```elisp
M-h/s/r  ; Jump to home/semi-home/results buffer
M-H/S/R  ; Set current buffer as home/semi-home/results
```

### Tab 
```elisp
M-1..9   ; Jump to tab by index
M-t      ; Enables `etm-command-map'
M-t 0    ; Close current tab
M-t 1    ; Close other tabs
M-t 2/n  ; Create new tab with name allocating
M-t k    ; Kill or hide if the buffer is linked to a buffer type
M-t r    ; Rename tab
```

## Customizing Buffer Types

```elisp
;; Add custom buffer types
(setq etm-custom-buffer-types '("docs" "tests"))

;; Protected buffers
(setq etm-protected-buffers '("*scratch*" "*Messages*")) ; Not killed but hidden by `etm-buffer-kill-or-bury'

;; Key bindings
(etm-buffer-define-buffer-type-jumper-function "docs") ;; Defines etm-navigation-jump-by-buffer-type-docs
(etm-buffer-define-buffer-type-setter-function "docs") ;; Defines etm-buffer-set-docs
(define-key etm-command-map (kbd "d") #'etm-navigation-jump-by-buffer-type-docs)
(define-key etm-command-map (kbd "D") #'etm-buffer-set-docs)
```

## Layout Management

```elisp
; Saving
M-x etm-layout-save RET my-layout RET
;; Creates etm-open-my-layout.el in etm-layout-save-dir
;; Default: emacs-tab-manager/etm-layout/saved-layouts/

;; Loading
M-x etm-open-my-layout   ; Load saved layout
M-x my-layout RET        ; Load saved layout (alias)
```

<!-- EOF -->