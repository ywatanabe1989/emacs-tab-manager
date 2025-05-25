<!-- ---
!-- Timestamp: 2025-05-09 23:56:58
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/README.md
!-- --- -->

# Emacs Tab Manager (ETM)

[![Build Status](https://github.com/ywatanabe1989/emacs-tab-manager/workflows/run_tests/badge.svg)](https://github.com/ywatanabe1989/emacs-tab-manager/actions)

A powerful extension for Emacs [tab-bar.el](https://github.com/emacs-mirror/emacs/blob/master/lisp/tab-bar.el) that enhances tab and buffer management with type-based organization.

📚 **[Quick Start Guide](docs/QUICK-START.md)** | 🏗️ **[Architecture](ARCHITECTURE.md)** | 🤝 **[Contributing](CONTRIBUTING.md)**

## Features

- **Buffer Type System** (home, semi-home, results by default)
  - Register buffers with specific types per tab
  - Navigate efficiently between typed buffers
  - Smart buffer killing (kill/hide based on registration status)

- **Numeric Buffer System** (NEW in v2.1.0)
  - Register buffers with numeric keys (0-9) for quick access
  - Per-tab numeric buffer assignments
  - Jump to numeric buffers with C-0 through C-9

- **Buffer Groups** (NEW in v2.3.0)
  - Organize related buffers into named groups
  - Tab-specific groups with multi-group support
  - Quick navigation within and between groups
  - Visual indicators in tab-bar and mode-line

- **Layout Management**
  - Save and load window configurations
  - Remote host support with path mirroring
  - Persistent layouts across sessions

## Installation

```elisp
;; Add to load path
(add-to-list 'load-path "/path/to/etm")
(require 'etm)
(etm-init) ;; Enables enhanced tab-bar mode
```

## Usage

### Buffer Management

```elisp
M-h/s/r  ; Jump to home/semi-home/results buffer
M-H/S/R  ; Set current buffer as home/semi-home/results
```

### Numeric Buffer System

```elisp
C-0..C-9 ; Jump to numeric buffer 0-9
M-t 0..9 ; Register current buffer as numeric buffer 0-9
```

### Buffer Groups (NEW in v2.3.0)

Organize related buffers into named groups for efficient navigation:

```elisp
;; Group management
M-t g c  ; Create new group
M-t g d  ; Delete group
M-t g a  ; Add current buffer to group
M-t g r  ; Remove current buffer from group

;; Navigation
M-t g n  ; Next buffer in group
M-t g p  ; Previous buffer in group
M-t g g  ; Switch to group (first buffer)
M-t g N  ; Cycle to next group
M-t g P  ; Cycle to previous group

;; Information
M-t g l  ; List all groups and buffers
M-t g ?  ; Show groups for current buffer
M-t g i  ; Toggle group indicators
```

Groups are tab-specific and buffers can belong to multiple groups. Visual indicators show group membership in the tab-bar and mode-line.

### Tab Navigation and Management

```elisp
M-1..9   ; Jump to tab by index
M-t      ; Activates `etm-command-map` prefix
M-t 0    ; Close current tab
M-t 1    ; Close other tabs
M-t 2/n  ; Create new tab with name allocation
M-t k    ; Kill or hide if buffer is linked to a buffer type
M-t r    ; Rename current tab
```

## Customizing Buffer Types

```elisp
;; Add custom buffer types
(setq etm-custom-buffer-types '("docs" "tests"))

;; Set protected buffers (not killed but hidden by `etm-buffer-kill-or-bury`)
(setq etm-protected-buffers '("*scratch*" "*Messages*"))

;; Create and bind custom buffer type functions
(etm-buffer-define-buffer-type-jumper-function "docs") ;; Creates etm-navigation-jump-by-buffer-type-docs
(etm-buffer-define-buffer-type-setter-function "docs") ;; Creates etm-buffer-set-docs
(define-key etm-command-map (kbd "d") #'etm-navigation-jump-by-buffer-type-docs)
(define-key etm-command-map (kbd "D") #'etm-buffer-set-docs)
```

## Layout Management

```elisp
;; Saving layouts
M-x etm-layout-save RET my-layout RET
;; Creates etm-open-my-layout.el in etm-layout-save-dir
;; Default location: emacs-tab-manager/etm-layout/saved-layouts/

;; Preview layouts (NEW in v2.2.0)
M-t P            ; List all layouts with preview capability
M-t p            ; Preview specific layout
M-t l            ; Alternative key for layout list

;; Loading layouts
M-x etm-open-my-layout   ; Load saved layout
M-x my-layout RET        ; Load saved layout (alias)
```

### Layout Preview System
The layout preview feature allows you to see layout contents before loading:
- ASCII diagram showing window arrangement
- Window types (file/shell) and paths
- Remote host information
- Navigate with RET to load, SPC/p to preview

<!-- EOF -->