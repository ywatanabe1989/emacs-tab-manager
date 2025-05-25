<!-- ---
!-- Timestamp: 2025-05-09 23:56:58
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/README.md
!-- --- -->

# Emacs Tab Manager (ETM)

[![Build Status](https://github.com/ywatanabe1989/emacs-tab-manager/workflows/run_tests/badge.svg)](https://github.com/ywatanabe1989/emacs-tab-manager/actions)

A powerful extension for Emacs [tab-bar.el](https://github.com/emacs-mirror/emacs/blob/master/lisp/tab-bar.el) that enhances tab and buffer management with type-based organization.

📚 **[Quick Start Guide](docs/QUICK-START.md)** | 🧠 **[Smart Suggestions](docs/SMART-SUGGESTIONS.md)** | 🏗️ **[Architecture](ARCHITECTURE.md)** | 🤝 **[Contributing](CONTRIBUTING.md)**

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

- **Enhanced Remote Support** (NEW in v2.4.0)
  - Multi-method TRAMP connection management (SSH, FTP, sudo, docker, kubernetes)
  - Visual indicators for remote connections in tab names, buffer names, and mode-line
  - Remote-aware navigation commands
  - Automatic connection health monitoring
  - Persistent remote connections with layouts

- **Smart Suggestions** (NEW in v2.5.0)
  - Machine learning-inspired buffer recommendations
  - Context-aware suggestions based on project, mode, time, and remote host
  - Privacy-focused local-only pattern tracking
  - Adaptive scoring algorithm
  - Integration with completion frameworks (ivy, helm, vertico)
  - Visual overlay hints for quick switching

- **Layout Management**
  - Save and load window configurations
  - Remote host support with path mirroring
  - Persistent layouts across sessions
  - Automatic remote connection restoration

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

### Enhanced Remote Support (NEW in v2.4.0)

ETM now provides comprehensive support for working with remote files via TRAMP:

```elisp
;; Remote navigation
C-x t r j    ; Jump to remote host
C-x t r n    ; Next remote buffer
C-x t r p    ; Previous remote buffer
C-x t r l    ; List all remote buffers
C-x t r L    ; Switch to local buffer

;; Remote features
- Tab names show connected hosts (e.g., "main [@server.com]")
- Buffer names prefixed with host (e.g., "[server.com] file.txt")
- Mode line indicators show connection status
- Automatic connection health monitoring
- Connection state saved with layouts
```

Remote connections are managed per-tab and support multiple simultaneous connections. Visual indicators help you track which host you're working on:
- 🟢 Connected
- 🟡 Connecting
- 🔴 Error
- ⚪ Disconnected

### Smart Suggestions (NEW in v2.5.0)

ETM learns your buffer switching patterns and provides intelligent suggestions:

```elisp
;; Enable Smart Suggestions
M-x etm-smart-mode RET

;; Smart commands
C-x t S s    ; Switch to top suggested buffer
C-x t S S    ; Show all suggestions in dedicated buffer
C-x t S o    ; Show suggestion overlay at point
C-x t S t    ; Toggle Smart Suggestions on/off
C-x t S c    ; Clear patterns for current tab
C-x t S r    ; Reset scores for current tab

;; Configuration
(setq etm-smart-max-suggestions 5)      ; Number of suggestions to show
(setq etm-smart-min-confidence 0.3)     ; Minimum score threshold
(setq etm-smart-decay-factor 0.95)      ; Score decay over time
(setq etm-smart-ui-use-overlay t)       ; Enable visual overlay hints
```

Smart Suggestions tracks your buffer switching patterns per tab and uses context (project, major mode, time of day, remote host) to provide relevant suggestions. All data is stored locally for privacy.

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