# ETM Quick Start Guide

Get up and running with Emacs Tab Manager in 5 minutes!

## Installation

### Using straight.el
```elisp
(straight-use-package
 '(emacs-tab-manager :type git :host github :repo "ywatanabe1989/emacs-tab-manager"))
(require 'etm)
(etm-init)
```

### Manual Installation
```elisp
(add-to-list 'load-path "/path/to/emacs-tab-manager")
(require 'etm)
(etm-init)
```

## Basic Usage

### 1. Tab Navigation
- `M-1` to `M-9`: Jump to tab by number
- `M-t n`: Create new tab
- `M-t r`: Rename current tab
- `M-t 0`: Close current tab

### 2. Buffer Types
Each tab can organize buffers into types:

#### Setting Buffer Types
- `M-H`: Set current buffer as "home" (main file)
- `M-S`: Set as "semi-home" (related files)
- `M-R`: Set as "results" (output buffers)

#### Jumping to Buffer Types
- `M-h`: Jump to home buffer
- `M-s`: Cycle through semi-home buffers
- `M-r`: Cycle through results buffers

### 3. Numeric Buffers (NEW!)
Quick access to frequently used buffers:

- `C-0` to `C-9`: Jump to numeric buffer
- `M-t 0` to `M-t 9`: Assign current buffer to slot

Visual indicators show occupied slots in tab names.

### 4. Layout Management

#### Save Current Layout
```
M-x etm-layout-save RET
Layout name: my-workspace RET
Host (or empty): RET
```

#### Preview Layouts
- `M-t P`: List all layouts with preview
- `M-t p`: Preview specific layout

#### Load Layout
```
M-x my-workspace RET
```

## Common Workflows

### Development Workflow
```elisp
;; 1. Create development tab
M-t n "MyProject" RET

;; 2. Open main file and set as home
C-x C-f main.py RET
M-H

;; 3. Open test file and set as semi-home
C-x C-f test_main.py RET
M-S

;; 4. Run tests, output goes to results
M-x compile RET
M-R

;; 5. Save layout for later
M-x etm-layout-save RET "python-dev" RET
```

### Research Workflow
```elisp
;; 1. Set up numeric buffers for quick access
C-x C-f paper.org RET
M-t 1                    ; Assign to slot 1

C-x C-f references.bib RET
M-t 2                    ; Assign to slot 2

;; 2. Quick switching
C-1                      ; Jump to paper.org
C-2                      ; Jump to references.bib
```

### Remote Development
```elisp
;; 1. Save layout with remote host
M-x etm-layout-save RET
Layout name: remote-dev RET
Host: myserver RET

;; 2. Load layout (auto-connects SSH)
M-x remote-dev RET
```

## Tips & Tricks

### 1. Protected Buffers
Prevent accidental killing of important buffers:
```elisp
(setq etm-protected-buffers '("*scratch*" "*Messages*" "COMMIT_EDITMSG"))
```

### 2. Custom Buffer Types
Add your own buffer types:
```elisp
(setq etm-custom-buffer-types '("docs" "config"))

;; Create jump function
(etm-buffer-define-buffer-type-jumper-function "docs")
(define-key etm-command-map (kbd "d") #'etm-navigation-jump-by-buffer-type-docs)

;; Create setter function
(etm-buffer-define-buffer-type-setter-function "docs")
(define-key etm-command-map (kbd "D") #'etm-buffer-set-docs)
```

### 3. Layout Preview Keys
In layout list (`M-t P`):
- `RET`: Load layout
- `SPC` or `p`: Preview layout
- `n`/`p`: Navigate list
- `q`: Quit

### 4. Visual Customization
```elisp
;; Customize numeric indicators
(setq etm-numeric-indicator-format "(%s)")  ; Use parentheses
(setq etm-numeric-indicator-separator "-")  ; Use dashes
```

## Troubleshooting

### Buffers Not Jumping
- Ensure buffer is registered: Check with `M-x describe-variable RET etm-registered-buffers`
- Verify you're in the correct tab

### Layout Not Loading
- Check layout file exists in `etm-layout-save-dir`
- Ensure all paths in layout are accessible
- For remote hosts, verify SSH connection

### Keys Not Working
- Run `M-x etm-init` to ensure proper initialization
- Check key conflicts with `C-h k` followed by the key combination

## Next Steps

1. Read the full [README](../README.md) for comprehensive documentation
2. See [CONTRIBUTING](../CONTRIBUTING.md) to extend ETM
3. Check [examples](examples/) for advanced usage patterns
4. Join discussions on [GitHub](https://github.com/ywatanabe1989/emacs-tab-manager)

Happy tab managing! 🎉