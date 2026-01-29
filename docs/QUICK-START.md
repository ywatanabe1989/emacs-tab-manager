# Quick Start Guide

## Installation

```elisp
(add-to-list 'load-path "/path/to/emacs-tab-manager")
(require 'etm)
(etm-init)
```

## Essential Keybindings

### Buffer Navigation
| Key | Action |
|-----|--------|
| `M-h` | Jump to home buffer |
| `M-s` | Jump to semi-home buffer |
| `M-r` | Jump to results buffer |
| `M-H` | Set current as home |
| `M-S` | Set current as semi-home |
| `M-R` | Set current as results |

### Numeric Buffers
| Key | Action |
|-----|--------|
| `C-1` to `C-9` | Jump to buffer 1-9 |
| `M-t b r` | Register current buffer |
| `M-t b l` | List numeric buffers |

### Tab Management
| Key | Action |
|-----|--------|
| `M-1` to `M-9` | Jump to tab by index |
| `M-t n` | New tab |
| `M-t r` | Rename tab |
| `M-t 0` | Close current tab |

### Layouts
| Key | Action |
|-----|--------|
| `M-t P` | List/preview layouts |
| `M-t p` | Preview layout |
| `M-x etm-layout-save` | Save current layout |

## First Steps

1. **Create a tab**: `M-t n` and enter a name
2. **Set home buffer**: Open a file, press `M-H`
3. **Register numeric buffers**: Open files, press `M-t b r` for each
4. **Save layout**: `M-x etm-layout-save RET my-layout RET`
5. **Load later**: `M-x etm-open-my-layout`
