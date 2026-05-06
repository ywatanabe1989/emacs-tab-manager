---
description: Emacs Tab Manager (ETM) - enhanced tab-bar with buffer type system, navigation, and layout management
globs: ["**/.emacs.d/lisp/emacs-tab-manager/**/*.el"]
---

# emacs-tab-manager (ETM)

ETM extends Emacs' built-in `tab-bar.el` with a buffer type registration system,
named tab navigation, and saved/loaded window layouts with optional SSH remote support.

## Setup

```elisp
(require 'etm)
;; etm-init is called automatically at the end of etm.el
```

## Tab Creation and Renaming

```elisp
;; Create a new tab named "myproject"
;; If "myproject" already exists, returns "myproject<2>" etc.
(etm-new "myproject")   ; -> returns the actual unique tab name (string)

;; Rename the current tab
(etm-rename "newname")
```

## Buffer Type System

ETM registers buffers per-tab under named types. The three built-in types are
`"home"`, `"semi-home"`, and `"results"`. Custom types can be added via
`etm-custom-buffer-types`.

### Registering buffers

```elisp
;; Set the current buffer as the "home" buffer for the current tab
(etm-buffer-set "home")
(etm-buffer-set "semi-home")
(etm-buffer-set "results")

;; Convenience aliases auto-generated for each type:
(etm-buffer-set-home)
(etm-buffer-set-semi-home)
(etm-buffer-set-results)

;; Register a specific buffer for a specific tab key
(etm-buffer-set "home" "my-tab-name" some-buffer-object)
```

`etm-buffer-set` uses the tab's ETM unique ID (preferred) or tab name as the key.

### Navigation by buffer type

```elisp
;; Jump to the "home" buffer in the current tab
(etm-navigation-jump-by-buffer-type "home")
(etm-navigation-jump-by-buffer-type "results")
```

### Navigation by tab name or index

```elisp
;; Jump to a tab by name (completing-read prompt when interactive)
(etm-navigation-jump-by-name "myproject")

;; Jump to tab at numeric index (1-based)
(etm-navigation-jump-by-index 3)

;; Convenience functions for tabs 1-9:
(etm-navigation-jump-to-1)
(etm-navigation-jump-to-2)
;; ... through etm-navigation-jump-to-9

;; Move the current tab N positions in the tab bar
(etm-navigation-move 2)    ; move 2 right
(etm-navigation-move -1)   ; move 1 left
```

## Layout Management

Layouts are saved as elisp files under `etm-layout-save-dir` (default:
`src/etm-layout/saved-layouts/`). Each file defines a function `etm-open-<name>`.

### Creating layouts programmatically

`--etm-layout-create-from-positions` is the core layout builder:

```elisp
;; window-specs format: (type path x y width height [path-host])
;; type: 'file or 'shell
;; x, y: column/row position (0-based, for sort order)
(--etm-layout-create-from-positions
 "my-tab"
 '((file  "/path/to/file.org"  0 0 80 50)
   (shell "~/proj/myrepo"      1 0 80 50)
   (shell "~/proj/myrepo"      1 1 80 25))
 nil)   ; host: nil for local, "hostname" for remote SSH
```

Window at position (x=0, y=0) gets `"home"` type; other file windows get `"semi-home"`.

### Opening saved layouts asynchronously

```elisp
(etm-open-async "scitex-python")  ; opens saved layout by name
```

### Save/load aliases

```elisp
(save-layout)          ; alias for etm-layout-save
(save-layout-startup)  ; alias for etm-startup-edit-layouts
```

## Key internal variables

| Variable | Purpose |
|---|---|
| `etm-registered-buffers` | Alist of `(tab-key (type . buffer-name) ...)` |
| `etm-registered-buffer-types` | List of built-in types: `("home" "semi-home" "results")` |
| `etm-custom-buffer-types` | User-defined extra types |
| `etm-layout-save-dir` | Directory for saved layout `.el` files |
| `etm-layout-default-hosts` | Hash table: tab-name -> default SSH host |
| `etm-layout-auto-register-numeric` | If non-nil, auto-assign numeric buffer IDs |

## Typical usage pattern (e.g. in ecc-monitoring-tab)

```elisp
(let ((tab-name (etm-new "Claude")))
  (delete-other-windows)
  (find-file "~/status.org")
  (etm-buffer-set "home")
  (split-window-right)
  (other-window 1)
  (switch-to-buffer "*Repo Monitor*")
  (etm-buffer-set "semi-home")
  (other-window 1)
  (switch-to-buffer my-vterm-buf)
  (etm-buffer-set "results"))
```
