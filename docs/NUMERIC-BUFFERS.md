# ETM Numeric Buffer System

## Overview

The ETM Numeric Buffer System provides quick access to frequently used buffers through numeric keybindings. This feature allows you to jump to buffers using `M-t 1` through `M-t 9`, making navigation extremely fast.

## Quick Start

### Manual Registration
1. Switch to a buffer you want quick access to
2. Press `M-t b r` to register it
3. The buffer gets assigned the next available number (1-9)
4. Jump to it anytime with `M-t <number>`

### Automatic Registration (NEW)
When you open a layout using `etm-open-*` commands:
- The first 9 file buffers are automatically registered
- You can immediately use `M-t 1`, `M-t 2`, `M-t 3` to jump to them
- No manual registration needed!

## Key Bindings

| Key | Function | Description |
|-----|----------|-------------|
| `M-t b r` | Register | Register current buffer with next available ID |
| `M-t b l` | List | Show all registered buffers |
| `M-t b c` | Cleanup | Remove dead buffer entries |
| `M-t b ?` | Help | Show quick help |
| `M-t 1-9` | Jump | Jump directly to buffer by ID |
| `M-t b 1-9` | Jump (alt) | Alternative jump method |

## Configuration

### Enable/Disable Automatic Registration
```elisp
;; Enable automatic registration (default)
(setq etm-layout-auto-register-numeric t)

;; Disable automatic registration
(setq etm-layout-auto-register-numeric nil)
```

### Configure Number of Auto-Registered Buffers
```elisp
;; Register first 5 buffers (default is 9)
(setq etm-layout-auto-register-max 5)

;; Only register the first buffer
(setq etm-layout-auto-register-max 1)
```

### Configure Maximum Numeric Buffers
```elisp
;; Allow up to 9 numeric buffers per tab (default)
(setq etm-max-numeric-buffers 9)
```

## Usage Examples

### Example 1: Manual Registration Workflow
```
1. Open your main project file
2. M-t b r          ; Register as buffer #1
3. Open your test file
4. M-t b r          ; Register as buffer #2
5. Open documentation
6. M-t b r          ; Register as buffer #3

Now you can:
- M-t 1            ; Jump to main project file
- M-t 2            ; Jump to test file
- M-t 3            ; Jump to documentation
```

### Example 2: Layout with Auto-Registration
```
1. M-x etm-open-myproject RET
   ; Layout opens with 3 files automatically registered
2. M-t 1            ; Jump to first file
3. M-t 2            ; Jump to second file
4. M-t 3            ; Jump to third file
5. M-t b l          ; List all registered buffers
```

### Example 3: Mixed Manual and Auto Registration
```
1. M-x etm-open-myproject RET
   ; First 9 files auto-registered as 1-9
2. Open a new important file
3. M-t b r          ; Manually register as buffer #4
4. M-t b l          ; See all 4 registered buffers
```

## Important Notes

1. **Per-Tab Registration**: Each tab has its own set of numeric buffers
2. **No Auto-Registration for Shells**: Only file buffers are auto-registered
3. **First Come, First Served**: Auto-registration happens in the order windows are created
4. **Cleanup**: Dead buffers are automatically cleaned up periodically
5. **Persistence**: Numeric registrations are not saved between Emacs sessions

## Troubleshooting

### "No buffer registered with ID X"
- Use `M-t b l` to see what's registered
- Use `M-t b r` to register the current buffer
- Check if auto-registration is enabled

### Auto-registration not working
- Verify `etm-layout-auto-register-numeric` is `t`
- Check that you're opening a layout (not creating tabs manually)
- Ensure the layout contains file buffers (not just shells)

### Buffer registered but can't jump
- The buffer might have been killed
- Run `M-t b c` to clean up dead entries
- Re-register if needed with `M-t b r`

## Advanced Usage

### Selective Auto-Registration
If you want to control which layouts use auto-registration:

```elisp
(defun my-etm-open-with-auto-register (layout-name)
  "Open layout with auto-registration enabled."
  (let ((etm-layout-auto-register-numeric t))
    (etm-layout-open layout-name)))

(defun my-etm-open-without-auto-register (layout-name)
  "Open layout with auto-registration disabled."
  (let ((etm-layout-auto-register-numeric nil))
    (etm-layout-open layout-name)))
```

### Hook for Custom Registration
```elisp
(add-hook 'etm-layout-load-hook
          (lambda ()
            ;; Custom logic after layout loads
            (when (string-match-p "important" (buffer-name))
              (etm-numeric-register-current-buffer))))
```

## See Also
- [ETM Layout System](../README.md#layout-management)
- [ETM Buffer Types](../README.md#buffer-management)