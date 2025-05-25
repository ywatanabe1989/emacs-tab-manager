# Smart Suggestions for ETM

Smart Suggestions is a machine learning-inspired feature that learns your buffer switching patterns and provides intelligent recommendations.

## Overview

Smart Suggestions tracks how you navigate between buffers and uses this information to predict which buffer you're likely to want next. It considers multiple contextual factors:

- **Current buffer**: What you're looking at now
- **Project context**: The project you're working in
- **Major mode**: The type of file you're editing
- **Time of day**: Morning, afternoon, or evening patterns
- **Remote host**: Which server you're connected to

## Quick Start

1. Enable Smart Suggestions:
   ```elisp
   M-x etm-smart-mode RET
   ```

2. Use normally - ETM will start learning your patterns automatically

3. After a few buffer switches, try the suggestions:
   ```elisp
   C-x t S s    ; Quick switch to top suggestion
   ```

## Usage

### Key Bindings

All Smart Suggestions commands are under the `C-x t S` prefix:

| Key         | Command                          | Description                           |
|-------------|----------------------------------|---------------------------------------|
| `C-x t S s` | `etm-smart-switch-to-suggested` | Switch to the top suggested buffer    |
| `C-x t S S` | `etm-smart-show-suggestions`    | Show all suggestions in a buffer      |
| `C-x t S o` | `etm-smart-ui-show-overlay`     | Display suggestion overlay at point   |
| `C-x t S t` | `etm-smart-toggle`              | Toggle Smart Suggestions on/off       |
| `C-x t S c` | `etm-smart-clear-patterns`      | Clear patterns for current tab        |
| `C-x t S r` | `etm-smart-reset-scores`        | Reset scores for current tab          |

### Suggestion Buffer

When you use `C-x t S S`, a dedicated buffer shows all suggestions:

```
ETM Smart Suggestions
=====================

1. config.el (score: 8.5)
2. test.el (score: 6.2)
3. README.md (score: 4.1)

Press 1-9 to switch to a suggestion
Press q to quit
```

You can quickly jump to any suggestion by pressing its number.

### Visual Overlays

With overlays enabled, ETM can show inline hints when you're idle:

```elisp
(setq etm-smart-ui-use-overlay t)
(setq etm-smart-ui-overlay-delay 0.5)  ; Show after 0.5 seconds
```

### Integration with Completion Frameworks

Smart Suggestions integrates with popular completion frameworks:

- **Ivy**: Suggestions appear at the top of ivy candidates
- **Helm**: Suggestions are highlighted in helm buffer lists
- **Vertico**: Suggestions are annotated with scores
- **Default**: Works with standard Emacs completion

## Configuration

### Basic Settings

```elisp
;; Maximum number of suggestions to show
(setq etm-smart-max-suggestions 5)

;; Minimum confidence score (0.0 to 1.0)
(setq etm-smart-min-confidence 0.3)

;; How quickly old patterns decay (0.0 to 1.0)
(setq etm-smart-decay-factor 0.95)

;; Minimum pattern count before suggesting
(setq etm-smart-min-pattern-count 2)
```

### Privacy Settings

```elisp
;; Buffers matching these patterns won't be tracked
(setq etm-smart-blacklist-patterns
      '("\\*.*\\*"           ; Special buffers
        "COMMIT_EDITMSG"     ; Git commits
        "\\*scratch\\*"      ; Scratch buffer
        "\\.gpg$"))          ; Encrypted files

;; Don't track patterns in these major modes
(setq etm-smart-blacklist-modes
      '(dired-mode
        magit-mode
        help-mode))
```

### UI Settings

```elisp
;; Enable/disable overlay hints
(setq etm-smart-ui-use-overlay t)

;; Delay before showing overlay (seconds)
(setq etm-smart-ui-overlay-delay 0.5)

;; Show scores in completion annotations
(setq etm-smart-show-scores t)

;; Show suggestion count in mode line
(setq etm-smart-show-mode-line t)
```

### Storage Settings

```elisp
;; Where to save pattern data
(setq etm-smart-storage-file
      (expand-file-name "etm-smart-data.el" user-emacs-directory))

;; Auto-save interval (seconds)
(setq etm-smart-auto-save-interval 300)

;; Enable fallback to recent buffers
(setq etm-smart-fallback-to-recent t)
```

## How It Works

### Pattern Tracking

When you switch from buffer A to buffer B, Smart Suggestions records:
- The switch pattern (A → B)
- The current context (project, mode, time, host)
- A timestamp

### Scoring Algorithm

Suggestions are scored based on:

1. **Frequency**: How often you make this switch
2. **Recency**: How recently you made this switch
3. **Context Match**: How well the current context matches

The final score is a weighted combination:
```
score = 0.4 × frequency + 0.4 × recency + 0.2 × context
```

### Context Matching

Context matching considers:
- **Project**: Full match = 1.0, no match = 0.0
- **Major Mode**: Same mode = 1.0, different = 0.0
- **Time of Day**: Same period = 0.5, different = 0.0
- **Remote Host**: Same host = 1.0, different = 0.0

### Privacy Features

- **Local Only**: All data stays on your machine
- **Blacklisting**: Sensitive buffers can be excluded
- **Tab Isolation**: Patterns are tracked per tab
- **Manual Control**: Clear data anytime with `C-x t S c`

## Examples

### Example 1: Code-Test Pattern

If you frequently switch between `main.el` and `test-main.el`, Smart Suggestions will learn this pattern. When you're in `main.el`, it will suggest `test-main.el` as the top recommendation.

### Example 2: Documentation Pattern

When editing code in the morning, you might often reference documentation. Smart Suggestions will learn to suggest `README.md` or `API.md` during morning coding sessions.

### Example 3: Remote Work Pattern

When connected to `server.example.com`, you might work with specific configuration files. Smart Suggestions will recommend these files when you're on that server, but not when working locally.

## Troubleshooting

### Suggestions Not Appearing

1. Check if Smart Suggestions is enabled:
   ```elisp
   M-: etm-smart-enabled RET
   ```

2. Verify you have enough patterns:
   ```elisp
   M-: (hash-table-count etm-smart-patterns) RET
   ```

3. Check minimum confidence setting:
   ```elisp
   M-: etm-smart-min-confidence RET
   ```

### Clearing Bad Patterns

If Smart Suggestions learned incorrect patterns:

1. Clear patterns for current tab:
   ```elisp
   C-x t S c
   ```

2. Or reset just the scores:
   ```elisp
   C-x t S r
   ```

### Performance Issues

If you experience slowdowns:

1. Reduce maximum suggestions:
   ```elisp
   (setq etm-smart-max-suggestions 3)
   ```

2. Increase minimum confidence:
   ```elisp
   (setq etm-smart-min-confidence 0.5)
   ```

3. Disable overlay hints:
   ```elisp
   (setq etm-smart-ui-use-overlay nil)
   ```

## Advanced Usage

### Custom Scoring

You can customize the scoring algorithm:

```elisp
(defun my-etm-smart-score-modifier (pattern context)
  "Boost scores for test files."
  (if (string-match-p "test" (etm-smart-pattern-to-buffer pattern))
      1.2  ; 20% boost for test files
    1.0))

(add-hook 'etm-smart-score-functions #'my-etm-smart-score-modifier)
```

### Export/Import Patterns

Export your patterns to share across machines:

```elisp
;; Export
M-x etm-smart-save-data RET

;; Import (on another machine)
M-x etm-smart-load-data RET
```

### Integration with Other Packages

Smart Suggestions can enhance other packages:

```elisp
;; With projectile
(defun my-projectile-with-suggestions ()
  "Projectile find file with ETM suggestions."
  (interactive)
  (let ((suggestions (etm-smart-suggest-buffers 3)))
    ;; Add suggestions to projectile candidates
    ...))
```

## Future Enhancements

Planned improvements include:

- Group-aware suggestions
- Cross-tab pattern learning
- Time-based pattern weighting
- Negative feedback learning
- Pattern visualization tools

## Contributing

Contributions are welcome! See [CONTRIBUTING.md](../CONTRIBUTING.md) for guidelines.

Key areas for contribution:
- Scoring algorithm improvements
- New context types
- UI enhancements
- Integration with more packages