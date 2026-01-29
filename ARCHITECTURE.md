# Architecture

## Module Structure

```
emacs-tab-manager/
├── etm.el                 # Main entry point
└── src/
    ├── etm-core/          # Core variables, helpers, SSH
    ├── etm-buffer/        # Buffer type system, numeric buffers
    ├── etm-tabs/          # Tab creation and management
    ├── etm-layout/        # Layout save/load/preview
    ├── etm-keys/          # Keybinding definitions
    ├── etm-close/         # Buffer closing logic
    ├── etm-groups/        # Buffer group management
    ├── etm-remote/        # TRAMP/SSH support
    └── etm-email/         # Email integration
```

## Key Components

### Buffer Type System (`etm-buffer/`)
- Registers buffers with types (home, semi-home, results)
- Per-tab buffer associations
- Numeric buffer system (0-9 quick access)

### Layout System (`etm-layout/`)
- Saves window configurations to elisp files
- Handles remote paths and SSH connections
- Preview system with ASCII diagrams

### Remote Support (`etm-remote/`)
- Multi-method TRAMP connections
- Visual connection indicators
- SSH connection pooling via ControlMaster

## Data Flow

1. **Tab Creation**: `etm-new` → creates tab → initializes buffer registry
2. **Buffer Registration**: `etm-buffer-set` → stores in per-tab hash
3. **Layout Save**: captures window tree → generates elisp → saves to file
4. **Layout Load**: reads elisp → creates windows → establishes connections

## Extension Points

- `etm-custom-buffer-types`: Add custom buffer types
- `etm-vterm-init-commands`: Configure per-vterm init commands
- `etm-protected-buffers`: Buffers to hide rather than kill
