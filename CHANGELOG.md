# Changelog

All notable changes to Emacs Tab Manager (ETM) will be documented in this file.

## [2.5.0] - 2026-01-30

### Added
- **Dynamic Project Commands**: Automatically generate `M-x lisp-xxx` and `M-x proj-xxx` commands based on directory structure
  - Configurable via `etm-dynamic-project-dirs` alist
  - Each command creates a 3-column layout (dired | vterm | vterm)
  - Commands: `etm-dynamic-generate-commands`, `etm-dynamic-list-commands`, `etm-dynamic-regenerate-commands`

### Changed
- Improved documentation organization

## [2.4.0] - 2026-01-24

### Added
- **Automatic Numeric Buffer Registration**: When opening layouts with `etm-open-*` functions, file buffers are now automatically registered with numeric IDs (1-9)
  - Configurable via `etm-layout-auto-register-numeric` (enable/disable)
  - Configurable via `etm-layout-auto-register-max` (number of buffers to register, default: 9)
  - Solves the "No buffer registered with ID" error for new users
- **Enhanced Error Messages**: More helpful guidance when trying to jump to unregistered numeric buffers
- **Quick Start Function**: Added `etm-numeric-quick-start` for detailed help on numeric buffer system
- **Documentation**: Added comprehensive NUMERIC-BUFFERS.md guide
- **Documentation**: Added QUICK-START.md, SMART-SUGGESTIONS.md, ARCHITECTURE.md, CONTRIBUTING.md
- **Configurable Vterm Init Commands**: Per-vterm position command configuration via `etm-vterm-init-commands`
- **List Registered Buffers**: New commands to view all registered buffers
  - `etm-list-registered-buffers` (M-t L) - Shows both type-based and numeric buffers for current tab
  - `etm-list-all-tabs-buffers` (M-t A) - Shows registered buffers across all tabs

### Changed
- `etm-numeric-jump-to-buffer` now suggests using `M-t b r` when no buffer is registered
- `etm-numeric-list-buffers` provides helpful message when no buffers are registered
- Numeric buffer help messages emphasize registration requirement

### Fixed
- Improved user experience for numeric buffer system by eliminating the need for manual registration in common use cases

## [2.3.0] - Buffer Groups

- **Buffer Groups**: Organize related buffers into named groups
- Tab-specific groups with multi-group support
- Quick navigation within and between groups

## [2.2.0] - Numeric Buffer System

- **Numeric Buffer System**: Register buffers with numeric keys (0-9) for quick access
- Per-tab numeric buffer assignments
- Jump to numeric buffers with C-0 through C-9

## [2.1.0] - Smart Suggestions

- **Smart Suggestions**: Machine learning-inspired buffer recommendations
- Context-aware suggestions based on project, mode, time, and remote host
- Privacy-focused local-only pattern tracking

## [2.0.0] - Initial Public Release

- Core ETM functionality
- Buffer type system (home, semi-home, results)
- Layout management with save/load capabilities
- Remote host support via SSH