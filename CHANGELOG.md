# Changelog

All notable changes to Emacs Tab Manager (ETM) will be documented in this file.

## [0.2.0] - 2026-01-24

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

## [0.1.0] - Previous Release

- Initial release with core ETM functionality
- Buffer type system (home, semi-home, results)
- Layout management with save/load capabilities
- Smart suggestions based on usage patterns
- Remote host support via SSH