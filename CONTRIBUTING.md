# Contributing

## Development Setup

1. Clone the repository
2. Add to load-path: `(add-to-list 'load-path "/path/to/emacs-tab-manager")`
3. Load with `(require 'etm)`

## Running Tests

```bash
./run_tests.sh
```

Tests use ERT (Emacs Regression Testing) framework.

## Code Style

- Use `lexical-binding: t` in all files
- Prefix public functions with `etm-`
- Prefix private functions with `--etm-`
- Add docstrings to all public functions

## Module Guidelines

Each module in `src/` should:
- Have a main file (e.g., `etm-buffer.el`) that requires submodules
- Provide a feature matching its filename
- Include a `provide` statement

## Pull Requests

1. Create a feature branch from `develop`
2. Write/update tests for changes
3. Ensure all tests pass
4. Update CHANGELOG.md
5. Submit PR to `develop` branch

## Reporting Issues

Please include:
- Emacs version (`M-x emacs-version`)
- ETM version
- Steps to reproduce
- Expected vs actual behavior
