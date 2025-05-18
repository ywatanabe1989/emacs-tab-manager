# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Emacs Tab Manager (ETM) is an extension for Emacs' built-in tab-bar.el that enhances tab and buffer management with a type-based organization system. It allows users to register buffers with specific types per tab, efficiently navigate between typed buffers, and manage window layouts.

## Key Features

- **Buffer Type System**: Register buffers with specific types per tab (home, semi-home, results by default)
- **Smart Buffer Management**: Different handling for buffers based on registration status
- **Layout Management**: Save and load window configurations with remote host support

## Architecture

ETM is organized into modular components:

1. **Core (etm-core/)**: Contains fundamental variables, helpers, and initialization functions
   - `etm-core-variables.el`: Basic variables and data structures
   - `etm-core-helpers.el`: Utility functions
   - `etm-core-init.el`: Initialization procedures
   - `etm-core-tab-id.el`: Tab identification management

2. **Buffer Management (etm-buffer/)**: Handles buffer registration and navigation
   - `etm-buffer-setters.el`: Functions to register buffers with types
   - `etm-buffer-getters.el`: Functions to retrieve registered buffers
   - `etm-buffer-jumpers.el`: Navigation between buffers of specific types
   - `etm-buffer-checkers.el`: Functions to check buffer registration status

3. **Layout Management (etm-layout/)**: Manages window layouts and configurations
   - `etm-layout-create.el`: Creates layouts from specifications
   - `etm-layout-save.el`: Saves current layout to elisp file
   - `etm-layout-load.el`: Loads saved layouts

4. **Tab Management (etm-tabs/)**: Handles tab creation and manipulation
   - `etm-tabs-new-and-rename.el`: Creates and renames tabs

5. **Close Operations (etm-close/)**: Handles tab closing operations
   - `etm-close-core.el`: Core closing functions
   - `etm-close-utils.el`: Utilities for closing operations

6. **Keybindings (etm-keys/)**: Defines ETM keybindings
   - `etm-keys-command-map.el`: Defines command prefix map
   - `etm-keys-navigation.el`: Keybindings for navigation

7. **Main Entry (etm.el)**: Main entry point that loads all required modules

## Development Workflow

### Running Tests

The project uses a custom elisp test framework. Tests can be run with:

```bash
# Run all tests
./run_tests.sh

# Run a single test file
./run_tests.sh -s tests/etm-core/test-etm-core-variables.el

# Run tests with debug output
./run_tests.sh -d

# Run tests in a specific directory
./run_tests.sh -t tests/etm-buffer
```

### Code Organization Conventions

1. Each module is contained in its own directory (e.g., `etm-core/`, `etm-buffer/`)
2. Each file should provide a specific feature with the same name as the file
3. Every elisp file should include:
   - Proper lexical binding header (`;;; -*- coding: utf-8; lexical-binding: t -*-`)
   - Author and timestamp information
   - Commentary section explaining purpose
   - Appropriate `provide` statement at the end

### Test Organization

Tests are organized in the `tests/` directory with a structure mirroring the main code:

- `tests/etm-core/` - Tests for core functionality
- `tests/etm-buffer/` - Tests for buffer management, etc.

Each test file should correspond to a specific implementation file being tested.

## Configuration Options

The most important customizable variables are:

- `etm-custom-buffer-types`: List of additional buffer types beyond the defaults
- `etm-protected-buffers`: List of buffer names that should be hidden rather than killed
- `etm-layout-save-dir`: Directory path for saving ETM layouts

## Initialization Process

The package is initialized with:

```elisp
(require 'etm)
(etm-init)  ;; Initialize and enable ETM
```