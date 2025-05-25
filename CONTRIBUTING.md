# Contributing to Emacs Tab Manager (ETM)

Thank you for your interest in contributing to ETM! This guide will help you understand the project structure, development practices, and how to contribute effectively.

## Table of Contents

1. [Project Architecture](#project-architecture)
2. [Development Setup](#development-setup)
3. [Development Workflow](#development-workflow)
4. [Testing Guidelines](#testing-guidelines)
5. [Code Style](#code-style)
6. [Submitting Changes](#submitting-changes)
7. [Release Process](#release-process)

## Project Architecture

ETM is organized into modular components for maintainability and extensibility:

```
etm/
├── etm-core/           # Core functionality and variables
├── etm-buffer/         # Buffer management and registration
├── etm-layout/         # Layout management and preview
├── etm-tabs/           # Tab creation and manipulation
├── etm-close/          # Tab closing operations
├── etm-keys/           # Keybindings and commands
└── tests/              # Test suite (mirrors main structure)
```

### Core Modules

#### etm-core
- **etm-core-variables.el**: Global variables and customization options
- **etm-core-helpers.el**: Utility functions used across modules
- **etm-core-init.el**: Initialization and startup procedures
- **etm-core-ssh-connection.el**: SSH connection management

#### etm-buffer
- **etm-buffer-setters.el**: Register buffers with types
- **etm-buffer-getters.el**: Retrieve registered buffers
- **etm-buffer-navigation.el**: Navigate between typed buffers
- **etm-buffer-numeric.el**: Numeric buffer system (0-9 slots)
- **etm-buffer-numeric-indicators.el**: Visual indicators for numeric buffers

#### etm-layout
- **etm-layout-create.el**: Create layouts from specifications
- **etm-layout-save.el**: Save current window configuration
- **etm-layout-preview.el**: Preview layouts without loading
- **etm-layout-commands.el**: Interactive layout commands

### Entry Point
- **etm.el**: Main entry point that loads all required modules

## Development Setup

1. **Clone the repository**:
   ```bash
   git clone https://github.com/ywatanabe1989/emacs-tab-manager.git
   cd emacs-tab-manager
   ```

2. **Load ETM for development**:
   ```elisp
   (add-to-list 'load-path "/path/to/emacs-tab-manager")
   (require 'etm)
   (etm-init)
   ```

3. **Run tests**:
   ```bash
   ./run_tests.sh                    # Run all tests
   ./run_tests.sh -s tests/etm-buffer/test-etm-buffer-numeric.el  # Run specific test
   ./run_tests.sh -d                 # Run with debug output
   ```

## Development Workflow

### 1. Test-Driven Development (TDD)

ETM follows strict TDD practices:

1. **Write tests first**: Create comprehensive tests before implementation
2. **Verify test failures**: Ensure tests fail initially
3. **Implement functionality**: Write code to make tests pass
4. **Refactor**: Improve code while keeping tests green

Example workflow:
```bash
# 1. Create test file
tests/etm-feature/test-etm-feature-new.el

# 2. Write failing tests
# 3. Run tests to confirm failure
./run_tests.sh -s tests/etm-feature/test-etm-feature-new.el

# 4. Implement feature
etm-feature/etm-feature-new.el

# 5. Run tests until they pass
./run_tests.sh
```

### 2. Git Workflow

We use a feature branch workflow:

1. **Start from develop**: `git checkout develop`
2. **Create feature branch**: `git checkout -b feature/descriptive-name`
3. **Make changes with meaningful commits**
4. **Push to origin**: `git push origin feature/descriptive-name`
5. **Create PR to develop**
6. **After review, merge to develop**
7. **Periodically merge develop to main for releases**

### 3. Commit Message Format

```
Short summary (50 chars or less)

- Detailed bullet point explanations
- What changed and why
- Any breaking changes noted

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>
```

## Testing Guidelines

### Test Structure

Tests mirror the main code structure:
```
tests/
├── etm-core/
│   ├── test-etm-core-variables.el
│   └── test-etm-core-helpers.el
├── etm-buffer/
│   ├── test-etm-buffer-setters.el
│   └── test-etm-buffer-numeric.el
└── ...
```

### Writing Tests

```elisp
;;; test-etm-feature-example.el

(require 'ert)
(require 'etm-feature)

(ert-deftest test-etm-feature-basic ()
  "Test basic feature functionality."
  (let ((test-value (etm-feature-function "input")))
    (should (equal test-value "expected-output"))))

(provide 'test-etm-feature-example)
```

### Test Best Practices

1. **Isolated tests**: Each test should be independent
2. **Clear names**: Test names should describe what they test
3. **Edge cases**: Test boundary conditions and error cases
4. **Mocking**: Use `cl-letf` for mocking when needed
5. **Setup/teardown**: Use test fixtures for complex scenarios

## Code Style

### Elisp Conventions

1. **File headers**:
   ```elisp
   ;;; -*- coding: utf-8; lexical-binding: t -*-
   ;;; Author: yourname
   ;;; Timestamp: <2025-05-25 12:00:00>
   ;;; File: /path/to/file.el
   ```

2. **Naming conventions**:
   - Public functions: `etm-module-function-name`
   - Private functions: `--etm-module-internal-function`
   - Variables: `etm-module-variable-name`

3. **Documentation**:
   - All public functions must have docstrings
   - Use proper formatting for arguments in docstrings
   - Include usage examples for complex functions

4. **Code organization**:
   ```elisp
   ;;; Commentary:
   ;; Module description

   ;; Dependencies
   (require 'etm-core)

   ;; Variables
   ;; ----------------------------------------
   
   ;; Functions
   ;; ----------------------------------------
   
   ;; Interactive Commands
   ;; ----------------------------------------
   
   (provide 'etm-module)
   ```

### Quality Standards

1. **No comments in code** (self-documenting code)
2. **Consistent indentation** (use Emacs defaults)
3. **Proper error handling** with meaningful messages
4. **Avoid global state** where possible
5. **Prefer pure functions** for testability

## Submitting Changes

### Pull Request Process

1. **Update tests**: Ensure all tests pass
2. **Update documentation**: Include README changes if needed
3. **Clean commits**: Squash or organize commits logically
4. **PR description**: Clearly describe changes and motivation
5. **Link issues**: Reference any related issues

### PR Template

```markdown
## Summary
Brief description of changes

## Changes
- Bullet points of specific changes
- Breaking changes noted with ⚠️

## Testing
- How to test the changes
- Test coverage percentage

## Screenshots (if UI changes)
Before/after comparisons

## Checklist
- [ ] Tests pass (./run_tests.sh)
- [ ] Documentation updated
- [ ] No breaking changes (or documented)
- [ ] Follows code style guidelines
```

## Release Process

1. **Version numbering**: Semantic versioning (MAJOR.MINOR.PATCH)
2. **Release branch**: Create from develop when ready
3. **Testing**: Full test suite must pass
4. **Documentation**: Update README with new features
5. **Tag release**: Create annotated tag
6. **GitHub release**: Create with comprehensive notes

### Version Guidelines

- **Patch (x.x.1)**: Bug fixes, minor improvements
- **Minor (x.1.x)**: New features, backward compatible
- **Major (1.x.x)**: Breaking changes

## Getting Help

- **Issues**: Use GitHub issues for bugs and features
- **Discussions**: GitHub discussions for questions
- **Documentation**: Check `/docs` directory
- **Examples**: See `/docs/examples` for usage patterns

## Code of Conduct

- Be respectful and inclusive
- Focus on constructive feedback
- Help others learn and grow
- Maintain high quality standards

Thank you for contributing to ETM! Your efforts help make tab and buffer management in Emacs better for everyone.