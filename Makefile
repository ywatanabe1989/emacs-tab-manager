# -*- coding: utf-8 -*-
# Author: ywatanabe
# Timestamp: <2025-12-24>
# File: Makefile

# Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

# =============================================================================
# Emacs Tab Manager (ETM) - Makefile
# =============================================================================

.PHONY: all test test-quick test-dir sync sync-move lint clean help install-hooks uninstall-hooks

# Directories
SRC_DIR := src
TESTS_DIR := tests

# Default target - show help
all: help

# =============================================================================
# Testing
# =============================================================================

## Run all tests
test:
	@./$(TESTS_DIR)/run_tests.sh

## Run tests with debug output
test-debug:
	@./$(TESTS_DIR)/run_tests.sh -d

## Run tests with verbose output
test-verbose:
	@./$(TESTS_DIR)/run_tests.sh -v

## Run tests in a specific directory (e.g., make test-dir DIR=etm-buffer)
test-dir:
	@./$(TESTS_DIR)/run_tests.sh -t $(DIR)

## Run a single test file (e.g., make test-file FILE=tests/etm-core/test-etm-core-helpers.el)
test-file:
	@./$(TESTS_DIR)/run_tests.sh -s $(FILE)

# =============================================================================
# Test Synchronization
# =============================================================================

## Sync test files with source (creates/updates test file placeholders)
sync:
	@./$(TESTS_DIR)/sync_tests_with_source.sh

## Sync and move stale test files to .old/
sync-move:
	@./$(TESTS_DIR)/sync_tests_with_source.sh -m

## Report only (dry run)
sync-report:
	@./$(TESTS_DIR)/sync_tests_with_source.sh -r

# =============================================================================
# Linting
# =============================================================================

## Lint elisp files (requires emacs)
lint:
	@echo "Linting elisp files..."
	@find $(SRC_DIR) -name "*.el" -exec emacs -Q --batch \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile {} \; 2>&1 | grep -E "(Error|Warning)" || echo "No lint errors"

## Byte-compile all source files
compile:
	@echo "Byte-compiling elisp files..."
	@emacs -Q --batch \
		--eval "(add-to-list 'load-path \".\")" \
		--eval "(add-to-list 'load-path \"$(SRC_DIR)\")" \
		--eval "(dolist (dir '(\"etm-core\" \"etm-buffer\" \"etm-layout\" \"etm-tabs\" \"etm-close\" \"etm-keys\" \"etm-smart\" \"etm-remote\" \"etm-groups\")) (add-to-list 'load-path (concat \"$(SRC_DIR)/\" dir)))" \
		--eval "(batch-byte-compile)" \
		etm.el $(shell find $(SRC_DIR) -name "*.el" -not -name ".*")

# =============================================================================
# Cleanup
# =============================================================================

## Clean compiled files
clean:
	@echo "Cleaning compiled files..."
	@find $(SRC_DIR) -name "*.elc" -delete
	@find $(TESTS_DIR) -name "*.elc" -delete
	@echo "Done"

## Clean old test files
clean-old:
	@echo "Cleaning .old directories..."
	@find $(TESTS_DIR) -type d -name ".old-*" -exec rm -rf {} + 2>/dev/null || true
	@echo "Done"

# =============================================================================
# Git Hooks
# =============================================================================

## Install pre-commit hook (runs tests before commit)
install-hooks:
	@echo "Installing pre-commit hook..."
	@mkdir -p .git/hooks
	@echo '#!/bin/bash' > .git/hooks/pre-commit
	@echo '# ETM pre-commit hook - runs tests before commit' >> .git/hooks/pre-commit
	@echo 'echo "Running ETM tests..."' >> .git/hooks/pre-commit
	@echo 'make test' >> .git/hooks/pre-commit
	@echo 'if [ $$? -ne 0 ]; then' >> .git/hooks/pre-commit
	@echo '    echo "Tests failed. Commit aborted."' >> .git/hooks/pre-commit
	@echo '    exit 1' >> .git/hooks/pre-commit
	@echo 'fi' >> .git/hooks/pre-commit
	@chmod +x .git/hooks/pre-commit
	@echo "Pre-commit hook installed."

## Uninstall pre-commit hook
uninstall-hooks:
	@echo "Removing pre-commit hook..."
	@rm -f .git/hooks/pre-commit
	@echo "Pre-commit hook removed."

# =============================================================================
# Project Info
# =============================================================================

## Show project structure
tree:
	@tree -L 2 -I '__pycache__|*.elc|.git|.old*'

## Count lines of code
loc:
	@echo "Source files:"
	@find $(SRC_DIR) -name "*.el" | xargs wc -l | tail -1
	@echo "Test files:"
	@find $(TESTS_DIR) -name "*.el" -not -path "*/saved-layouts/*" | xargs wc -l | tail -1

# =============================================================================
# Help
# =============================================================================

## Show this help message
help:
	@echo "Emacs Tab Manager (ETM) - Available targets:"
	@echo ""
	@echo "Testing:"
	@echo "  make test          - Run all tests"
	@echo "  make test-debug    - Run tests with debug output"
	@echo "  make test-verbose  - Run tests with verbose output"
	@echo "  make test-dir DIR=etm-buffer  - Run tests in specific directory"
	@echo "  make test-file FILE=path/to/test.el  - Run single test file"
	@echo ""
	@echo "Synchronization:"
	@echo "  make sync          - Sync test files with source"
	@echo "  make sync-move     - Sync and move stale files to .old/"
	@echo "  make sync-report   - Report only (dry run)"
	@echo ""
	@echo "Linting & Compilation:"
	@echo "  make lint          - Lint elisp files"
	@echo "  make compile       - Byte-compile source files"
	@echo ""
	@echo "Cleanup:"
	@echo "  make clean         - Remove .elc files"
	@echo "  make clean-old     - Remove .old directories"
	@echo ""
	@echo "Git Hooks:"
	@echo "  make install-hooks - Install pre-commit hook (runs tests)"
	@echo "  make uninstall-hooks - Remove pre-commit hook"
	@echo ""
	@echo "Project Info:"
	@echo "  make tree          - Show project structure"
	@echo "  make loc           - Count lines of code"
