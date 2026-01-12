#!/bin/bash
# -*- coding: utf-8 -*-
# Author: ywatanabe
# Timestamp: <2025-12-24>
# File: tests/run_tests.sh

# Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

# =============================================================================
# Test Runner for Emacs Tab Manager
# =============================================================================
#
# USAGE:
#   ./tests/run_tests.sh                    # Run all tests
#   ./tests/run_tests.sh -t etm-buffer      # Run tests in specific directory
#   ./tests/run_tests.sh -s test-file.el    # Run single test file
#   ./tests/run_tests.sh -d                 # Run with debug output
#
# =============================================================================

set -u

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${THIS_DIR}/.." && pwd)"
SRC_DIR="${PROJECT_ROOT}/src"
TESTS_DIR="${THIS_DIR}"
LOG_PATH="${THIS_DIR}/.run_tests.sh.log"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Options
TEST_DIR=""
SINGLE_FILE=""
DEBUG=""
VERBOSE=""

usage() {
    cat <<EOF
Usage: $0 [options]

Test runner for Emacs Tab Manager (ETM).

Options:
  -t, --test-dir DIR   Run tests in specific directory (e.g., etm-buffer)
  -s, --single FILE    Run single test file
  -d, --debug          Enable debug-on-error
  -v, --verbose        Show command being executed
  -h, --help           Display this help message

Examples:
  $0                           # Run all tests
  $0 -t etm-buffer             # Run tests in tests/etm-buffer/
  $0 -s tests/etm-core/test-etm-core-helpers.el
  $0 -d                        # Run with debug mode
EOF
    exit 0
}

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
    -t | --test-dir)
        TEST_DIR="$2"
        shift 2
        ;;
    -s | --single)
        SINGLE_FILE="$2"
        shift 2
        ;;
    -d | --debug)
        DEBUG="--eval (setq debug-on-error t)"
        shift
        ;;
    -v | --verbose)
        VERBOSE="1"
        shift
        ;;
    -h | --help)
        usage
        ;;
    *)
        shift
        ;;
    esac
done

# Build Emacs command
CMD="emacs -Q --batch"

# Add project root (for etm.el) and src subdirectories to load path
CMD="$CMD --eval \"(add-to-list 'load-path \\\"$PROJECT_ROOT\\\")\""
CMD="$CMD --eval \"(add-to-list 'load-path \\\"$SRC_DIR\\\")\""
for dir in etm-core etm-buffer etm-layout etm-tabs etm-close etm-keys etm-smart etm-remote etm-groups; do
    if [ -d "$SRC_DIR/$dir" ]; then
        CMD="$CMD --eval \"(add-to-list 'load-path \\\"$SRC_DIR/$dir\\\")\""
    fi
done

# Determine test files to run
if [ -n "$SINGLE_FILE" ]; then
    # Single file mode
    if [[ "$SINGLE_FILE" = /* ]]; then
        TEST_FILES="$SINGLE_FILE"
    else
        TEST_FILES="$PROJECT_ROOT/$SINGLE_FILE"
    fi
elif [ -n "$TEST_DIR" ]; then
    # Specific directory mode
    if [ -d "$TESTS_DIR/$TEST_DIR" ]; then
        TEST_FILES=$(find "$TESTS_DIR/$TEST_DIR" -name "test-*.el" -not -path "*.old*" | sort)
    else
        echo -e "${RED}Error: Test directory not found: $TESTS_DIR/$TEST_DIR${NC}"
        exit 1
    fi
else
    # All tests mode - exclude saved-layouts and .old directories
    TEST_FILES=$(find "$TESTS_DIR" -name "test-*.el" \
        -not -path "*/saved-layouts/*" \
        -not -path "*.old*" \
        -not -path "*/manual/*" \
        -not -path "*/mocks/*" | sort)
fi

# Add test directories to load path
while IFS= read -r test_dir; do
    CMD="$CMD --eval \"(add-to-list 'load-path \\\"$test_dir\\\")\""
done < <(find "$TESTS_DIR" -type d -not -path "*.old*")

# Add debug flag if requested
if [ -n "$DEBUG" ]; then
    CMD="$CMD $DEBUG"
fi

# Count test files
FILE_COUNT=$(echo "$TEST_FILES" | grep -c "\.el$" || echo 0)

# Load test files
for file in $TEST_FILES; do
    if [ -f "$file" ]; then
        CMD="$CMD --load \"$file\""
    fi
done

# Run tests
CMD="$CMD --eval \"(ert-run-tests-batch-and-exit)\""

echo -e "${BLUE}=== ETM Test Runner ===${NC}"
echo -e "Source:  $SRC_DIR"
echo -e "Tests:   $TESTS_DIR"
echo -e "Files:   $FILE_COUNT"
echo ""

if [ -n "$VERBOSE" ]; then
    echo -e "${YELLOW}Command:${NC}"
    echo "$CMD"
    echo ""
fi

echo -e "${BLUE}Running tests...${NC}"
echo ""

# Execute and capture output
eval "$CMD" 2>&1 | tee "$LOG_PATH"
EXIT_CODE=${PIPESTATUS[0]}

echo ""
if [ "$EXIT_CODE" -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
else
    echo -e "${RED}Some tests failed. See log: $LOG_PATH${NC}"
fi

exit "$EXIT_CODE"
