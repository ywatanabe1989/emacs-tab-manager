#!/bin/bash
# -*- coding: utf-8 -*-
# Simple test runner for ETM
# Run tests: ./run_tests.sh
# Run specific directory: ./run_tests.sh -t tests/etm-buffer

THIS_DIR="$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)"
TEST_DIR=""
SINGLE_FILE=""

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    -t|--test-dir)
      TEST_DIR="$2"
      shift 2
      ;;
    -s|--single)
      SINGLE_FILE="$2"
      shift 2
      ;;
    -d|--debug)
      DEBUG="--eval (setq debug-on-error t)"
      shift
      ;;
    *)
      shift
      ;;
  esac
done

# Build Emacs command
CMD="emacs -Q --batch"

# Add load paths
CMD="$CMD --eval \"(add-to-list 'load-path \\\"$THIS_DIR\\\")\""

# Add all subdirectories to load path
for dir in etm-core etm-buffer etm-layout etm-tabs etm-close etm-keys etm-smart etm-remote etm-groups; do
  if [ -d "$THIS_DIR/$dir" ]; then
    CMD="$CMD --eval \"(add-to-list 'load-path \\\"$THIS_DIR/$dir\\\")\""
  fi
done

# Add test directories to load path
if [ -n "$SINGLE_FILE" ]; then
  # Single file mode
  TEST_FILES="$SINGLE_FILE"
elif [ -n "$TEST_DIR" ]; then
  # Specific directory mode
  TEST_FILES=$(find "$THIS_DIR/$TEST_DIR" -name "test-*.el" | sort)
else
  # All tests mode
  TEST_FILES=$(find "$THIS_DIR/tests" -name "test-*.el" | sort)
fi

# Add test directories to load path
for test_dir in $(find "$THIS_DIR/tests" -type d); do
  CMD="$CMD --eval \"(add-to-list 'load-path \\\"$test_dir\\\")\""
done

# Add debug flag if requested
if [ -n "$DEBUG" ]; then
  CMD="$CMD $DEBUG"
fi

# Load test files
for file in $TEST_FILES; do
  CMD="$CMD --load \"$file\""
done

# Run tests
CMD="$CMD --eval \"(ert-run-tests-batch-and-exit)\""

echo "Running ETM tests..."
if [ -n "$DEBUG" ]; then
  echo "Command: $CMD"
fi

# Execute
eval $CMD