#!/bin/bash
# -*- coding: utf-8 -*-
# Timestamp: "2025-05-09 19:41:57 (ywatanabe)"
# File: ./run_tests_elisp.sh

THIS_DIR="$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)"
LOG_PATH="$THIS_DIR/.$(basename $0).log"
echo > "$LOG_PATH"

GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color
# ---------------------------------------

# Color definitions

# Script to run elisp tests
TEST_TIMEOUT=10
ELISP_TEST_PATH="$HOME/.emacs.d/lisp/elisp-test"
TESTS_DIR="${2:-$THIS_DIR/tests}"
SRC_DIR="$THIS_DIR"

usage() {
    echo "Usage: $0 [options]"
    echo
    echo "Options:"
    echo "  --tests-dir|-t TESTS_DIR  Directory containing elisp test files (default: $TESTS_DIR)"
    echo "  --src-dir|-s SRC_DIR      Directory containing source files (default: $SRC_DIR)"
    echo "  --elisp-test PATH         Loadpath to elisp-test.el (default: $ELISP_TEST_PATH)"
    echo "  --timeout SECONDS         Timeout for tests in seconds (default: ${TEST_TIMEOUT}s)"
    echo "  --recursive|-r            Recursively add all subdirectories to load-path"
    echo "  -h, --help                Display this help message"
    echo
    echo "Example:"
    echo "  $0"
    echo "  $0 --tests-dir ./tests --recursive"
    echo "  $0 --tests-dir /path/to/custom/tests --src-dir /path/to/source"
    echo "  $0 --timeout 30 --recursive"
}

# Parse command line arguments
RECURSIVE=false
while [[ $# -gt 0 ]]; do
    case $1 in
        --timeout)
            TEST_TIMEOUT="$2"
            shift 2
            ;;
        --elisp-test)
            ELISP_TEST_PATH="$2"
            shift 2
            ;;
        --tests-dir|-t)
            TESTS_DIR="$2"
            shift 2
            ;;
        --src-dir|-s)
            SRC_DIR="$2"
            shift 2
            ;;
        --recursive|-r)
            RECURSIVE=true
            shift
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            echo -e "${RED}Error: Unknown option $1${NC}" >&2
            usage
            exit 1
            ;;
    esac
done

# Function to generate load-path entries recursively
generate_load_paths() {
    local base_dir="$1"
    local load_path_elisp=""

    if [ ! -d "$base_dir" ]; then
        return
    fi

    # Add the base directory
    load_path_elisp+="(add-to-list 'load-path \"$base_dir\")\n"

    # If recursive mode, add all subdirectories
    if [ "$RECURSIVE" = true ]; then
        while IFS= read -r dir; do
            load_path_elisp+="(add-to-list 'load-path \"$dir\")\n"
        done < <(find "$base_dir" -type d -not -path "*/\.*" | sort)
    fi

    echo -e "$load_path_elisp"
}

# Function to run all tests in a directory
run_tests_elisp() {
    local directory="$1"

    if [ -z "$directory" ]; then
        echo -e "${RED}Error: Test directory not specified${NC}" | tee -a "$LOG_PATH"
        usage
        return 1
    fi

    if [ ! -d "$directory" ]; then
        echo -e "${RED}Error: Directory '$directory' does not exist${NC}" | tee -a "$LOG_PATH"
        return 1
    fi

    echo -e "${YELLOW}Running tests in $directory...${NC}" | tee -a "$LOG_PATH"

    # Generate load paths for source and test directories
    local src_load_paths=$(generate_load_paths "$SRC_DIR")
    local test_load_paths=$(generate_load_paths "$directory")

    # Create a temporary elisp file with the load path configuration
    local temp_elisp_file=$(mktemp)
    cat > "$temp_elisp_file" <<EOF
(add-to-list 'load-path "$(pwd)")
$src_load_paths
$test_load_paths
(add-to-list 'load-path "$ELISP_TEST_PATH")
(require 'elisp-test)
(elisp-test-run "$directory" $TEST_TIMEOUT t)
EOF

    # Run the tests using the temporary file
    timeout ${TEST_TIMEOUT}s emacs -Q --batch \
        --load "$temp_elisp_file" \
        >> "$LOG_PATH" 2>&1

    local exit_status=$?
    rm -f "$temp_elisp_file"

    if [ $exit_status -eq 124 ] || [ $exit_status -eq 137 ]; then
        echo -e "${RED}Test execution timed out after ${TEST_TIMEOUT}s${NC}" | tee -a "$LOG_PATH"
        return $exit_status
    fi

    # Find reports created in the last minute
    local report_file=$(find "$THIS_DIR" -maxdepth 1 -mmin -0.1 -name "*ELISP-TEST-REPORT*" | head -n 1)

    if [ -f "$report_file" ]; then
        echo -e "${GREEN}Report created: $report_file${NC}" | tee -a "$LOG_PATH"
        cat "$report_file" >> "$LOG_PATH"
        return 0
    else
        echo -e "${RED}No test report was generated. Check for errors.${NC}" | tee -a "$LOG_PATH"
        return 1
    fi
}

# Execute tests and log output
run_tests_elisp "$TESTS_DIR" | tee -a "$LOG_PATH"
exit_code=${PIPESTATUS[0]}

if [ $exit_code -eq 0 ]; then
    echo -e "${GREEN}Tests completed successfully with exit code: $exit_code${NC}" | tee -a "$LOG_PATH"
else
    echo -e "${RED}Tests completed with errors. Exit code: $exit_code${NC}" | tee -a "$LOG_PATH"
fi

exit $exit_code

# EOF