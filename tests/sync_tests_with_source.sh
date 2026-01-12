#!/bin/bash
# -*- coding: utf-8 -*-
# Author: ywatanabe
# Timestamp: <2025-12-24>
# File: tests/sync_tests_with_source.sh

# Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)

# =============================================================================
# Test Synchronization Script for Emacs Tab Manager
# =============================================================================
#
# PURPOSE:
#   Synchronizes test file structure with source code structure, ensuring
#   every source file has a corresponding test file with embedded source
#   code for reference.
#
# BEHAVIOR:
#   1. Mirrors src/etm-*/ directory structure to tests/etm-*/
#   2. For each source file (e.g., src/etm-core/etm-core-helpers.el):
#      - Creates/updates tests/etm-core/test-etm-core-helpers.el
#      - Preserves existing test code (before source block)
#      - Updates commented source code block at file end
#   3. Identifies "stale" tests (tests without matching source files)
#   4. With -m flag: moves stale tests to .old-{timestamp}/ directories
#
# STRUCTURE OF GENERATED TEST FILES:
#   - Header with metadata
#   - User's test code (preserved across syncs)
#   - ERT runner guard
#   - Commented source code block (auto-updated)
#
# USAGE:
#   ./sync_tests_with_source.sh          # Sync and report stale/placeholder files
#   ./sync_tests_with_source.sh -m       # Also move stale files to .old/
#   ./sync_tests_with_source.sh -j 8     # Use 8 parallel jobs
#   ./sync_tests_with_source.sh -r       # Report only (no sync)
#   ./sync_tests_with_source.sh -h       # Show help
#
# =============================================================================

set -u

ORIG_DIR="$(pwd)"
THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_PATH="${THIS_DIR}/.$(basename "$0").log"
echo "" >"$LOG_PATH"

# Configuration
PROJECT_ROOT="$(cd "${THIS_DIR}/.." && pwd)"
SRC_DIR="${PROJECT_ROOT}/src"
TESTS_DIR="${PROJECT_ROOT}/tests"
FILE_EXT=".el"
TEST_PREFIX="test-"

# Colors
GRAY='\033[0;90m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m'

# Default options
DO_MOVE=false
DO_SYNC=true
DO_REPORT=true
CPU_COUNT=$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)
PARALLEL_JOBS=$((CPU_COUNT / 2 > 0 ? CPU_COUNT / 2 : 1))

# Exclusion patterns
EXCLUDE_PATTERNS=(
    "^\."
    "\.old"
    "deprecated"
    "archive"
    "backup"
    "tmp"
    "temp"
    "manual"
    "mocks"
)

# Logging functions
log_info() { echo -e "${GRAY}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[OK]${NC} $1"; }
log_warning() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }
log_header() { echo -e "${BLUE}=== $1 ===${NC}"; }

# Usage
usage() {
    cat <<EOF
Usage: $0 [options]

Synchronizes test files with source files for Emacs Tab Manager.
Preserves existing test code while updating source references.

Options:
  -m, --move         Move stale test files to .old directory
  -r, --report-only  Only report, don't sync files
  -j, --jobs N       Number of parallel jobs (default: $PARALLEL_JOBS)
  -h, --help         Display this help message

Examples:
  $0                 # Sync and report
  $0 -m              # Sync, report, and move stale files
  $0 -r              # Report only (dry run)
  $0 -j 8            # Use 8 parallel jobs
EOF
    exit 0
}

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
    -m | --move)
        DO_MOVE=true
        shift
        ;;
    -r | --report-only)
        DO_SYNC=false
        shift
        ;;
    -j | --jobs)
        PARALLEL_JOBS="$2"
        shift 2
        ;;
    -h | --help)
        usage
        ;;
    *)
        log_error "Unknown option: $1"
        usage
        ;;
    esac
done

should_exclude() {
    local path="$1"
    for pattern in "${EXCLUDE_PATTERNS[@]}"; do
        if [[ "$path" =~ $pattern ]]; then
            return 0
        fi
    done
    return 1
}

# Find all source directories (src/etm-*/)
find_source_dirs() {
    find "${SRC_DIR}" -maxdepth 1 -type d -name "etm-*" 2>/dev/null | sort
}

# Find source files in a directory
find_source_files() {
    local dir="$1"
    find "$dir" -type f -name "*${FILE_EXT}" ! -name "${TEST_PREFIX}*" 2>/dev/null |
        while read -r file; do
            if ! should_exclude "$file"; then
                echo "$file"
            fi
        done
}

# Convert source path to test path
# src/etm-core/etm-core-helpers.el -> tests/etm-core/test-etm-core-helpers.el
src_to_test_path() {
    local src_file="$1"
    local rel_path
    rel_path="${src_file#"${SRC_DIR}"/}"
    local dir_part
    dir_part=$(dirname "$rel_path")
    local base_name
    base_name=$(basename "$rel_path")
    local test_name="${TEST_PREFIX}${base_name}"
    echo "${TESTS_DIR}/${dir_part}/${test_name}"
}

# Convert test path to source path
# tests/etm-core/test-etm-core-helpers.el -> src/etm-core/etm-core-helpers.el
test_to_src_path() {
    local test_file="$1"
    local rel_path
    rel_path="${test_file#"${TESTS_DIR}"/}"
    local dir_part
    dir_part=$(dirname "$rel_path")
    local test_name
    test_name=$(basename "$rel_path")
    local src_name="${test_name#"${TEST_PREFIX}"}"
    echo "${SRC_DIR}/${dir_part}/${src_name}"
}

# Get source code block
get_source_code_block() {
    local src_file="$1"
    echo ""
    echo ";; --------------------------------------------------------------------------------"
    echo ";; Start of Source Code from: ${src_file}"
    echo ";; --------------------------------------------------------------------------------"
    sed 's/^/;; /' "$src_file"
    echo ""
    echo ";; --------------------------------------------------------------------------------"
    echo ";; End of Source Code from: ${src_file}"
    echo ";; --------------------------------------------------------------------------------"
}

# Get ERT runner guard
get_ert_guard() {
    echo ""
    echo "(when (not load-file-name)"
    echo "  (ert-run-tests-interactively t))"
}

# Extract test code (before source block)
extract_test_code() {
    local test_file="$1"
    if [[ -r "$test_file" ]]; then
        if grep -q ";; Start of Source Code from:" "$test_file"; then
            sed -n '1,/^;; Start of Source Code from:/p' "$test_file" | head -n -1 |
                sed -n '/^(when (not load-file-name)/q;p'
        else
            sed -n '/^(when (not load-file-name)/q;p' "$test_file"
        fi
    fi
}

# Check if file has real tests
has_real_tests() {
    local test_file="$1"
    local test_code
    test_code=$(extract_test_code "$test_file")
    echo "$test_code" | grep -qE '\(ert-deftest' && return 0
    return 1
}

# Generate test file content
generate_test_content() {
    local src_file="$1"
    local src_name
    src_name=$(basename "$src_file")
    local feature_name="${src_name%.el}"

    cat <<EOF
;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <$(date +%Y-%m-%d)>
;;; Test file for: ${src_name}

;;; Copyright (C) $(date +%Y) Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for ${feature_name}

;;; Code:

(require 'ert)
(require '${feature_name})

;; Add your tests here
;; (ert-deftest test-${feature_name}-example ()
;;   "Example test."
;;   (should t))
EOF
}

# Process a single source file (for parallel execution)
process_single_file() {
    local src_file="$1"
    local SRC_DIR="$2"
    local TESTS_DIR="$3"
    local TEST_PREFIX="$4"

    local src_name
    src_name=$(basename "$src_file")
    local feature_name="${src_name%.el}"
    local rel_path
    rel_path="${src_file#"${SRC_DIR}"/}"
    local dir_part
    dir_part=$(dirname "$rel_path")
    local test_name="${TEST_PREFIX}${src_name}"
    local test_file="${TESTS_DIR}/${dir_part}/${test_name}"
    local test_dir
    test_dir=$(dirname "$test_file")

    mkdir -p "$test_dir"

    if [[ ! -f "$test_file" ]]; then
        # Create new test file
        {
            cat <<EOF
;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <$(date +%Y-%m-%d)>
;;; Test file for: ${src_name}

;;; Copyright (C) $(date +%Y) Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for ${feature_name}

;;; Code:

(require 'ert)
(require '${feature_name})

;; Add your tests here
;; (ert-deftest test-${feature_name}-example ()
;;   "Example test."
;;   (should t))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: ${src_file}
;; --------------------------------------------------------------------------------
EOF
            sed 's/^/;; /' "$src_file"
            echo ""
            echo ";; --------------------------------------------------------------------------------"
            echo ";; End of Source Code from: ${src_file}"
            echo ";; --------------------------------------------------------------------------------"
            echo ""
            echo ";;; ${test_name} ends here"
        } >"$test_file"
        echo "Created: ${test_file#"${TESTS_DIR}"/}"
    else
        # Update existing file - preserve test code
        local temp_file
        temp_file=$(mktemp)
        local test_code=""

        # Extract test code
        if grep -q ";; Start of Source Code from:" "$test_file"; then
            test_code=$(sed -n '1,/^;; Start of Source Code from:/p' "$test_file" | head -n -1 |
                sed -n '/^(when (not load-file-name)/q;p')
        else
            test_code=$(sed -n '/^(when (not load-file-name)/q;p' "$test_file")
        fi

        # Write test code or placeholder
        if [[ -n "$test_code" ]]; then
            echo "$test_code" >"$temp_file"
        else
            cat >"$temp_file" <<EOF
;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <$(date +%Y-%m-%d)>
;;; Test file for: ${src_name}

;;; Copyright (C) $(date +%Y) Yusuke Watanabe (ywatanabe@scitex.ai)

;;; Commentary:
;; Tests for ${feature_name}

;;; Code:

(require 'ert)
(require '${feature_name})

;; Add your tests here
EOF
        fi

        # Add ERT guard and source block
        {
            echo ""
            echo "(when (not load-file-name)"
            echo "  (ert-run-tests-interactively t))"
            echo ""
            echo ";; --------------------------------------------------------------------------------"
            echo ";; Start of Source Code from: ${src_file}"
            echo ";; --------------------------------------------------------------------------------"
            sed 's/^/;; /' "$src_file"
            echo ""
            echo ";; --------------------------------------------------------------------------------"
            echo ";; End of Source Code from: ${src_file}"
            echo ";; --------------------------------------------------------------------------------"
            echo ""
            echo ";;; ${test_name} ends here"
        } >>"$temp_file"

        mv "$temp_file" "$test_file"
        echo "Updated: ${test_file#"${TESTS_DIR}"/}"
    fi
}
export -f process_single_file

# Find stale test files
find_stale_tests() {
    while IFS= read -r test_file; do
        if should_exclude "$test_file"; then
            continue
        fi
        local src_file
        src_file=$(test_to_src_path "$test_file")
        if [[ ! -f "$src_file" ]]; then
            echo "$test_file"
        fi
    done < <(find "${TESTS_DIR}" -type f -name "${TEST_PREFIX}*${FILE_EXT}" -not -path "*.old*" 2>/dev/null)
}

# Find placeholder tests (no real ert-deftest)
find_placeholder_tests() {
    while IFS= read -r test_file; do
        if should_exclude "$test_file"; then
            continue
        fi
        if ! has_real_tests "$test_file"; then
            echo "$test_file"
        fi
    done < <(find "${TESTS_DIR}" -type f -name "${TEST_PREFIX}*${FILE_EXT}" -not -path "*.old*" 2>/dev/null)
}

# Move stale files to .old directory
move_stale_tests() {
    local timestamp
    timestamp=$(date +%Y%m%d_%H%M%S)
    local stale_count=0
    local moved_count=0
    local stale_files=()

    while IFS= read -r stale_file; do
        if [[ -n "$stale_file" ]]; then
            stale_files+=("$stale_file")
            ((stale_count++))
        fi
    done < <(find_stale_tests)

    if [[ $stale_count -gt 0 ]]; then
        echo ""
        log_header "Stale Test Files ($stale_count found)"
        echo ""
        for stale_path in "${stale_files[@]}"; do
            local rel_path
            rel_path="${stale_path#"${TESTS_DIR}"/}"
            if [[ "$DO_MOVE" == "true" ]]; then
                local stale_dir
                stale_dir=$(dirname "$stale_path")
                local stale_name
                stale_name=$(basename "$stale_path")
                local old_dir="${stale_dir}/.old-${timestamp}"
                mkdir -p "$old_dir"
                mv "$stale_path" "${old_dir}/${stale_name}"
                log_success "  [MOVED] $rel_path"
                ((moved_count++))
            else
                log_warning "  [STALE] $rel_path"
            fi
        done
        echo ""
        if [[ "$DO_MOVE" == "false" ]]; then
            log_info "To move stale files, run: $0 -m"
        else
            log_success "Moved $moved_count stale test files"
        fi
    fi
}

# Report placeholder tests
report_placeholder_tests() {
    local placeholder_count=0
    local placeholder_files=()

    while IFS= read -r placeholder_file; do
        if [[ -n "$placeholder_file" ]]; then
            placeholder_files+=("$placeholder_file")
            ((placeholder_count++))
        fi
    done < <(find_placeholder_tests)

    if [[ $placeholder_count -gt 0 ]]; then
        echo ""
        log_header "Placeholder Test Files ($placeholder_count found)"
        echo ""
        for placeholder_path in "${placeholder_files[@]}"; do
            local rel_path
            rel_path="${placeholder_path#"${TESTS_DIR}"/}"
            log_warning "  [PLACEHOLDER] $rel_path"
        done
        echo ""
        log_info "These test files have no actual test functions (no ert-deftest)."
    else
        echo ""
        log_success "No placeholder-only test files found"
    fi
}

# Sync all source files
sync_files() {
    log_info "Syncing test files (parallel, jobs=$PARALLEL_JOBS)..."

    local all_files=()
    while IFS= read -r src_dir; do
        while IFS= read -r src_file; do
            all_files+=("$src_file")
        done < <(find_source_files "$src_dir")
    done < <(find_source_dirs)

    local file_count=${#all_files[@]}

    if [[ $file_count -eq 0 ]]; then
        log_warning "No source files found in ${SRC_DIR}"
        log_info "Make sure source directories exist: src/etm-*/"
        return 1
    fi

    printf '%s\n' "${all_files[@]}" |
        xargs -P "$PARALLEL_JOBS" -I {} bash -c 'process_single_file "$@"' _ {} "$SRC_DIR" "$TESTS_DIR" "$TEST_PREFIX"

    echo ""
    log_success "Processed $file_count source files"
}

# Main
main() {
    local start_time
    start_time=$(date +%s)

    echo ""
    log_header "Emacs Tab Manager - Test Synchronization"
    echo ""
    log_info "Project:   $PROJECT_ROOT"
    log_info "Source:    $SRC_DIR"
    log_info "Tests:     $TESTS_DIR"
    log_info "Jobs:      $PARALLEL_JOBS"
    log_info "Move:      $DO_MOVE"
    log_info "Sync:      $DO_SYNC"
    echo ""

    # Check if src directory exists
    if [[ ! -d "$SRC_DIR" ]]; then
        log_warning "Source directory not found: $SRC_DIR"
        log_info "Expected structure: src/etm-*/*.el"
        log_info "Skipping sync..."
        DO_SYNC=false
    fi

    if [[ "$DO_SYNC" == "true" ]]; then
        sync_files
    fi

    if [[ "$DO_REPORT" == "true" ]]; then
        move_stale_tests
        report_placeholder_tests
    fi

    local end_time
    end_time=$(date +%s)
    local elapsed=$((end_time - start_time))

    echo ""
    log_header "Summary"
    log_success "Completed in ${elapsed}s"
    echo ""

    # Log tree for reference
    { tree "$TESTS_DIR" 2>&1 || find "$TESTS_DIR" -type f -name "*.el" 2>/dev/null; } >>"$LOG_PATH"
}

main "$@"
cd "$ORIG_DIR" || exit

# EOF
