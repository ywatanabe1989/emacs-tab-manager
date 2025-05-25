#!/bin/bash
# Simple ERT test runner

# Initialize counters
total=0
passed=0
failed=0

# Find all test files
test_files=$(find tests -name "test-*.el" | sort)

# Run each test file
for test_file in $test_files; do
    echo "Running $test_file..."
    
    # Run the test and capture output
    output=$(emacs -Q --batch \
        -L . -L etm-core -L etm-buffer -L etm-close -L etm-keys -L etm-layout -L etm-tabs -L etm-groups -L etm-remote \
        -L tests/mocks \
        -l "$test_file" \
        -f ert-run-tests-batch-and-exit 2>&1)
    
    exit_code=$?
    
    # Parse results
    if [ $exit_code -eq 0 ]; then
        # Extract test counts from output
        test_count=$(echo "$output" | grep -oP 'Running \K[0-9]+(?= tests)' | head -1)
        if [ -n "$test_count" ]; then
            total=$((total + test_count))
            passed=$((passed + test_count))
        fi
        echo "  ✓ All tests passed"
    else
        # Extract failure count
        test_count=$(echo "$output" | grep -oP 'Running \K[0-9]+(?= tests)' | head -1)
        failed_count=$(echo "$output" | grep -oP '[0-9]+(?= unexpected)' | head -1)
        
        if [ -n "$test_count" ] && [ -n "$failed_count" ]; then
            total=$((total + test_count))
            passed=$((passed + test_count - failed_count))
            failed=$((failed + failed_count))
        fi
        echo "  ✗ Some tests failed"
        echo "$output" | grep "FAILED" | head -5
    fi
done

# Calculate percentage
if [ $total -gt 0 ]; then
    percent=$((passed * 100 / total))
else
    percent=0
fi

# Summary
echo
echo "Test Summary:"
echo "============="
echo "Total tests: $total"
echo "Passed: $passed"
echo "Failed: $failed"
echo "Success rate: $percent%"

# Exit with appropriate code
if [ $failed -eq 0 ]; then
    exit 0
else
    exit 1
fi