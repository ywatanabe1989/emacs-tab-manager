#!/bin/bash
# Run each test file individually to avoid load-path issues

THIS_DIR="$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)"
cd "$THIS_DIR"

# Find all test files
test_files=$(find ./tests -name "test-*.el" | sort)

# Run each test file individually
success=0
failed=0

for test_file in $test_files; do
  echo "Running test file: $test_file"
  ./run_tests.sh "$test_file"
  
  if [ $? -eq 0 ]; then
    ((success++))
  else
    ((failed++))
    echo "Test file failed: $test_file"
  fi
  echo "-----------------------------------------"
done

echo "Test run complete:"
echo "Success: $success"
echo "Failed: $failed"
echo "Total: $((success + failed))"

if [ $failed -eq 0 ]; then
  echo "All tests passed!"
  exit 0
else
  echo "Some tests failed."
  exit 1
fi