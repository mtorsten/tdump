#!/bin/bash

# Integration tests for tdump utility
# This script tests end-to-end functionality

TESTS_PASSED=0
TESTS_FAILED=0
TEMP_FILES=()

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Helper functions
pass_test() {
    echo -e "${GREEN}PASS:${NC} $1"
    ((TESTS_PASSED++))
}

fail_test() {
    echo -e "${RED}FAIL:${NC} $1"
    if [ $# -gt 1 ]; then
        echo "  Expected: $2"
        echo "  Actual: $3"
    fi
    ((TESTS_FAILED++))
}

cleanup() {
    for file in "${TEMP_FILES[@]}"; do
        [ -f "$file" ] && rm -f "$file"
    done
}

# Trap to cleanup on exit
trap cleanup EXIT

# Check if tdump exists
if [ ! -f "./tdump" ]; then
    echo "Error: tdump executable not found. Please compile tdump first."
    exit 1
fi

echo "Running tdump integration tests..."
echo

# Test 1: Basic file output
echo "=== Testing Basic File Output ==="
TEST_FILE="test_basic.bin"
printf '\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0A\x0B\x0C\x0D\x0E\x0F\x10' > "$TEST_FILE"
TEMP_FILES+=("$TEST_FILE")

OUTPUT=$(./tdump "$TEST_FILE" 2>&1)
EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
    pass_test "Basic file dump should exit successfully"
else
    fail_test "Basic file dump should exit successfully" "0" "$EXIT_CODE"
fi

if echo "$OUTPUT" | grep -q "00000000  01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F 10"; then
    pass_test "Should contain correct hex output"
else
    fail_test "Should contain correct hex output"
    echo "  Actual output: $OUTPUT"
fi

if echo "$OUTPUT" | grep -q "................"; then
    pass_test "Should contain ASCII representation"
else
    fail_test "Should contain ASCII representation"
fi

echo

# Test 2: Different output modes
echo "=== Testing Different Output Modes ==="
TEST_FILE="test_modes.bin"
printf '\x01\x02\x03\x04\x05\x06\x07\x08' > "$TEST_FILE"
TEMP_FILES+=("$TEST_FILE")

# Test word mode
OUTPUT=$(./tdump -w "$TEST_FILE" 2>&1)
EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
    pass_test "Word mode should exit successfully"
else
    fail_test "Word mode should exit successfully" "0" "$EXIT_CODE"
fi

if echo "$OUTPUT" | grep -q "0102 0304 0506 0708"; then
    pass_test "Word mode should group bytes correctly"
else
    fail_test "Word mode should group bytes correctly"
    echo "  Actual output: $OUTPUT"
fi

# Test dword mode
OUTPUT=$(./tdump -d "$TEST_FILE" 2>&1)
EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
    pass_test "DWord mode should exit successfully"
else
    fail_test "DWord mode should exit successfully" "0" "$EXIT_CODE"
fi

if echo "$OUTPUT" | grep -q "01020304 05060708"; then
    pass_test "DWord mode should group bytes correctly"
else
    fail_test "DWord mode should group bytes correctly"
    echo "  Actual output: $OUTPUT"
fi

# Test custom count
OUTPUT=$(./tdump -c 4 "$TEST_FILE" 2>&1)
EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
    pass_test "Custom count should exit successfully"
else
    fail_test "Custom count should exit successfully" "0" "$EXIT_CODE"
fi

if echo "$OUTPUT" | grep -q "00000000  01 02 03 04" && echo "$OUTPUT" | grep -q "00000004  05 06 07 08"; then
    pass_test "Should show correct line breaks with custom count"
else
    fail_test "Should show correct line breaks with custom count"
    echo "  Actual output: $OUTPUT"
fi

echo

# Test 3: Verbose output
echo "=== Testing Verbose Output ==="
TEST_FILE="test_verbose.bin"
printf 'ABCD' > "$TEST_FILE"
TEMP_FILES+=("$TEST_FILE")

OUTPUT=$(./tdump -v "$TEST_FILE" 2>&1)
EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
    pass_test "Verbose mode should exit successfully"
else
    fail_test "Verbose mode should exit successfully" "0" "$EXIT_CODE"
fi

if echo "$OUTPUT" | grep -q "Begin dump of $TEST_FILE"; then
    pass_test "Should contain verbose header"
else
    fail_test "Should contain verbose header"
    echo "  Actual output: $OUTPUT"
fi

if echo "$OUTPUT" | grep -q "End dump of $TEST_FILE"; then
    pass_test "Should contain verbose footer"
else
    fail_test "Should contain verbose footer"
    echo "  Actual output: $OUTPUT"
fi

if echo "$OUTPUT" | grep -q "4 bytes"; then
    pass_test "Should show correct file size"
else
    fail_test "Should show correct file size"
    echo "  Actual output: $OUTPUT"
fi

if echo "$OUTPUT" | grep -q "00000000  41 42 43 44"; then
    pass_test "Should contain hex data"
else
    fail_test "Should contain hex data"
    echo "  Actual output: $OUTPUT"
fi

echo

# Test 4: Standard input processing
echo "=== Testing Standard Input Processing ==="

OUTPUT=$(echo -n "Hello" | ./tdump 2>&1)
EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
    pass_test "Stdin processing should exit successfully"
else
    fail_test "Stdin processing should exit successfully" "0" "$EXIT_CODE"
fi

if echo "$OUTPUT" | grep -q "00000000  48 65 6C 6C 6F"; then
    pass_test "Should contain correct hex for 'Hello'"
else
    fail_test "Should contain correct hex for 'Hello'"
    echo "  Actual output: $OUTPUT"
fi

if echo "$OUTPUT" | grep -q "Hello"; then
    pass_test "Should contain ASCII representation"
else
    fail_test "Should contain ASCII representation"
    echo "  Actual output: $OUTPUT"
fi

# Test verbose with stdin
OUTPUT=$(echo -n "Hello" | ./tdump -v 2>&1)
EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
    pass_test "Verbose stdin should exit successfully"
else
    fail_test "Verbose stdin should exit successfully" "0" "$EXIT_CODE"
fi

if echo "$OUTPUT" | grep -q "End dump of stdin"; then
    pass_test "Should contain verbose footer for stdin"
else
    fail_test "Should contain verbose footer for stdin"
    echo "  Actual output: $OUTPUT"
fi

echo

# Test 5: Error scenarios
echo "=== Testing Error Scenarios ==="

# Test non-existent file
OUTPUT=$(./tdump nonexistent.bin 2>&1)
EXIT_CODE=$?

if [ $EXIT_CODE -ne 0 ]; then
    pass_test "Non-existent file should exit with error"
else
    fail_test "Non-existent file should exit with error" "non-zero" "$EXIT_CODE"
fi

if echo "$OUTPUT" | grep -q "Error:"; then
    pass_test "Should contain error message"
else
    fail_test "Should contain error message"
    echo "  Actual output: $OUTPUT"
fi

# Test invalid option
OUTPUT=$(./tdump -x 2>&1)
EXIT_CODE=$?

if [ $EXIT_CODE -ne 0 ]; then
    pass_test "Invalid option should exit with error"
else
    fail_test "Invalid option should exit with error" "non-zero" "$EXIT_CODE"
fi

if echo "$OUTPUT" | grep -q "Error:" && echo "$OUTPUT" | grep -q "Unknown option"; then
    pass_test "Should mention unknown option"
else
    fail_test "Should mention unknown option"
    echo "  Actual output: $OUTPUT"
fi

# Test invalid count
OUTPUT=$(./tdump -c abc 2>&1)
EXIT_CODE=$?

if [ $EXIT_CODE -ne 0 ]; then
    pass_test "Invalid count should exit with error"
else
    fail_test "Invalid count should exit with error" "non-zero" "$EXIT_CODE"
fi

if echo "$OUTPUT" | grep -q "Error:" && echo "$OUTPUT" | grep -q "Invalid count"; then
    pass_test "Should mention invalid count"
else
    fail_test "Should mention invalid count"
    echo "  Actual output: $OUTPUT"
fi

echo

# Test 6: Help option
echo "=== Testing Help Option ==="

OUTPUT=$(./tdump -h 2>&1)
EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
    pass_test "Help option should exit successfully"
else
    fail_test "Help option should exit successfully" "0" "$EXIT_CODE"
fi

if echo "$OUTPUT" | grep -q "tdump - Hexadecimal dump utility"; then
    pass_test "Should contain program description"
else
    fail_test "Should contain program description"
    echo "  Actual output: $OUTPUT"
fi

if echo "$OUTPUT" | grep -q "Usage:" && echo "$OUTPUT" | grep -q "Options:" && echo "$OUTPUT" | grep -q "Examples:"; then
    pass_test "Should contain usage information"
else
    fail_test "Should contain usage information"
    echo "  Actual output: $OUTPUT"
fi

# Test long help option
OUTPUT=$(./tdump --help 2>&1)
EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
    pass_test "Long help option should exit successfully"
else
    fail_test "Long help option should exit successfully" "0" "$EXIT_CODE"
fi

echo

# Test 7: Edge cases
echo "=== Testing Edge Cases ==="

# Test empty file
TEST_FILE="test_empty.bin"
touch "$TEST_FILE"
TEMP_FILES+=("$TEST_FILE")

OUTPUT=$(./tdump "$TEST_FILE" 2>&1)
EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
    pass_test "Empty file should exit successfully"
else
    fail_test "Empty file should exit successfully" "0" "$EXIT_CODE"
fi

# Test single byte file
TEST_FILE="test_single.bin"
printf 'A' > "$TEST_FILE"
TEMP_FILES+=("$TEST_FILE")

OUTPUT=$(./tdump "$TEST_FILE" 2>&1)
EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
    pass_test "Single byte file should exit successfully"
else
    fail_test "Single byte file should exit successfully" "0" "$EXIT_CODE"
fi

if echo "$OUTPUT" | grep -q "00000000  41" && echo "$OUTPUT" | grep -q "A"; then
    pass_test "Should contain single byte hex and ASCII"
else
    fail_test "Should contain single byte hex and ASCII"
    echo "  Actual output: $OUTPUT"
fi

echo

# Test 8: Long options
echo "=== Testing Long Options ==="
TEST_FILE="test_long.bin"
printf '\x01\x02\x03\x04\x05\x06\x07\x08' > "$TEST_FILE"
TEMP_FILES+=("$TEST_FILE")

OUTPUT=$(./tdump --count 4 --word --verbose "$TEST_FILE" 2>&1)
EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
    pass_test "Long options should exit successfully"
else
    fail_test "Long options should exit successfully" "0" "$EXIT_CODE"
fi

if echo "$OUTPUT" | grep -q "Begin dump of $TEST_FILE"; then
    pass_test "Should contain verbose header with long options"
else
    fail_test "Should contain verbose header with long options"
    echo "  Actual output: $OUTPUT"
fi

if echo "$OUTPUT" | grep -q "0102 0304" && echo "$OUTPUT" | grep -q "0506 0708"; then
    pass_test "Should use word mode with long options"
else
    fail_test "Should use word mode with long options"
    echo "  Actual output: $OUTPUT"
fi

echo

# Final results
echo "=== Integration Test Results ==="
echo "Tests passed: $TESTS_PASSED"
echo "Tests failed: $TESTS_FAILED"
echo "Total tests: $((TESTS_PASSED + TESTS_FAILED))"

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "${GREEN}All integration tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some integration tests failed!${NC}"
    exit 1
fi