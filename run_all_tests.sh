#!/bin/bash

# Run all tests for tdump utility

echo "Running all tdump tests..."
echo

# Compile and run unit tests
echo "=== Compiling and running unit tests ==="
if fpc test_tdump.pas > /dev/null 2>&1; then
    echo "Unit tests compiled successfully"
    ./test_tdump
    UNIT_EXIT=$?
else
    echo "Failed to compile unit tests"
    exit 1
fi

echo
echo "=== Running integration tests ==="
./run_integration_tests.sh
INTEGRATION_EXIT=$?

echo
echo "=== Overall Test Results ==="
if [ $UNIT_EXIT -eq 0 ] && [ $INTEGRATION_EXIT -eq 0 ]; then
    echo "All tests passed successfully!"
    exit 0
else
    echo "Some tests failed!"
    exit 1
fi