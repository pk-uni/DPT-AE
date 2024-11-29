#!/bin/bash

# Directory setup
RESULTS_DIR="data/results"
ERLANG_DIR="erlang"
mkdir -p "$RESULTS_DIR"

# CSV file setup
CSV_FILE="$RESULTS_DIR/erlang_results.csv"

# Initialize CSV with headers if it doesn't exist
if [ ! -f "$CSV_FILE" ]; then
    echo "dataset,implementation,runtime,workers" > "$CSV_FILE"
fi

echo "Running sequential tests..."
# Run sequential tests for DS1 and DS2
erl -noshell -pa ${ERLANG_DIR}/ebin -eval 'test_sequential:run_DS1()' -s init stop >> "$CSV_FILE"
erl -noshell -pa ${ERLANG_DIR}/ebin -eval 'test_sequential:run_DS2()' -s init stop >> "$CSV_FILE"

echo "Running parallel tests..."
# Run parallel tests for all datasets
erl -noshell -pa ${ERLANG_DIR}/ebin -eval 'test_parallel:run_DS1()' -s init stop >> "$CSV_FILE"
erl -noshell -pa ${ERLANG_DIR}/ebin -eval 'test_parallel:run_DS2()' -s init stop >> "$CSV_FILE"
erl -noshell -pa ${ERLANG_DIR}/ebin -eval 'test_parallel:run_DS3()' -s init stop >> "$CSV_FILE"

echo "All tests completed. Results written to $CSV_FILE"