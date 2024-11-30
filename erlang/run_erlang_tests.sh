#!/bin/bash

# Directory setup
RESULTS_DIR="../data/results"
mkdir -p "$RESULTS_DIR"

# CSV file setup
CSV_FILE="$RESULTS_DIR/erlang_results.csv"

# Initialize CSV with headers if it doesn't exist
if [ ! -f "$CSV_FILE" ]; then
    echo "dataset,implementation,runtime,workers" > "$CSV_FILE"
fi

# Function to extract runtime from Erlang output
extract_runtime() {
    local file=$1
    # Assuming each line in the file is already CSV formatted
    cat "$file"
}

echo "Starting Erlang experiments..."

# Sequential Tests
echo "Running Sequential DS1..."
output_file="$RESULTS_DIR/erl_seq_ds1.txt"
erl -noshell -pa ebin -eval 'test_sequential:run_DS1()' -s init stop > "$output_file"
cat "$output_file"
extract_runtime "$output_file" >> "$CSV_FILE"

echo "Running Sequential DS2..."
output_file="$RESULTS_DIR/erl_seq_ds2.txt"
erl -noshell -pa ebin -eval 'test_sequential:run_DS2()' -s init stop > "$output_file"
cat "$output_file"
extract_runtime "$output_file" >> "$CSV_FILE"

# Parallel Tests
echo "Running Parallel DS1..."
output_file="$RESULTS_DIR/erl_par_ds1.txt"
erl -noshell -pa ebin -eval 'test_parallel:run_DS1()' -s init stop > "$output_file"
cat "$output_file"
extract_runtime "$output_file" >> "$CSV_FILE"

echo "Running Parallel DS2..."
output_file="$RESULTS_DIR/erl_par_ds2.txt"
erl -noshell -pa ebin -eval 'test_parallel:run_DS2()' -s init stop > "$output_file"
cat "$output_file"
extract_runtime "$output_file" >> "$CSV_FILE"

echo "Running Parallel DS3..."
output_file="$RESULTS_DIR/erl_par_ds3.txt"
erl -noshell -pa ebin -eval 'test_parallel:run_DS3()' -s init stop > "$output_file"
cat "$output_file"
extract_runtime "$output_file" >> "$CSV_FILE"

echo "All Erlang experiments complete!"
echo "Results have been saved to:"
echo "- CSV file: $CSV_FILE"
echo "- Individual run logs: $RESULTS_DIR/erl_*.txt"