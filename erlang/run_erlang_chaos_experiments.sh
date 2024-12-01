#!/bin/bash

# Directory setup
RESULTS_DIR="../data/results"
mkdir -p "$RESULTS_DIR"

# CSV file setup
CSV_FILE="$RESULTS_DIR/erlang_chaos_results.csv"

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

echo "Starting Erlang Chaos experiments..."


echo "Running Chaos 4 : 3"
output_file="$RESULTS_DIR/erl_chaos_t1.txt"
erl -noshell -pa ebin -eval 'test_chaos:run_t1()' -s init stop > "$output_file"
cat "$output_file"
extract_runtime "$output_file" >> "$CSV_FILE"

# echo "Running Chaos 8 : 6"
# output_file="$RESULTS_DIR/erl_chaos_t2.txt"
# erl -noshell -pa ebin -eval 'test_chaos:run_t2()' -s init stop > "$output_file"
# cat "$output_file"
# extract_runtime "$output_file" >> "$CSV_FILE"

# echo "Running Chaos 12 : 10"
# output_file="$RESULTS_DIR/erl_chaos_t3.txt"
# erl -noshell -pa ebin -eval 'test_chaos:run_t3()' -s init stop > "$output_file"
# cat "$output_file"
# extract_runtime "$output_file" >> "$CSV_FILE"

echo "All Erlang experiments complete!"
echo "Results have been saved to:"
echo "- CSV file: $CSV_FILE"
echo "- Individual run logs: $RESULTS_DIR/erl_*.txt"