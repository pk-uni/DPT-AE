#!/bin/bash

# Configuration
DATASETS=(1500 3000 6000)
RUNS=5
RESULTS_DIR="data/results"

mkdir -p "$RESULTS_DIR"

# Function to run experiment for a specific dataset
run_experiment() {
    local size=$1
    local dataset_num=$2
    echo "Running Dataset $dataset_num (range: 1 to $size)..."

    # C Sequential
    echo "Running C Sequential..."
    echo "C Sequential:" > "$RESULTS_DIR/c_seq_ds${dataset_num}.txt"
    ./c/bin/sequential/totient 1 "$size" >> "$RESULTS_DIR/c_seq_ds${dataset_num}.txt"

    # C Parallel
    echo "Running C Parallel..."
    echo "C Parallel:" > "$RESULTS_DIR/c_par_ds${dataset_num}.txt"
    ./c/bin/parallel/totient 1 "$size" >> "$RESULTS_DIR/c_par_ds${dataset_num}.txt"

    # Go Sequential
    echo "Running Go Sequential..."
    echo "Go Sequential:" > "$RESULTS_DIR/go_seq_ds${dataset_num}.txt"
    ./go/bin/totient -mode=sequential -lower=1 -upper="$size" -runs="$RUNS" >> "$RESULTS_DIR/go_seq_ds${dataset_num}.txt"

    # Go Parallel
    echo "Running Go Parallel..."
    echo "Go Parallel:" > "$RESULTS_DIR/go_par_ds${dataset_num}.txt"
    ./go/bin/totient -mode=parallel -lower=1 -upper="$size" -runs="$RUNS" >> "$RESULTS_DIR/go_par_ds${dataset_num}.txt"
}

# Main execution
echo "Starting experiments..."

for i in "${!DATASETS[@]}"; do
    dataset_num=$((i + 1))
    run_experiment "${DATASETS[$i]}" "$dataset_num"
    echo "Dataset $dataset_num complete"
    echo "-------------------"
done

echo "All experiments complete!"