#!/bin/bash

DATASETS=(1500 3000 6000)
CORE_COUNTS=(1 2 4 8 12 16 24 32 48 64)
RUNS=5
RESULTS_DIR="data/results"
CSV_FILE="$RESULTS_DIR/results.csv"


mkdir -p "$RESULTS_DIR"

# initialize CSV file
echo "dataset,language,implementation,runtime_median,core_count" > "$CSV_FILE"


extract_median_time() {
    local file=$1
    grep "Median Time Taken:" "$file" | awk '{print $4}'
}

run_sequential() {
    local label=$1
    local size=$2
    local output_file

    echo "Running C Sequential..."
    output_file="$RESULTS_DIR/c_seq_${label}.txt"
    ./c/bin/sequential/totient 1 "$size" > "$output_file"
    local time=$(extract_median_time "$output_file")
    echo "$label,c,seq,$time,1" >> "$CSV_FILE"

    echo "Running Go Sequential..."
    output_file="$RESULTS_DIR/go_seq_${label}.txt"
    ./go/bin/totient -mode=sequential -lower=1 -upper="$size" -runs="$RUNS" > "$output_file"
    time=$(extract_median_time "$output_file")
    echo "$label,go,seq,$time,1" >> "$CSV_FILE"
}

run_parallel() {
    local label=$1
    local size=$2
    local cores=$3
    local output_file

    echo "Running C Parallel with $cores cores..."
    output_file="$RESULTS_DIR/c_par_${label}_${cores}.txt"
    export OMP_NUM_THREADS=$cores
    ./c/bin/parallel/totient 1 "$size" > "$output_file"
    local time=$(extract_median_time "$output_file")
    echo "$label,c,par,$time,$cores" >> "$CSV_FILE"

    echo "Running Go Parallel with $cores goroutines..."
    output_file="$RESULTS_DIR/go_par_${label}_${cores}.txt"
    ./go/bin/totient -mode=parallel -lower=1 -upper="$size" -runs="$RUNS" -maxThreads="$cores" > "$output_file"
    time=$(extract_median_time "$output_file")
    echo "$label,go,par,$time,$cores" >> "$CSV_FILE"
}


echo "Starting experiments..."
echo "-------------------"

for i in "${!DATASETS[@]}"; do
    label="DS1$(i+1)"
    size="${DATASETS[i]}"

    echo "Processing $label (size: $size)"

    if [[ $i != 2 ]]; then
        run_sequential "$label" "$size"
    else
       echo "Skipping sequential runs for $label (size: $size)"
    fi

    for cores in "${CORE_COUNTS[@]}"; do
        run_parallel "$label" "$size" "$cores"
    done
    
    echo "$label complete"
    echo "-------------------"
done

echo "All done!"
echo "Results have been saved to:"
echo "- CSV file: $CSV_FILE"
echo "- Individual run logs: $RESULTS_DIR/*.txt"