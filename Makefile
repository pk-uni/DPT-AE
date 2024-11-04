# Datasets for experiments
DS1 = 15000
DS2 = 30000
DS3 = 60000

# Directory structure
DATA_DIR = data
RESULTS_DIR = $(DATA_DIR)/results

# Make sure results directory exists
$(shell mkdir -p $(RESULTS_DIR))

.PHONY: all c-build go-build clean run-all run-ds1 run-ds2 run-ds3

# Default target builds everything
all: c-build go-build

# Build C implementations
c-build:
	$(MAKE) -C c

# Build Go implementations
go-build:
	$(MAKE) -C go

# Run experiments for each dataset
run-ds1: all
	./c/bin/sequential/totient 1 $(DS1) > $(RESULTS_DIR)/c_seq_ds1.txt
	./c/bin/parallel/totient 1 $(DS1) > $(RESULTS_DIR)/c_par_ds1.txt
	./go/bin/totient -mode=sequential -lower=1 -upper=$(DS1) > $(RESULTS_DIR)/go_seq_ds1.txt
	./go/bin/totient -mode=parallel -lower=1 -upper=$(DS1) > $(RESULTS_DIR)/go_par_ds1.txt

run-ds2: all
	./c/bin/sequential/totient 1 $(DS2) > $(RESULTS_DIR)/c_seq_ds2.txt
	./c/bin/parallel/totient 1 $(DS2) > $(RESULTS_DIR)/c_par_ds2.txt
	./go/bin/totient -mode=sequential -lower=1 -upper=$(DS2) > $(RESULTS_DIR)/go_seq_ds2.txt
	./go/bin/totient -mode=parallel -lower=1 -upper=$(DS2) > $(RESULTS_DIR)/go_par_ds2.txt

run-ds3: all
	./c/bin/sequential/totient 1 $(DS3) > $(RESULTS_DIR)/c_seq_ds3.txt
	./c/bin/parallel/totient 1 $(DS3) > $(RESULTS_DIR)/c_par_ds3.txt
	./go/bin/totient -mode=sequential -lower=1 -upper=$(DS3) > $(RESULTS_DIR)/go_seq_ds3.txt
	./go/bin/totient -mode=parallel -lower=1 -upper=$(DS3) > $(RESULTS_DIR)/go_par_ds3.txt

# Run all datasets
run-all: run-ds1 run-ds2 run-ds3

# Clean everything
clean:
	$(MAKE) -C c clean
	$(MAKE) -C go clean
	rm -rf $(RESULTS_DIR)/*