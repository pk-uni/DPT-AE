# Build settings
.PHONY: all c-build go-build clean run-experiments

# Default target builds everything
all: c-build go-build erl-build

# Build C implementations
c-build:
	@echo "Building C implementations..."
	@$(MAKE) -C c

# Build Go implementations
go-build:
	@echo "Building Go implementations..."
	@$(MAKE) -C go

# Build Go implementations
erl-build:
	@echo "Building Erlang implementations..."
	@$(MAKE) -C erlang

# Run all experiments
run-experiments: all
	@echo "Running all experiments..."
	@./scripts/run_experiments.sh

# Clean everything
clean:
	@echo "Cleaning all build artifacts and results..."
	@$(MAKE) -C c clean
	@$(MAKE) -C go clean
	@rm -rf data/results/*