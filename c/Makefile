# Compiler settings
CC = gcc-14
CFLAGS = -Wall -O3
OMPFLAGS = -fopenmp

# Directories
SRC_DIR = src
BUILD_DIR = bin
SEQ_DIR = sequential
PAR_DIR = parallel
INCLUDE_DIR = include

# Make sure the build directory exists
$(shell mkdir -p $(BUILD_DIR)/$(SEQ_DIR) $(BUILD_DIR)/$(PAR_DIR))

# Source files
SEQ_SRC = $(SRC_DIR)/$(SEQ_DIR)/totient.c
PAR_SRC = $(SRC_DIR)/$(PAR_DIR)/totient.c

# Output executables
SEQ_TARGET = $(BUILD_DIR)/$(SEQ_DIR)/totient
PAR_TARGET = $(BUILD_DIR)/$(PAR_DIR)/totient

.PHONY: all clean sequential parallel

# Default target
all: sequential parallel

# Sequential version
sequential: $(SEQ_TARGET)

$(SEQ_TARGET): $(SEQ_SRC)
	$(CC) $(CFLAGS) -I$(INCLUDE_DIR) $< -o $@

# Parallel version
parallel: $(PAR_TARGET)

$(PAR_TARGET): $(PAR_SRC)
	$(CC) $(CFLAGS) $(OMPFLAGS) -I$(INCLUDE_DIR) $< -o $@

# Clean build artifacts
clean:
	rm -rf $(BUILD_DIR)