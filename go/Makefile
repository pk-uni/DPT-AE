# Build settings
GO = go
BUILD_DIR = bin
BINARY_NAME = totient

# Make sure the build directory exists
$(shell mkdir -p $(BUILD_DIR))

# Source directories
CMD_DIR = cmd/totient
MAIN_PKG = ./$(CMD_DIR)

.PHONY: all build clean fmt

# Default target
all: fmt build

# Build the binary
build:
	$(GO) build -o $(BUILD_DIR)/$(BINARY_NAME) $(MAIN_PKG)

# Format code (keeping this as it's a good practice)
fmt:
	$(GO) fmt ./...

# Clean build artifacts
clean:
	rm -rf $(BUILD_DIR)
	$(GO) clean