# Erlang compiler and flags
ERLC = erlc
ERLC_FLAGS = +debug_info +warn_export_vars

# Directories
EBIN_DIR = ebin
SRC_DIR = src

# Source files
SOURCES = $(wildcard $(SRC_DIR)/*.erl)
BEAMS = $(patsubst $(SRC_DIR)/%.erl,$(EBIN_DIR)/%.beam,$(SOURCES))

# Ensure ebin directory exists
$(shell mkdir -p $(EBIN_DIR))

.PHONY: all clean test-sequential test-parallel test-chaos test-all

all: compile

compile: $(BEAMS)

$(EBIN_DIR)/%.beam: $(SRC_DIR)/%.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

clean:
	rm -rf $(EBIN_DIR)/*.beam erl_crash.dump

test-sequential:
	erl -noshell -pa $(EBIN_DIR) -eval 'test_sequential:run_sequential_experiments(), init:stop().'

test-parallel:
	erl -noshell -pa $(EBIN_DIR) -eval 'test_parallel:run_parallel_experiments(), init:stop().'

test-chaos:
	erl -noshell -pa $(EBIN_DIR) -eval 'test_chaos:run_chaos_experiments(), init:stop().'

test-all:
	erl -noshell -pa $(EBIN_DIR) -eval 'test:run_all(), init:stop().'