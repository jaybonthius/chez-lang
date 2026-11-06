SCHEME ?= /usr/lib/csv10.4.1/ta6le/scheme
CHEZ_SRC ?= tests/chez
TEST_TIMEOUT ?= 600
LONG_TIMEOUT ?= 1700

OBJ := build/obj
RUN := $(SCHEME) -q --libdirs src::$(OBJ)/src --program
TESTS := $(sort $(wildcard tests/*-test.sps))

export CHEZ_SRC

.PHONY: build test corpus-test error-test exhaustive-test bench clean

build:
	$(SCHEME) -q --script build.ss

test: build
	@set -e; for t in $(TESTS); do \
	  echo "$$t"; \
	  timeout $(TEST_TIMEOUT) $(RUN) $(OBJ)/$${t%.sps}.so; \
	done

corpus-test: build
	timeout $(TEST_TIMEOUT) $(RUN) $(OBJ)/tests/corpus-test.so

error-test: build
	timeout $(TEST_TIMEOUT) $(RUN) $(OBJ)/tests/error-test.so

exhaustive-test: build
	timeout $(LONG_TIMEOUT) $(RUN) $(OBJ)/tests/exhaustive-test.so 4

bench: build
	timeout $(TEST_TIMEOUT) $(RUN) $(OBJ)/bench/read.so

clean:
	rm -rf build
