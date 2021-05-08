# Makefile for the ActorForth project.


# Define tools to use.
CC 		= clang
CXX 	= clang++
STRIP 	= strip
MAKEDIR	= mkdir -p
RM 		= rm
AR 		= llvm-ar

# Name of the main binary.
OUTPUT := libparser
BIN_OUT	:= actorforth
VERSION := 1.0

# Get target architecture.
ARCH = $(shell $(CXX) -dumpmachine)


INCLUDES = -Icpp/include -Icpp/include \
			-Icpp/coro/include/felspar -Icpp/coro/include/felspar/coro -Icpp/coro/include
SOURCES := cpp/src/continuation.cpp cpp/src/operation.cpp cpp/src/parser.cpp cpp/src/stack.cpp \
			 cpp/src/type.cpp $(wildcard cpp/src/types/*.cpp)
MAIN_SRC :=  cpp/src/repl.cpp

# Create object file names.
OBJECTS := $(addprefix obj/static/$(ARCH)/,$(notdir) $(SOURCES:.cpp=.o))
SHARED_OBJECTS := $(addprefix obj/shared/$(ARCH)/,$(notdir) $(SOURCES:.cpp=.o))
MAIN_OBJ := $(addprefix obj/static/$(ARCH)/,$(notdir) $(MAIN_SRC:.cpp=.o))

# Set compile flags for static & shared builds.
FLAGS 		:= $(INCLUDES) -g3 -std=c++20  -fcoroutines-ts -stdlib=libc++ -O0 \
					-fprofile-instr-generate -fcoverage-mapping -fdiagnostics-show-option
SHARED_FLAGS := 
LD_FLAGS 	:= -shared -Wl,-soname,$(OUTPUT).so.$(VERSION)
LIBS 		:= 

# Add platform-specific flags.
ifdef OS
	# Windows platform.
	#LD_FLAGS += -lclang_rt.profile-x86_64
	#FLAGS += --coverage
else
	# Linux, etc.
	SHARED_FLAGS += -fPIC
endif

# Test-related files.
TEST_INCLUDE := -Icpp/doctest -Icpp/doctest/doctest/extensions -Icpp/doctest/doctest/parts
TEST_DOCSRC := cpp/doctest/doctest/parts/doctest.cpp
TEST_DOCOBJ := $(addprefix obj/static/$(ARCH)/,$(notdir) $(TEST_DOCSRC:.cpp=.o))
TEST_SOURCES :=  $(wildcard cpp/tests/*.cpp)
TEST_OBJECTS := $(addprefix obj/tests/$(ARCH)/,$(notdir) $(TEST_SOURCES:.cpp=.o))
TEST_BINS	:= $(addprefix bin/tests/$(ARCH)/,$(basename $(notdir $(TEST_OBJECTS))))


all: makedir static shared binary tests


makedir:
	$(MAKEDIR) bin/$(ARCH)
	$(MAKEDIR) bin/tests/$(ARCH)
	$(MAKEDIR) obj/tests/$(ARCH)/cpp/tests
	$(MAKEDIR) obj/shared/$(ARCH)/cpp/src/types
	$(MAKEDIR) obj/static/$(ARCH)/cpp/src/types
	$(MAKEDIR) obj/static/$(ARCH)/cpp/doctest/doctest/parts
	

static: bin/$(ARCH)/$(OUTPUT).a

shared: bin/$(ARCH)/$(OUTPUT).so.$(VERSION)

binary: $(MAIN_OBJ) bin/$(ARCH)/$(BIN_OUT)

	
obj/static/$(ARCH)/%.o: %.cpp
	$(CXX) -c -o $@ $< $(FLAGS)
	
obj/shared/$(ARCH)/%.o: %.cpp
	$(CXX) -c -o $@ $<  $(SHARED_FLAGS) $(FLAGS) $(LIBS)
	
bin/$(ARCH)/$(OUTPUT).a: $(OBJECTS)
	-rm -f $@
	$(AR) rcs $@ $^
	
bin/$(ARCH)/$(OUTPUT).so.$(VERSION): $(SHARED_OBJECTS)
# FIXME: Detect Windows platform & disable linking.
ifndef OS
	$(CXX) -o $@ $(FLAGS) $(SHARED_OBJECTS) $(LIBS) $(LD_FLAGS)
endif

bin/$(ARCH)/$(BIN_OUT): static $(MAIN_OBJ)
	$(CXX) -o $@ $(FLAGS) $(OBJECTS) $(TEST_DOCSRC) $(MAIN_OBJ)
	cp $@ $@.debug
	$(STRIP) -S --strip-unneeded $@


# ---- TESTS ---

build_tests: static $(TEST_DOCOBJ) $(TEST_BINS)

tests: run_tests

obj/tests/$(ARCH)/%.o: %.cpp
	$(CXX) -c -o $@ $< $(FLAGS) $(TEST_INCLUDE)
	
bin/tests/$(ARCH)/%: $(TEST_OBJECTS)
	$(CXX) -o $@ $(FLAGS) obj/tests/$(ARCH)/cpp/tests/$(basename $(notdir $@)).o $(OBJECTS)
	
run_tests: test_parser test_stack test_type test_operation test_help

test_parser: build_tests
ifndef OS
	$(info === Test: test_parser ===)
	perf stat bin/tests/$(ARCH)/test_parser < cpp/tests/data/parseme.a4	
else
	$(info > No perf support on Windows. < )
endif

test_stack: build_tests
	$(info === Test: test_stack ===)
	LLVM_PROFILE_FILE="bin/tests/$(ARCH)/test_stack.profraw" \
	./bin/tests/$(ARCH)/test_stack -s -d
	
test_type: build_tests
	$(info === Test: test_type ===)
	LLVM_PROFILE_FILE="./bin/tests/$(ARCH)/test_type.profraw" ./bin/tests/$(ARCH)/test_type -s -d
	
test_operation: build_tests
	$(info === Test: test_operation ===)
	LLVM_PROFILE_FILE="./bin/tests/$(ARCH)/test_operation.profraw" ./bin/tests/$(ARCH)/test_operation -s -d

test_help: build_tests binary
ifndef OS
	$(info === Test: actorforth help ===)
	perf stat bin/$(ARCH)/$(BIN_OUT) --help
else
	$(info > No perf support on Windows. < )
endif
	

# --- CLEAN ---
	
clean: clean-static clean-shared clean-tests

clean-static:
	$(RM) $(OBJECTS)
	
clean-shared:
	$(RM) $(SHARED_OBJECTS)
	
clean-tests:
	$(RM) $(TEST_DOCOBJ) $(MAIN_OBJ) $(TEST_OBJECTS)
	$(RM) bin/tests/$(ARCH)/test_stack.profraw
	$(RM) bin/tests/$(ARCH)/test_type.profraw
	$(RM) bin/tests/$(ARCH)/test_operation.profraw
	

.PHONY: tests static shared makedir all


# [x] make clean all

# 'make test' or 'make test  <test file>'

# make perf


# make perf <test file>

# [X] make clean

# perf stat

# collect profile data
