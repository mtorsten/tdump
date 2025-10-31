# Makefile for tdump - Hexadecimal dump utility
# Pascal implementation using Free Pascal Compiler (fpc)

# Program name and version
PROGRAM = tdump
VERSION = 1.0
SOURCE = tdump.pas

# Compiler and flags
FPC = fpc
FPCFLAGS = -O2 -Xs -XX -CX
DEBUG_FLAGS = -g -gl -Ci -Co -Cr -Ct

# Installation directories
PREFIX = /usr/local
BINDIR = $(PREFIX)/bin
MANDIR = $(PREFIX)/share/man/man1

# Build directories
BUILDDIR = build
DEBUGDIR = debug

# Default target
.PHONY: all
all: $(PROGRAM)

# Main build target
$(PROGRAM): $(SOURCE)
	@echo "Building $(PROGRAM)..."
	$(FPC) $(FPCFLAGS) -o$(PROGRAM) $(SOURCE)
	@echo "Build complete: $(PROGRAM)"

# Debug build
.PHONY: debug
debug: $(DEBUGDIR)/$(PROGRAM)

$(DEBUGDIR)/$(PROGRAM): $(SOURCE)
	@mkdir -p $(DEBUGDIR)
	@echo "Building debug version of $(PROGRAM)..."
	$(FPC) $(DEBUG_FLAGS) -o$(DEBUGDIR)/$(PROGRAM) $(SOURCE)
	@echo "Debug build complete: $(DEBUGDIR)/$(PROGRAM)"

# Test the program
.PHONY: test
test: $(PROGRAM)
	@echo "Running basic tests..."
	@echo "Test 1: Help output"
	./$(PROGRAM) --help > /dev/null
	@echo "Test 2: Version check (implicit)"
	@echo "Hello World!" > test_input.tmp
	./$(PROGRAM) test_input.tmp > /dev/null
	@echo "Test 3: Stdin input"
	echo "Testing stdin" | ./$(PROGRAM) > /dev/null
	@echo "Test 4: Verbose mode"
	./$(PROGRAM) -v test_input.tmp > /dev/null
	@echo "Test 5: Word mode"
	./$(PROGRAM) -w test_input.tmp > /dev/null
	@echo "Test 6: DWord mode"
	./$(PROGRAM) -d test_input.tmp > /dev/null
	@rm -f test_input.tmp
	@echo "All tests passed!"

# Install the program
.PHONY: install
install: $(PROGRAM)
	@echo "Installing $(PROGRAM) to $(BINDIR)..."
	install -d $(BINDIR)
	install -m 755 $(PROGRAM) $(BINDIR)/$(PROGRAM)
	@echo "Installation complete."
	@echo "You can now run '$(PROGRAM)' from anywhere."

# Uninstall the program
.PHONY: uninstall
uninstall:
	@echo "Uninstalling $(PROGRAM) from $(BINDIR)..."
	rm -f $(BINDIR)/$(PROGRAM)
	@echo "Uninstallation complete."

# Clean build artifacts
.PHONY: clean
clean:
	@echo "Cleaning build artifacts..."
	rm -f $(PROGRAM)
	rm -f *.o *.ppu *.compiled
	rm -rf $(BUILDDIR) $(DEBUGDIR)
	rm -f test_input.tmp
	@echo "Clean complete."

# Show build information
.PHONY: info
info:
	@echo "Program: $(PROGRAM) v$(VERSION)"
	@echo "Source: $(SOURCE)"
	@echo "Compiler: $(FPC)"
	@echo "Flags: $(FPCFLAGS)"
	@echo "Install prefix: $(PREFIX)"
	@echo "Binary directory: $(BINDIR)"

# Check if required tools are available
.PHONY: check
check:
	@echo "Checking build requirements..."
	@which $(FPC) > /dev/null || (echo "Error: Free Pascal Compiler (fpc) not found" && exit 1)
	@echo "Free Pascal Compiler: $$($(FPC) -iV)"
	@echo "All requirements satisfied."

# Create a source distribution
.PHONY: dist
dist: clean
	@echo "Creating source distribution..."
	@mkdir -p $(PROGRAM)-$(VERSION)
	@cp $(SOURCE) Makefile README.md $(PROGRAM)-$(VERSION)/ 2>/dev/null || cp $(SOURCE) Makefile $(PROGRAM)-$(VERSION)/
	@tar -czf $(PROGRAM)-$(VERSION).tar.gz $(PROGRAM)-$(VERSION)
	@rm -rf $(PROGRAM)-$(VERSION)
	@echo "Source distribution created: $(PROGRAM)-$(VERSION).tar.gz"

# Show help
.PHONY: help
help:
	@echo "Available targets:"
	@echo "  all       - Build the program (default)"
	@echo "  debug     - Build debug version with debugging symbols"
	@echo "  test      - Run basic functionality tests"
	@echo "  install   - Install the program to $(BINDIR)"
	@echo "  uninstall - Remove the program from $(BINDIR)"
	@echo "  clean     - Remove build artifacts"
	@echo "  check     - Check build requirements"
	@echo "  info      - Show build information"
	@echo "  dist      - Create source distribution tarball"
	@echo "  help      - Show this help message"
	@echo ""
	@echo "Variables:"
	@echo "  PREFIX    - Installation prefix (default: $(PREFIX))"
	@echo "  FPC       - Pascal compiler (default: $(FPC))"
	@echo "  FPCFLAGS  - Compiler flags (default: $(FPCFLAGS))"