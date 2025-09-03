# Makefile for Advanced Knowledge Base System

EMACS ?= emacs
ELPA_DIR = $(HOME)/.emacs.d/elpa

# Source files
LISP_FILES = $(wildcard lisp/*.el)
TEST_FILES = $(wildcard test/*.el)

# Compiled files
COMPILED_FILES = $(LISP_FILES:.el=.elc)

.PHONY: all compile test clean install lint

all: compile

# Compile all elisp files
compile: $(COMPILED_FILES)

%.elc: %.el
	$(EMACS) -Q -batch -L lisp -f batch-byte-compile $<

# Run tests
test:
	@echo "No tests available"

# Clean compiled files
clean:
	rm -f $(COMPILED_FILES)

# Install dependencies (if any)
install:
	@echo "No external elisp dependencies to install"

# Basic lint check
lint:
	$(EMACS) -Q -batch -L lisp --eval "(checkdoc-file \"lisp/kb-advanced-system.el\")"

# Generate documentation
docs:
	@echo "Generating documentation..."
	$(EMACS) -Q -batch -L lisp -l lisp/kb-advanced-system.el --eval "(describe-function 'kb-assert)" > docs/api.txt

# Package for distribution
package: compile
	tar -czf kb-advanced-system.tar.gz lisp/ docs/ examples/ README.md

# Run examples
example-basic:
	$(EMACS) -Q -batch -L lisp -l examples/basic-usage.el

# Show help
help:
	@echo "Available targets:"
	@echo "  all       - Compile all files (default)"
	@echo "  compile   - Byte-compile elisp files" 
	@echo "  test      - Run test suite"
	@echo "  clean     - Remove compiled files"
	@echo "  lint      - Run basic linting"
	@echo "  docs      - Generate documentation"
	@echo "  package   - Create distribution package"
	@echo "  help      - Show this help"
