# Advanced Knowledge Base System v2.1

Symbolic AI knowledge base system in Emacs Lisp with microtheories, truth maintenance, temporal logic, and data generation capabilities.

> **⚠️ DEVELOPMENT STATUS**: This system is under active development. Several features are experimental or have known issues. See [Current Status](#current-status) and [Known Issues](#known-issues) sections below.

## Current Status

### ✅ Working Features

- **Basic Knowledge Storage**: Facts can be asserted and queried
- **Microtheory Framework**: Context-scoped knowledge with inheritance (basic implementation)
- **Truth Maintenance System (TMS)**: Justification tracking and belief revision
- **Persistence System**: Save/load knowledge base state to files with robust error handling
- **Testing Framework**: Comprehensive test suite for validation
- **Event System**: Event creation and basic temporal reasoning
- **Non-Monotonic Reasoning**: Default rules with exception handling
- **Data Generation**: Python scripts for LLM-powered fact extraction
- **🆕 Validation System**: Comprehensive input validation and error handling across all modules
- **🆕 Error Recovery**: Graceful degradation with helpful error messages and suggestions
- **🆕 User Validation Tools**: Interactive functions to validate facts, microtheories, and system state

### 🔧 Partially Working Features

- **Inference Engine**: Layered inference workers (implementation incomplete)
- **Temporal Reasoning**: Basic temporal facts (full temporal logic not implemented)
- **RDF Integration**: Import/export framework exists but needs testing
- **Query Caching**: LRU cache system implemented but may have issues

### ❌ Known Issues

- **Initialization Problems**: Multiple initialization calls cause errors (partially mitigated by validation)
- **Test Suite Failures**: All 17 tests currently fail due to microtheory re-initialization
- **Incomplete Integration**: Some modules don't properly communicate
- **🆕 Improved**: Error handling significantly enhanced with validation system

## Installation

### Prerequisites

- GNU Emacs 24.3 or later
- Python 3.7+ (for LLM data generation)

### Basic Setup

```elisp
;; Add to your .emacs or init.el
(add-to-list 'load-path "/path/to/symbolic_ai_elisp_knowledge_base/lisp")
(require 'kb-advanced-system)

;; Initialize (with improved error handling)
(kb-init)

;; Enable validation for development (optional)
(setq kb-validation-enabled t)
(setq kb-validation-strict-mode t)
```

### Python Dependencies (Optional)

```bash
# For LLM-powered data generation
cd scripts/
pip install -r requirements.txt
```

## Core API

### Basic Operations (Working)

```elisp
;; Initialize system (do only once)
(kb-init)

;; Assert facts
(kb-assert 'Socrates 'is-a 'human)
(kb-assert 'human 'is-a 'mammal)

;; Query facts
(kb-query 'Socrates 'is-a)  ; Returns facts about Socrates

;; Create microtheories
(kb-create-microtheory 'MyMt 'BaseMt)
```

### Truth Maintenance (Working)

```elisp
;; Initialize TMS
(kb-tms-init)

;; Assert with justification tracking
(kb-tms-assert-fact 'bird 'can-fly t)

;; Check belief status
(kb-tms-is-believed 'bird 'can-fly t)

;; Get explanations
(kb-tms-explain-fact 'bird 'can-fly t)
```

### Persistence (Working)

```elisp
;; Save entire knowledge base
(kb-save "my-kb.el")

;; Load knowledge base
(kb-load "my-kb.el")

;; Create backup
(kb-backup)
```

### Non-Monotonic Reasoning (Partially Working)

```elisp
;; Add default rule
(kb-add-default 'birds-fly 
                 '((?x is-a bird)) 
                 '(?x can-fly t))

;; Add exception
(kb-add-exception 'penguin-exception 
                  'birds-fly
                  '((?x is-a penguin))
                  '(?x can-fly nil))

;; Perform reasoning (may have issues)
(kb-reason)
```

### 🆕 Validation and Error Handling (New in v2.1)

```elisp
;; Enable comprehensive validation
(setq kb-validation-enabled t)
(setq kb-validation-strict-mode t)

;; Operations now include automatic validation
(kb-assert 'John 'loves 'Mary)      ; ✓ Valid - succeeds
(kb-assert nil 'loves 'Mary)        ; ✗ Invalid - helpful error message

;; Check data validity before operations
(kb-check-fact 'Alice 'knows 'Bob)  ; Returns t if valid
(kb-check-microtheory 'TestMt)      ; Validates microtheory name

;; Validate entire system
(kb-validate-system)                ; Comprehensive system check

;; Error recovery example
(condition-case err
    (kb-persist-save "/invalid/path/kb.dat")
  (kb-validation-error
   (message "Save failed: %s" (cadr err))
   (kb-persist-save "~/backup-kb.dat")))  ; Try alternative location

;; Toggle validation for performance
(kb-validation-disable)             ; Disable for production
(kb-validation-enable)              ; Re-enable for safety
```

## Project Structure

```
symbolic_ai_elisp_knowledge_base/
├── README.md                    # This file
├── Makefile                     # Build automation (has issues)
├── lisp/                        # Core Elisp modules
│   ├── kb-advanced-system.el    # Main API (improved error handling)
│   ├── kb-microtheories.el      # Context management (enhanced validation)
│   ├── kb-tms.el               # Truth maintenance (working)
│   ├── kb-persistence.el       # Save/load system (robust error handling)
│   ├── kb-testing.el           # Test framework (working)
│   ├── kb-inference-engine.el  # Layered inference (incomplete)
│   ├── kb-nonmonotonic.el      # Default reasoning (partial)
│   ├── kb-events.el            # Event system (with validation)
│   ├── kb-rdf.el               # RDF integration (untested)
│   ├── kb-debugger.el          # Debug tools (basic)
│   ├── kb-cache.el             # Query caching (issues)
│   ├── kb-validation.el        # 🆕 Comprehensive validation system
│   └── kb-system.el            # Legacy compatibility
├── scripts/                     # Python data generators
│   ├── kb_openai_generator.py   # OpenAI + Outlines integration
│   ├── kb_data_generator.py     # Local LLM version
│   └── requirements.txt         # Python dependencies
├── test/                        # Test files (all failing)
├── examples/                    # Usage examples
│   └── validation-demo.el       # 🆕 Validation system demonstration
├── docs/                        # Additional documentation
│   └── VALIDATION.md           # 🆕 Comprehensive validation guide
```

## Data Generation with LLMs

Extract structured facts from text using Python scripts:

```bash
# Using OpenAI API
python scripts/kb_openai_generator.py --file input.txt --microtheory ScienceMt

# Using local models (if available)
python scripts/kb_data_generator.py --file input.txt --microtheory ScienceMt
```

Generated facts can be loaded into Emacs:

```elisp
(load-file "openai_facts.el")
```

## Testing

⚠️ **Current Issue**: All tests are failing due to initialization problems.

```bash
# Attempt to run tests (will fail)
make test

# Individual test files can be examined in test/
ls test/test-*.el
```

## Known Issues

### Critical Issues

1. **Multiple Initialization Problem**: Calling `kb-init` more than once causes "Microtheory already exists" errors
2. **Test Suite Broken**: All 17 tests fail due to initialization issues
3. **Demo Function Fails**: `(kb-demo)` fails on microtheory re-creation

### Compilation Warnings

- Missing lexical-binding declarations
- Deprecated pattern matching syntax
- Undefined function references between modules
- Free variable references

### Integration Issues

- Inference engine workers not properly connected
- Some modules don't communicate correctly
- Error handling is incomplete

## Build Commands

```bash
# Compile (with warnings)
make compile

# Attempt tests (will fail)
make test

# Clean compiled files  
make clean

# Basic lint check
make lint
```

## Troubleshooting

### "Microtheory already exists" Error

This is a known issue. Avoid calling `kb-init` multiple times. If you encounter this:

1. Restart Emacs
2. Call `kb-init` only once
3. Consider using `(setq kb-microtheories (make-hash-table :test 'equal))` to reset state

### Test Failures

The test suite currently fails due to initialization issues. To help debug:

1. Check individual test files in `test/`
2. Run single functions in isolation
3. Use the TMS and persistence modules directly as they're more stable

### Performance Issues

If you experience slowdowns:

1. Try enabling caching: `(kb-cache-on)`
2. Limit inference depth
3. Use specific microtheories to scope queries

## Development Priorities

1. **Fix initialization system** - Prevent multiple init issues
2. **Repair test suite** - Get all tests passing
3. **Complete inference engine** - Finish layered inference implementation  
4. **Improve error handling** - Make system more robust
5. **Add proper documentation** - Document working vs experimental features

## Contributing

This system needs significant work to reach production quality. High-value contributions:

- Fix the initialization/microtheory creation bug
- Repair the test suite
- Complete the inference engine implementation
- Add proper error handling throughout
- Improve module integration

## Attribution

### Core System
- **Microtheory System**: Influenced by Cyc's context system
- **Truth Maintenance**: Based on Forbus & de Kleer's ATMS
- **Non-Monotonic Reasoning**: Implements Reiter's default logic

### New Features (v2.1)
- **Enhanced TMS**: Extended justification tracking
- **Persistence System**: Full KB serialization/deserialization
- **Testing Framework**: Comprehensive validation system
- **Python Integration**: LLM-powered data generation

---

**This is experimental software. Use with caution and expect rough edges.**
