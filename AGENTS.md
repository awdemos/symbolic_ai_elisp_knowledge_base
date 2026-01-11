# AGENTS.md - Coding Guidelines for Repository

This document provides coding guidelines and instructions for AI agents working on this Symbolic AI Knowledge Base project.

## Quick Start: Running Tests

### Running a Single Test

```bash
# Method 1: Direct Emacs batch mode
emacs --batch --eval "(progn (setq load-path (cons \"$(pwd)/lisp\" load-path)) (require 'kb-advanced-system) (kb-init) (ert-run-test (ert-get-test 'test-basic-assert-query)))"

# Method 2: Using the test runner
emacs --batch --eval "(progn (setq load-path (cons \"$(pwd)/lisp\" load-path)) (load \"test/run-tests.el\") (ert-run-tests-batch))"

# Method 3: Load specific test files
emacs --batch -l test/test-core.el -f ert-run-tests-batch-and-exit
```

### Running All Tests

```bash
emacs --batch -l test/run-tests.el -f ert-run-tests-batch-and-exit
```

### Running Specific Test Suites

```bash
# Run only core tests
emacs --batch -l test/run-tests.el --eval "(ert-select-tests-set '(test-basic-assert-query test-multiple-facts test-kb-init))" -f ert-run-tests-batch

# Run specific test by name
emacs --batch -l test/test-core.el --eval "(ert-run-test (ert-get-test 'test-basic-assert-query))"
```

### Debugging Test Failures

```bash
# Run test with verbose output
emacs --batch -l test/test-core.el --eval "(progn (setq ert-verbosity 2) (ert-run-test (ert-get-test 'test-basic-assert-query)))"

# Check test output in *Messages* buffer
emacs --batch -l test/test-core.el --eval "(progn (ert-run-test (ert-get-test 'test-basic-assert-query)) (message \"Test completed\"))"
```

---

## Code Style Guidelines

### File Structure

```
symbolic_ai_elisp_knowledge_base/
├── lisp/                 # Core Lisp modules
│   ├── kb-advanced-system.el    # Main API (requires all other modules)
│   ├── kb-microtheories.el      # Microtheory/context management
│   ├── kb-persistence.el          # Save/load functionality
│   ├── kb-validation.el          # Input validation and error handling
│   ├── kb-tms.el                # Truth Maintenance System
│   ├── kb-nonmonotonic.el       # Default reasoning
│   ├── kb-events.el              # Event reification
│   ├── kb-inference-engine.el    # Layered inference engine
│   ├── kb-rdf.el                # RDF/OWL import/export
│   ├── kb-cache.el              # Query caching
│   ├── kb-debugger.el           # Debugging tools
│   ├── kb-testing.el            # Testing framework
│   └── kb-system.el             # Legacy compatibility
├── test/                  # Test suite
│   ├── run-tests.el            # Main test runner
│   ├── test-core.el            # Core functionality tests
│   ├── test-microtheories.el   # Microtheory tests (temporarily skipped due to corruption)
│   ├── test-inheritance.el       # Inheritance tests
│   ├── test-kb-persistence.el  # Persistence tests (14 failures from incomplete features)
│   ├── test-simple-persistence.el
│   ├── test-nonmonotonic.el     # Default reasoning tests
│   └── test-temporal.el        # Temporal reasoning tests
├── examples/              # Usage examples
│   ├── basic-usage.el
│   └── validation-demo.el
└── build/               # Build automation
    ├── Dockerfile          # Container build
    ├── Makefile            # Build targets
    └── dagger.py          # Dagger build automation
```

### File Header Format (REQUIRED)

Every `.el` file MUST start with this exact header format:

```elisp
;;; filename.el --- Brief Description
;; -*- lexical-binding: t; -*-

;; Author: AI Assistant
;; Keywords: relevant, keywords, here
;; Version: X.Y

;;; Commentary:

;; Detailed multi-line description of what this file does.
;; Can include examples, usage notes, or implementation details.

;;; Code:

(require 'necessary-libraries)

... code ...

(provide 'filename)
;;; filename.el ends here
```

**Important**: The `;; -*- lexical-binding: t; -*-` line MUST be the second line (after the title line).

### Imports and Requires

```elisp
;;; Correct: Explicit requires at file top
(require 'cl-lib)
(require 'kb-validation)
(require 'kb-microtheories)

;;; Wrong: Duplicate or missing requires
;; Don't: (require 'kb-validation) multiple times in same file
;; Don't: Forget to require dependencies used in code
```

### Naming Conventions

#### Variable Names
- Use descriptive names: `kb-current-mt`, `kb-microtheories`, not `mt`, `mts`
- Constants: `kb-max-depth`, `kb-default-priority` (kebab-case)
- Internal functions: `kb--internal-function` (double dash prefix)
- Hash tables: `kb-facts`, `kb-events` (not `facts`, `events`)

#### Function Names
- Exported functions: `kb-assert`, `kb-query`, `kb-create-microtheory` (lowercase, no double dash)
- Internal functions: `kb--match-premises`, `kb--validate-structure`
- Predicate functions: `kb-microtheory-p`, `kb-fact-subject` (use `-p` suffix for predicates)

#### Structure Accessors
For cl-defstruct structures:
- Use `kb-structure-name-fieldname` format
- Example: `kb-fact-subject`, `kb-fact-predicate`, `kb-fact-object`

### Code Formatting

```elisp
;;; Correct: Proper indentation and spacing
(defun kb-example-function (arg1 arg2)
  "Brief description."
  (let ((var1 value1))
    (when (condition-p var1 arg1)
      (do-something var1 arg2))))

;;; Wrong: Inconsistent indentation or spacing
(defun kb-example-function (arg1 arg2)
  "Brief description."
(let((var1 value1))
(when(condition-p var1 arg1)
(do-something var1 arg2)))
```

**Formatting Rules:**
- Use 2-space indentation for top-level forms
- Use 4-space indentation inside let forms
- Add blank line between top-level definitions
- Add space after opening parenthesis, before closing: `(when (condition` not `(when(condition`
- Limit lines to ~80 characters when possible

### Type Safety

```elisp
;;; NEVER use type suppression
;;; Wrong:
(kb-assert 'Socrates 'is-a 'human)
(kb-get-microtheory mt)  ; Suppresses type errors

;;; Correct: Use proper type checking
(defun kb-safe-function (value)
  (if (kb-valid-type-p value)
      value
    (signal 'kb-type-error (list "Invalid type" value))))
```

### Error Handling

```elisp
;;; Use kb-with-validation macro for validation
(kb-with-validation kb-create-microtheory (list name parent-mts)
  (kb-with-error-recovery
    ;; Implementation here
    ))
```

### Documentation Comments

```elisp
;;; Good: Self-documenting code
;; Calculate the square of a number
(defun kb-square (x)
  "Return X squared."
  (* x x))

;;; Avoid: Excessive inline comments
(defun kb-square (x)  ; This squares the number
  (* x x))  ; And returns the result
```

### Testing Guidelines

```elisp
;;; Test structure
(ert-deftest test-name ()
  "Brief description of what is being tested."
  (kb-init)  ; Set up clean state
  (let ((result (kb-assert 'Socrates 'is-a 'human)))
    (should result)  ; Check that assertion succeeded
    (should (kb-query 'Socrates 'is-a))))  ; Verify query works

;;; Clean up after tests
  (ert-deftest test-teardown ()
  "Clean up KB state after tests."
  (kb-clear))
```

### Performance Guidelines

```elisp
;;; Use hash tables for fast lookups
(setq kb-index (make-hash-table :test 'equal))
(puthash key value kb-index)

;;; Avoid repeated calculations
;; Use memoization for expensive operations
(defun kb-expensive-function (x)
  (let ((cache (make-hash-table :test 'equal)))
    (or (gethash x cache)
        (puthash x (calculate-result x) cache))))

;;; Limit recursion depth
(defconst kb-max-recursion-depth 100)
```

### Module Integration

```elisp
;;; Provide consistent API
(provide 'kb-module-name)

;;; Use prefix conventions for exported functions
(kb-module-assert ...)    ; Core assertion
(kb-module-create-...)    ; Creation functions
(kb-module-query-...)     ; Query functions
```

---

## Error Handling Patterns

### Validation Errors

```elisp
;;; Always use kb-with-validation macro
(kb-with-validation kb-assert-fact (list subject predicate object)
  (kb-with-error-recovery
    ;; Implementation with proper validation
    ))

;;; User-friendly error messages
(signal 'kb-validation-error 
        (list "Subject must be a symbol" 
              subject 
              "Try using quote (quote foo) instead of plain symbol"))
```

### Graceful Degradation

```elisp
;;; When features fail, provide fallback behavior
(defun kb-inference-with-fallback (query)
  "Attempt inference, fall back to direct query on failure."
  (condition-case err
      (let ((result (kb-infer query)))
        result)
    (kb-inference-error
      ;; Fallback: direct query
      (kb-query query))))
```

---

## Agent Configuration

### OpenCode Agent Settings

When working in this repository, OpenCode agents (Sisyphus and similar) should:

1. **Always check for AGENTS.md** before starting work
2. **Follow the coding guidelines** in this file
3. **Use provided test commands** for verification
4. **Update AGENTS.md** when adding new patterns or conventions
5. **Run diagnostics** after significant changes (lsp-diagnostics)

### Model-Specific Settings

For **Claude models** (like Sisyphus):
- Model: claude-3.5-sonnet or claude-3.7-sonnet (latest recommended)
- Settings location: `~/.config/` or equivalent
- Ensure settings are loaded before starting

For **OpenAI models**:
- Model: gpt-4 or gpt-4-turbo
- Configure appropriate settings for best results

### Environment Variables

```bash
# Set model path if using local models
export MODEL_PATH="/path/to/model"

# Enable verbose output for debugging
export VERBOSE=1

# Set test output format
export TEST_OUTPUT=verbose
```

---

## Repository-Specific Notes

### Known Issues

1. **test-microtheories.el** - File corruption/encoding issues
   - Status: Temporarily bypassed in run-tests.el
   - Needs: Recreation with proper encoding or investigation

2. **14 test failures in test-kb-persistence.el**
   - Cause: Tests for incomplete features (auto-save, incremental save, etc.)
   - Action: Either implement features or remove tests
   - Impact: Not critical for system operation

3. **All .el files now have lexical-binding declarations**
   - Status: ✅ Completed
   - Impact: Eliminates compilation warnings

### Module Dependencies

```
kb-advanced-system.el (main API)
├── kb-microtheories.el      (required)
├── kb-persistence.el          (required)
├── kb-validation.el          (required)
├── kb-tms.el                (required)
├── kb-nonmonotonic.el       (optional)
├── kb-events.el              (optional)
├── kb-inference-engine.el    (optional)
├── kb-rdf.el                (optional)
├── kb-cache.el              (optional)
├── kb-debugger.el           (optional)
└── kb-system.el             (legacy compatibility)
```

### Test Status

```bash
# Quick test status
emacs --batch -l test/run-tests.el -f ert-run-tests-batch | grep "Ran"

# Specific test status
emacs --batch -l test/test-core.el -f ert-run-test (ert-get-test 'test-basic-assert-query) 2>&1 | tail -5
```

---

## Checklist for New Work

Before committing changes, verify:

- [ ] All files have `;; -*- lexical-binding: t; -*-` header
- [ ] Code follows naming conventions
- [ ] Error handling uses proper macros
- [ ] New functions are documented
- [ ] Tests pass (or are documented as known failures)
- [ ] No compilation warnings
- [ ] No linter errors

---

## Build System

### Available Commands

```bash
# Compile all .el files
make compile

# Run all tests
make test

# Docker build
docker build -t symbolic-kb .

# Dagger build
dagger build
```

### Docker Workflow

```bash
# Build image
docker build -t symbolic-kb .

# Run tests in container
docker run --rm symbolic-kb emacs --batch -l test/run-tests.el -f ert-run-tests-batch

# Interactive development
docker run -it -v $(pwd):/app symbolic-kb emacs
```

---

## Quick Reference

### Common Emacs Commands

```elisp
;; Evaluate expression in current buffer
C-x C-e

;; Load file
M-x load-file

;; Compile current buffer
M-x byte-compile-file

;; Run tests
M-x ert-run-tests-interactively

;; Show documentation
C-h (describe-function)
```

### Git Workflow

```bash
# Check status
git status

# Stage all .el files
git add lisp/*.el

# Commit with conventional format
git commit -m "fix: brief description"

# View changes
git diff HEAD

# See commit history
git log --oneline -10
```

---

**Last Updated**: 2026-01-11
**Maintained By**: OpenCode AI Agents
