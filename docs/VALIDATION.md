# KB Validation System Documentation

## Overview

The Knowledge Base system now includes comprehensive error handling and validation to ensure data integrity and provide graceful degradation when errors occur. The validation system prevents crashes and provides helpful error messages with suggestions for correction.

## Features

### 1. Input Validation
- **Fact Validation**: Ensures facts have proper structure and types
- **Microtheory Validation**: Validates names, inheritance hierarchies, and prevents cycles  
- **Rule Validation**: Checks rule premises, conclusions, and priorities
- **Event Validation**: Validates event types, participants, and temporal information
- **Query Validation**: Ensures valid query parameters and microtheory references

### 2. Error Types
Custom error types for different validation scenarios:
- `kb-validation-error` - General validation error
- `kb-microtheory-error` - Microtheory-specific errors
- `kb-fact-error` - Fact validation errors
- `kb-rule-error` - Rule validation errors
- `kb-event-error` - Event validation errors
- `kb-temporal-error` - Temporal logic errors
- `kb-type-error` - Type validation errors

### 3. Error Recovery
- **Graceful Degradation**: System continues operation when possible
- **Backup Recovery**: Automatic recovery from backups during load failures
- **TMS Error Handling**: Truth maintenance system errors are contained
- **Rollback Capability**: Failed operations can be rolled back

### 4. User-Friendly Features
- **Helpful Error Messages**: Clear descriptions of what went wrong
- **Correction Suggestions**: Automatic suggestions for fixing common errors
- **Interactive Validation**: Functions to validate data before operations
- **Statistics Tracking**: Monitor validation activity

## Configuration

### Validation Settings

```elisp
;; Enable/disable validation (default: t)
(setq kb-validation-enabled t)

;; Enable strict mode for more restrictive checks (default: nil)
(setq kb-validation-strict-mode nil)

;; Maximum recursion depth for validation (default: 100)
(setq kb-validation-max-recursion-depth 100)
```

### Interactive Controls

```elisp
;; Toggle validation on/off
(kb-validation-enable)
(kb-validation-disable)

;; Toggle strict mode
(kb-validation-toggle-strict-mode)

;; Validate entire system
(kb-validate-system)
```

## Validation Functions

### Core Validation Functions

#### `kb-validate-fact-structure (subject predicate object)`
Validates the basic structure of a fact.

```elisp
;; Valid fact
(kb-validate-fact-structure 'John 'loves 'Mary) ; ✓

;; Invalid facts
(kb-validate-fact-structure nil 'loves 'Mary)    ; ✗ Error: Subject cannot be nil
(kb-validate-fact-structure 'John 'loves 'John) ; ✗ Error: Subject and object identical
```

#### `kb-validate-microtheory-name (name)`
Validates microtheory naming conventions.

```elisp
;; Valid names
(kb-validate-microtheory-name 'MyMicrotheory)    ; ✓
(kb-validate-microtheory-name 'BaseMt)           ; ✓

;; Invalid names  
(kb-validate-microtheory-name "123BadName")      ; ✗ Error: Cannot start with digit
(kb-validate-microtheory-name 'bad@name)        ; ✗ Error: Invalid characters
```

#### `kb-validate-rule-params (name premises conclusion &optional priority temporal-p mt)`
Validates rule creation parameters.

```elisp
;; Valid rule
(kb-validate-rule-params 'my-rule
                        '((loves ?x ?y))
                        '(loves ?y ?x)) ; ✓

;; Invalid rule
(kb-validate-rule-params 'bad-rule nil '(conclusion)) ; ✗ Error: Premises cannot be nil
```

### User-Accessible Functions

#### `kb-check-fact (subject predicate object)`
Check if a fact is valid without asserting it.

```elisp
(kb-check-fact 'Alice 'knows 'Bob)  ; Returns t if valid
(kb-check-fact nil 'knows 'Bob)     ; Returns nil, shows error message
```

#### `kb-check-microtheory (name)`
Check if a microtheory name is valid.

```elisp
(kb-check-microtheory 'ValidName)   ; Returns t if valid
(kb-check-microtheory '123Invalid)  ; Returns nil, shows error message
```

#### `kb-validate-system ()`
Validate the current state of the entire KB system.

```elisp
(kb-validate-system) ; Checks all microtheories, facts, and relationships
```

## Error Handling Patterns

### Using `kb-with-error-recovery`

```elisp
(kb-with-error-recovery
  ;; Your KB operations here
  (kb-assert 'John 'loves 'Mary)
  (kb-create-microtheory 'TestMt)
  ;; Errors are caught and handled gracefully
  )
```

### Custom Error Handling

```elisp
(condition-case err
    (kb-assert nil 'invalid 'fact)
  (kb-fact-error
   (message "Fact validation failed: %s" (cadr err)))
  (kb-validation-error  
   (message "General validation error: %s" (cadr err))))
```

## Validation in Different Modules

### Main API Functions (kb-advanced-system.el)
All major API functions now include validation:
- `kb-assert` - Validates fact structure, certainty, temporal info
- `kb-query` - Validates query parameters and microtheory existence
- `kb-retract` - Validates retraction parameters
- `kb-add-rule` - Validates rule structure and parameters
- `kb-ask` - Validates complex queries and timeouts

### Microtheory Operations (kb-microtheories.el)  
- `kb-create-microtheory` - Validates name, parents, inheritance modes
- `kb-add-fact` - Enhanced with comprehensive fact validation
- Cycle detection in inheritance hierarchies
- Automatic error recovery for TMS operations

### Event System (kb-events.el)
- `kb-create-event` - Validates event types, participants, temporal info
- Temporal consistency checking
- Event fact generation with error containment

### Persistence (kb-persistence.el)
- `kb-persist-save` - File system validation, backup creation
- `kb-persist-load` - File format validation, recovery mechanisms
- Large file warnings and user confirmation
- Automatic backup restoration on load failures

## Performance Considerations

### Validation Overhead
- Validation adds minimal overhead in normal operation
- Can be disabled for performance-critical applications
- Strict mode has higher overhead but better error detection

### Optimization Tips
```elisp
;; For production systems with trusted data
(setq kb-validation-enabled nil)

;; For development and testing
(setq kb-validation-enabled t)
(setq kb-validation-strict-mode t)
```

## Examples

### Basic Validation Usage

```elisp
;; Initialize system with validation
(require 'kb-advanced-system)
(kb-init)

;; Enable strict mode for development
(kb-validation-toggle-strict-mode)

;; Valid operations
(kb-assert 'John 'age 25)
(kb-create-microtheory 'PersonMt 'BaseMt)
(kb-query 'John 'age)

;; Invalid operations (will show helpful errors)
(kb-assert nil 'age 25)              ; ✗ Subject cannot be nil
(kb-create-microtheory 'PersonMt)    ; ✗ Already exists
(kb-query 'John 'age 'BadMt)         ; ✗ Microtheory not found
```

### Error Recovery Example

```elisp
;; Demonstrate error recovery during file operations
(condition-case err
    (kb-persist-save "/invalid/path/kb.dat")
  (kb-validation-error
   (message "Save failed: %s" (cadr err))
   ;; Try alternative location
   (kb-persist-save "~/kb-backup.dat")))
```

### Validation Statistics

```elisp
;; Monitor validation activity
(kb-validation-show-stats)
;; Output:
;; KB Validation Statistics:
;;   kb-assert: 15 calls validated
;;   kb-query: 8 calls validated  
;;   kb-create-microtheory: 3 calls validated
```

## Troubleshooting

### Common Validation Errors

1. **"Microtheory does not exist"**
   - Solution: Create the microtheory first with `kb-create-microtheory`
   - Or use an existing microtheory name

2. **"Subject cannot be nil"**  
   - Solution: Provide a valid subject (symbol, string, or number)

3. **"Rule premises cannot be nil"**
   - Solution: Provide at least one premise in the premises list

4. **"Invalid file syntax"**
   - Solution: Check file format, may be corrupted
   - Try loading from backup

### Debugging Validation Issues

```elisp
;; Enable detailed error reporting
(setq kb-validation-strict-mode t)

;; Check system state
(kb-validate-system)

;; Test individual components
(kb-check-fact 'test 'predicate 'value)
(kb-check-microtheory 'test-mt)
```

## Migration from Previous Versions

### Updating Existing Code

Old code without validation:
```elisp
(kb-assert subject predicate object)
```

New code with error handling:
```elisp
(condition-case err
    (kb-assert subject predicate object)
  (kb-validation-error
   (message "Validation failed: %s" (cadr err))))
```

### Compatibility

The validation system is backward compatible. Existing code will continue to work, but will now benefit from error checking and graceful failure handling.

## Summary

The KB validation system provides:
- ✅ Comprehensive input validation
- ✅ Graceful error recovery  
- ✅ Helpful error messages with suggestions
- ✅ User-friendly validation functions
- ✅ Performance controls
- ✅ Backward compatibility
- ✅ Extensive error handling across all modules

This ensures your knowledge base operations are robust, reliable, and user-friendly.
