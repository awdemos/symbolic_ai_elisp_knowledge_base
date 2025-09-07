# KB Persistence System Documentation

The KB persistence system provides comprehensive save/load functionality for the symbolic AI knowledge base, supporting full state preservation including microtheories, facts, rules, events, TMS justifications, and system configuration.

## Features

- **Complete State Preservation**: Saves all KB components including microtheories, facts, rules, events, and TMS justifications
- **Circular Reference Handling**: Uses `print-circle` to properly handle circular references in data structures
- **Incremental Save/Load**: Support for saving only changed data since last full save
- **Data Validation**: Validates loaded data for consistency and integrity
- **Backup Management**: Automatic backup creation with configurable retention
- **Auto-save Capability**: Periodic automatic saving with configurable intervals
- **S-expression Format**: Human-readable format compatible with Emacs Lisp

## Basic Usage

### Save the Knowledge Base

```elisp
;; Save entire KB to a file
(kb-save "my-knowledge-base.el")

;; Save only changes since last full save
(kb-incremental-save "kb-incremental.el")

;; Create a timestamped backup
(kb-backup)
```

### Load a Knowledge Base

```elisp
;; Load KB from file (replaces current state)
(kb-load "my-knowledge-base.el")

;; Load and merge with existing data
(kb-load "my-knowledge-base.el" t)
```

### Interactive Commands

```elisp
;; Interactive save
M-x kb-save-to-file

;; Interactive load
M-x kb-load-from-file

;; Create backup
M-x kb-backup
```

## Advanced Features

### Auto-save

Enable automatic saving every 30 seconds:

```elisp
(kb-auto-save-enable 30 "auto-save.el")

;; Disable auto-save
(kb-auto-save-disable)
```

### Validation

Validate KB consistency after loading:

```elisp
(kb-validate-kb)
```

### Status Information

Check persistence system status:

```elisp
(kb-persistence-status)
```

## File Format

The persistence system uses a structured s-expression format:

```elisp
(:version "1.0"
 :timestamp (25953 51234)
 :incremental-p nil
 :microtheories (...)
 :events (...)
 :processes (...)
 :default-rules (...)
 :exceptions (...)
 :justifications (...)
 :tms-facts (...)
 :tms-justifications (...)
 ...)
```

### Serialized Components

- **Microtheories**: Complete microtheory structures with facts and rules
- **Facts**: Subject-predicate-object triples with certainty and temporal info
- **Rules**: Inference rules with premises and conclusions
- **Events**: Reified events with participants, roles, and temporal information
- **TMS Data**: Truth maintenance system facts and justifications
- **System State**: Configuration settings and counters

## Configuration

### Backup Settings

```elisp
;; Number of backup files to keep
(setq kb-persistence-backup-count 10)

;; Enable automatic backups on save
(setq kb-persistence-auto-backup t)
```

### Validation Settings

```elisp
;; Validate data on load
(setq kb-persistence-validate-on-load t)
```

## Integration with TMS

The persistence system fully integrates with the Truth Maintenance System:

- **Justifications**: All fact justifications are preserved
- **Dependencies**: Fact dependency relationships are maintained
- **Belief Status**: IN/OUT status of facts is preserved
- **Circular References**: Complex justification networks are handled correctly

## Error Handling

The system provides comprehensive error handling:

- **File Format Validation**: Checks file version and structure
- **Data Consistency**: Validates references between components
- **Graceful Fallbacks**: Continues operation even with missing optional data

## Performance Considerations

- **Large KBs**: The system handles large knowledge bases efficiently
- **Memory Usage**: Uses streaming for large data structures
- **Incremental Saves**: Reduces I/O by saving only changed data
- **Compression**: Optional file compression (configurable)

## Examples

### Complete Workflow

```elisp
;; Initialize KB
(kb-init)

;; Add some knowledge
(kb-assert 'Socrates 'is-a 'human)
(kb-add-rule 'mortality '((human ?x)) '(mortal ?x))

;; Save the KB
(kb-save "philosophy.el")

;; Later, in a new session
(kb-load "philosophy.el")

;; Verify the data is there
(kb-query 'Socrates 'is-a) ; => ((human))
```

### Incremental Workflow

```elisp
;; Full save
(kb-save "base.el")

;; Add new facts
(kb-assert 'Plato 'is-a 'human)
(kb-assert 'Plato 'student-of 'Socrates)

;; Incremental save
(kb-incremental-save "incremental.el")
```

### Validation Example

```elisp
;; Load data
(kb-load "knowledge-base.el")

;; Validate consistency
(if (kb-validate-consistency)
    (message "KB is consistent")
  (message "KB has consistency issues"))
```

## Best Practices

1. **Regular Backups**: Enable auto-backup for important knowledge bases
2. **Incremental Saves**: Use incremental saves for frequent updates
3. **Validation**: Always validate after loading from untrusted sources
4. **Version Control**: Consider versioning your KB files
5. **Testing**: Test save/load cycles with your specific data patterns

## Troubleshooting

### Common Issues

1. **File Not Found**: Ensure file paths are absolute and accessible
2. **Version Mismatch**: Check file version compatibility
3. **Memory Issues**: Use incremental saves for very large KBs
4. **Circular References**: The system handles these automatically

### Debug Information

Enable debug mode for detailed persistence information:

```elisp
(setq debug-on-error t)
(kb-persistence-status)
```

This provides detailed information about the persistence system state and any issues encountered.
