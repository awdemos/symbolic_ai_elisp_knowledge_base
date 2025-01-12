# Advanced Knowledge Base System v2.1

## Overview

This is a significantly enhanced version of the Emacs Lisp knowledge base system, now incorporating advanced architectural principles for symbolic AI. The system provides a powerful platform for symbolic AI applications with support for contextual reasoning, sophisticated inference, temporal logic, and event processing.

## Key Features

### 🧠 **Microtheory System**
- **Context Isolation**: Facts and rules are scoped to specific microtheories (contexts)
- **Hierarchical Inheritance**: Microtheories can inherit knowledge from parent contexts  
- **Temporary Contexts**: Create isolated reasoning environments for queries
- **Built-in Microtheories**: CommonSenseMt, TemporalMt, EventMt, and more

### 🔍 **Layered Inference Engine**
- **Specialized Workers**: Fast direct lookup, taxonomic reasoning, temporal inference
- **Strategic Coordination**: Intelligent routing of queries to appropriate inference engines
- **Fallback Architecture**: Efficient → general reasoning with graceful degradation
- **Blackboard Coordination**: Workers share intermediate results for complex reasoning

### 🤔 **Non-Monotonic Reasoning**
- **Default Logic**: Rules with exceptions that can be overridden
- **Belief Revision**: Automatic handling of contradictory information
- **Truth Maintenance**: Track justifications and dependencies between facts
- **Exception Handling**: Define and manage exceptions to default rules

### ⏰ **Temporal Reasoning**
- **Time-bounded Facts**: Assert facts valid during specific time periods
- **Event Temporal Relations**: Before, after, during relationships
- **Temporal Queries**: Query facts at specific times
- **Temporal Inference**: Derive temporal relationships automatically

### 🎭 **Event Reification**
- **First-class Events**: Events are objects that can be reasoned about
- **Process Types**: Define templates for recurring event patterns
- **Event Relations**: Causal, temporal, and compositional relationships
- **Event Queries**: Find events by type, participants, time, or properties

## New in v2.1

### 🐛 **Debugging & Introspection**
- **Inference Tracing**: Step-by-step debugging of reasoning processes
- **Session Management**: Track and replay debugging sessions
- **Validation Tools**: Detect inconsistencies and logical errors
- **Visualization**: Text-based visualization of microtheory contents

### ⚡ **Query Caching**
- **Intelligent Caching**: Optional performance optimization for frequent queries
- **LRU Eviction**: Automatic memory management with least-recently-used cleanup
- **Cache Invalidation**: Smart invalidation when knowledge changes
- **Statistics Tracking**: Monitor cache hit rates and performance

### 🧪 **Testing Framework**
- **Structured Testing**: Define test suites and cases for knowledge base validation
- **Assertion Library**: Rich set of KB-specific assertions and checks
- **Automated Testing**: Run comprehensive test suites with detailed reporting
- **Test Isolation**: Temporary microtheories for isolated testing

### 🌐 **RDF/OWL Integration**
- **Semantic Web Import**: Load RDF/XML and Turtle format data
- **Namespace Management**: Handle RDF namespaces and URI resolution
- **Export Capabilities**: Export microtheories to standard RDF formats
- **Type Conversion**: Smart conversion between RDF and KB data types

### 💾 **Database Persistence**
- **SQLite Integration**: Persistent storage for knowledge bases
- **Schema Management**: Automatic database schema creation and versioning
- **Import/Export**: CSV and database import/export capabilities
- **Query Interface**: Direct database queries for analysis and reporting

## Installation

### Using Emacs

1. Load the main system:
```elisp
(require 'kb-advanced-system)
```

2. Initialize the knowledge base:
```elisp
(kb-init)
```

3. Try the demo:
```elisp
(kb-demo)
```

### Using Docker with Dagger

You can build and run the knowledge base system using a complete CI/CD pipeline powered by Dagger:

#### Available Commands

**Full Pipeline:**
```bash
# Run the complete CI/CD pipeline: lint → test → build → tag locally
dagger call pipeline

# Run pipeline with custom image tag
dagger call pipeline --tag="kb-advanced-system:v2.1"
```

**Individual Steps:**
```bash
# Lint: Check Emacs Lisp syntax and style
dagger call lint

# Test: Run automated test suite
dagger call test

# Build: Create Docker image
dagger call build
```

**Run the Container:**
```bash
# After pipeline completes, run the built image
docker run -it symbolic-ai-kb:latest

# Or run with custom tag
docker run -it kb-advanced-system:v2.1
```

#### Pipeline Features

The Dagger CI/CD pipeline provides:

- **🔍 Linting**: Emacs Lisp syntax validation and style checking
- **🧪 Testing**: Automated execution of the knowledge base test suite
- **🏗️ Building**: Docker image creation from the project Dockerfile
- **🏷️ Tagging**: Local Docker registry tagging (equivalent to `docker build -t tag .`)

This replaces traditional `make` and `docker build` workflows with a modern, containerized CI/CD approach that produces the same result as running `docker build -t kb-advanced-system:v2.1 .` in your project directory.

**Prerequisites**: 
- [Dagger](https://dagger.io/) installed
- Docker running locally

## Quick Start

### Basic Usage

```elisp
;; Assert basic facts
(kb-assert 'Socrates 'is-a 'human)
(kb-assert 'human 'is-a 'mammal)

;; Query with inference
(kb-ask '(Socrates is-a))  ; Returns: human, mammal, animal

;; Use microtheories for context
(in-microtheory ChristianMt
  (kb-assert 'God 'exists t))

(in-microtheory AtheistMt  
  (kb-assert 'God 'exists nil))

;; Add default rules with exceptions
(kb-add-default 'birds-fly 
                '((?x is-a bird)) 
                '(?x can-fly t))

(kb-add-exception 'penguin-exception 
                  'birds-fly
                  '((?x is-a penguin))
                  '(?x can-fly nil))

;; Create and relate events
(let ((walk1 (kb-create-event 'walking :participants '(John)))
      (walk2 (kb-create-event 'walking :participants '(Mary))))
  (kb-relate-events :before walk1 walk2))

;; Perform comprehensive reasoning
(kb-reason)
```

### New v2.1 Features

```elisp
;; Enable debugging and caching
(kb-debug-on)
(kb-cache-on 1000 300)  ; max 1000 entries, 300s TTL

;; Debug a query step-by-step  
(kb-trace-query 'Socrates 'is-mortal)

;; Validate microtheory consistency
(kb-validate "CommonSenseMt")

;; Connect to database for persistence
(kb-db-connect "~/kb-data.sqlite")
(kb-save-to-db "CommonSenseMt")

;; Import semantic web data
(kb-import-rdf "~/ontology.owl" :rdf-xml 'OntologyMt)

;; Run automated tests
(kb-run-tests)

;; Check system status
(kb-status)  ; Shows cache, debug, DB status
```

## Architecture

The system follows a modular architecture:

```
kb-advanced-system.el  # Unified API and high-level interface
├── kb-microtheories.el    # Context management and inheritance
├── kb-inference-engine.el # Layered inference with specialized workers  
├── kb-nonmonotonic.el     # Default logic and belief revision
├── kb-events.el           # Event reification and temporal reasoning
├── kb-debugger.el         # Debugging and introspection tools
├── kb-cache.el           # Query caching and performance optimization
├── kb-testing.el         # Testing framework and validation
├── kb-rdf.el             # RDF/OWL import and export
├── kb-database.el        # Database integration and persistence
└── kb-system.el          # Legacy compatibility layer
```

## API Reference

### Core Functions

- `(kb-assert subject predicate object &optional certainty temporal-info mt)`
- `(kb-query subject predicate &optional mt)`
- `(kb-ask query &optional mt timeout)`
- `(kb-reason &optional mt)`

### Microtheory Management

- `(in-microtheory mt &rest body)`
- `(kb-create-microtheory name &optional parents)`
- `(kb-switch-mt mt-name)`

### Rule Management

- `(kb-add-rule name premises conclusion)`
- `(kb-add-default name premises conclusion &optional exceptions)`
- `(kb-add-exception name rule-name conditions)`

### Event Management

- `(kb-create-event type &rest properties)`
- `(kb-define-process name &rest properties)`
- `(kb-relate-events relation source target)`

### Debugging & Validation (v2.1)

- `(kb-debug-on)` / `(kb-debug-off)`
- `(kb-trace-query subject predicate &optional mt)`
- `(kb-validate mt-name)`
- `(kb-visualize-microtheory mt-name)`

### Caching (v2.1)

- `(kb-cache-on &optional max-size ttl)` / `(kb-cache-off)`
- `(kb-cached-query subject predicate &optional mt)`
- `(kb-cache-show-stats)`

### Testing (v2.1)

- `(kb-deftest name &rest body)`
- `(kb-deftest-suite name &rest body)`
- `(kb-run-tests &optional suite-name)`
- `(kb-assert-fact-exists subject predicate object)`

### Database Integration (v2.1)

- `(kb-db-connect db-path)`
- `(kb-save-to-db mt-name)` / `(kb-load-from-db mt-name)`
- `(kb-db-import-csv file-path &optional mt-name)`
- `(kb-db-export-csv mt-name file-path)`

### RDF/OWL Integration (v2.1)

- `(kb-import-rdf file-path &optional format mt-name)`
- `(kb-export-rdf mt-name file-path &optional format)`
- `(kb-rdf-import-url url &optional format mt-name)`

### Enhanced Query Language

```elisp
(kb-with-query
  (select ?x is-a)
  (where (kb-system-is-a (kb-fact-object fact) 'mammal))
  (in-mt CommonSenseMt)
  (order-by kb-fact-certainty)
  (limit 10))
```

## Interactive Commands

### Core Commands
- `M-x kb-demo` - Run comprehensive demonstration
- `M-x kb-interactive-query` - Interactive query interface
- `M-x kb-interactive-assert` - Interactive fact entry
- `M-x kb-status` - Display system status
- `M-x kb-list-microtheories` - List all contexts

### v2.1 Commands
- `M-x kb-debug-on` / `M-x kb-debug-off` - Toggle debugging
- `M-x kb-cache-on` / `M-x kb-cache-off` - Toggle caching
- `M-x kb-validate` - Validate microtheory consistency
- `M-x kb-run-tests` - Run automated tests
- `M-x kb-db-setup` - Interactive database setup
- `M-x kb-import-rdf` - Import RDF/OWL data
- `M-x kb-cache-show-stats` - Display cache statistics

## Examples

### Common Sense Reasoning

```elisp
(in-microtheory CommonSenseMt
  ;; Birds typically fly
  (kb-add-default 'birds-fly '((?x is-a bird)) '(?x can-fly t))
  
  ;; But penguins don't
  (kb-add-exception 'penguin-exception 'birds-fly 
                    '((?x is-a penguin)) '(?x can-fly nil))
  
  ;; Test the reasoning
  (kb-assert 'Tweety 'is-a 'bird)
  (kb-assert 'Pingu 'is-a 'penguin) 
  (kb-reason)
  
  (kb-query 'Tweety 'can-fly)  ; => t
  (kb-query 'Pingu 'can-fly)   ; => nil
)
```

### Temporal Reasoning

```elisp
(kb-assert-temporal 'John 'location 'office 
                    "2025-01-01 09:00" "2025-01-01 17:00")

(kb-query-at-time 'John 'location "2025-01-01 12:00")  ; => office
(kb-query-at-time 'John 'location "2025-01-01 20:00")  ; => no result
```

### Event Processing

```elisp
;; Define a meeting process
(kb-define-process 'meeting
  :typical-duration 3600
  :preconditions '((location available t))
  :effects '((participants informed t)))

;; Create a specific meeting
(let ((meeting1 (kb-generate-event-instance 'meeting 
                   '(Alice Bob Charlie)
                   :location 'conference-room-a
                   :start-time (current-time))))
  
  ;; Find all meetings
  (kb-find-events-by-type 'meeting))
```

### Testing Example (v2.1)

```elisp
(kb-deftest-suite reasoning-tests
  "Test basic reasoning capabilities"
  
  (kb-deftest transitivity
    "Test transitive is-a relationships"
    (kb-assert 'Socrates 'is-a 'human)
    (kb-assert 'human 'is-a 'mammal)
    (kb-assert 'mammal 'is-a 'animal)
    (kb-reason)
    (kb-assert-fact-exists 'Socrates 'is-a 'animal))
  
  (kb-deftest consistency
    "Test microtheory consistency"
    (kb-assert 'Tweety 'is-a 'bird)
    (kb-assert 'Tweety 'can-fly t)
    (kb-assert-microtheory-consistent kb-current-mt)))

;; Run the tests
(kb-run-test-suite "reasoning-tests")
```

### Database Integration Example (v2.1)

```elisp
;; Connect to database and set up persistence
(kb-db-connect "~/my-kb.sqlite")

;; Save current knowledge to database
(kb-save-to-db "CommonSenseMt")

;; Import external data
(kb-db-import-csv "~/facts.csv" 'ImportedDataMt)

;; Query database directly
(kb-db-search-facts "animal")

;; Export for analysis
(kb-db-export-csv "CommonSenseMt" "~/exported-facts.csv")
```

## Migration from v1.0

The system maintains backward compatibility through `kb-system.el`. However, for new development, use the enhanced API:

**Old v1.0 way:**
```elisp
(kb-system-add-fact 'Socrates 'is-a 'human)
(kb-system-query 'Socrates 'is-a)
```

**New v2.0 way:**
```elisp
(kb-assert 'Socrates 'is-a 'human)  
(kb-query 'Socrates 'is-a)
```

## Performance Characteristics

- **Fast Lookups**: Direct fact retrieval in O(1) time
- **Efficient Inference**: Specialized workers handle common patterns quickly
- **Scalable Architecture**: Microtheories prevent knowledge base bloat
- **Lazy Evaluation**: Inference triggered only when needed

## Future Extensions

The modular architecture supports easy extension:

- Additional inference workers for specialized domains
- More sophisticated temporal logic
- Probabilistic reasoning integration
- Natural language interfaces
- Distributed knowledge bases

## References

- [Advanced Knowledge Representation](https://en.wikipedia.org/wiki/Knowledge_representation_and_reasoning)
- [Microtheory Systems](https://en.wikipedia.org/wiki/Context_(computing))
- [Non-monotonic Reasoning](https://en.wikipedia.org/wiki/Non-monotonic_reasoning)

---

*For questions or contributions, see the project repository.*
