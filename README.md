# Advanced Knowledge Base System v2.1

Symbolic AI knowledge base system in Emacs Lisp with microtheories, non-monotonic reasoning, temporal logic, and LLM-powered data generation.

> Warning: some of the functionality in this repo is "half baked," not working as well as it should, or flat out doesn't work yet. PRs welcome!

## Key Features

- **Microtheory System**: Context-scoped facts with hierarchical inheritance
- **Layered Inference**: Specialized workers for fast reasoning with fallback architecture  
- **Non-Monotonic Logic**: Default rules with exceptions and belief revision
- **Temporal Reasoning**: Time-bounded facts and event relationships
- **Event Reification**: First-class events with causal and temporal relations
- **Debugging Tools**: Inference tracing, validation, and visualization
- **Query Caching**: LRU-based performance optimization
- **Testing Framework**: Automated KB validation and test suites
- **RDF/OWL Integration**: Semantic web import/export

- **LLM Data Generation**: Structured fact extraction using Outlines library

## Installation

### Using Emacs

```elisp
(add-to-list 'load-path "./lisp")
(require 'kb-advanced-system)
(kb-init)
(kb-demo)  ; Run demonstration
```

### LLM Data Generation

Generate structured KB facts from text using Outlines:

```bash
# Install dependencies
pip install -r scripts/requirements.txt

# Extract facts from text
python scripts/kb_openai_generator.py --file input.txt --output facts.el --microtheory MyMt

# Load generated facts in Emacs
(load-file "facts.el")
```

## Quick Start

```elisp
;; Basic facts and queries
(kb-assert 'Socrates 'is-a 'human)
(kb-ask '(Socrates is-a))  ; Returns inherited types

;; Microtheories for context isolation
(in-microtheory MyMt
  (kb-assert 'God 'exists t))

;; Default rules with exceptions
(kb-add-default 'birds-fly '((?x is-a bird)) '(?x can-fly t))
(kb-add-exception 'penguin-exception 'birds-fly '((?x is-a penguin)) '(?x can-fly nil))

;; Events and temporal reasoning
(kb-create-event 'meeting :participants '(Alice Bob))
(kb-assert-temporal 'John 'location 'office "2025-01-01" "2025-01-02")

;; Advanced features
(kb-debug-on)
(kb-cache-on)

(kb-import-rdf "ontology.owl")
(kb-reason)
```

### LLM-Generated Data

```bash
# Extract facts from text using Outlines
cd scripts/
python kb_openai_generator.py --file ../test/document.txt --microtheory ScientistMt
# Outputs: openai_facts.el

# Load in Emacs
(load-file "openai_facts.el")
```

## Project Structure

```
symbolic_ai_elisp_knowledge_base/
├── README.md                    # Main documentation
├── Makefile                     # Build automation
├── lisp/                       # All Elisp source files
│   ├── kb-advanced-system.el   # Main API
│   ├── kb-microtheories.el     # Context management
│   └── ...                     # Other modules
├── scripts/                    # Python data generators  
│   ├── kb_openai_generator.py  # OpenAI + Outlines
│   ├── kb_data_generator.py    # Local LLM version
│   └── requirements.txt        # Python dependencies
├── test/                       # Test files and samples
├── docs/                       # Additional documentation
└── examples/                   # Usage examples
```

## Build and Usage

```bash
# Compile all elisp files
make compile

# Run demo from command line
emacs -Q -batch -L lisp -l lisp/kb-advanced-system.el --eval "(kb-demo)"

# Run tests 
make test

# Try basic example
make example-basic

# Clean compiled files
make clean
```

## API Reference

**Core Functions:**
- `(kb-assert subject predicate object)`
- `(kb-query subject predicate)`
- `(kb-ask query)`
- `(kb-reason)`

**Microtheories:**
- `(in-microtheory mt &rest body)`
- `(kb-create-microtheory name parents)`

**Rules & Defaults:**
- `(kb-add-default name premises conclusion)`
- `(kb-add-exception name rule conditions)`

**Events & Temporal:**
- `(kb-create-event type :participants list)`
- `(kb-assert-temporal subject predicate object from to)`

**Tools:**
- `(kb-debug-on)` / `(kb-cache-on)`
- `(kb-import-rdf file)`
- `(kb-run-tests)` / `(kb-status)`

## Interactive Commands

- `M-x kb-demo` - Run demonstration
- `M-x kb-interactive-query` - Interactive queries  
- `M-x kb-status` - Display system status
- `M-x kb-debug-on` - Enable debugging
- `M-x kb-run-tests` - Run test suite

## Example Usage

```elisp
;; Common sense reasoning with defaults
(kb-add-default 'birds-fly '((?x is-a bird)) '(?x can-fly t))
(kb-add-exception 'penguin-exception 'birds-fly '((?x is-a penguin)) '(?x can-fly nil))
(kb-assert 'Tweety 'is-a 'bird)
(kb-reason)
(kb-query 'Tweety 'can-fly)  ; => t

;; Temporal facts  
(kb-assert-temporal 'John 'location 'office "2025-01-01" "2025-01-02")


```

---

*A modular symbolic AI platform supporting contextual reasoning, non-monotonic logic, and LLM-powered data generation.*
