# Outlines Integration for KB Data Generation

This directory contains scripts to generate structured knowledge base facts using the [Outlines](https://github.com/dottxt-ai/outlines) library, compatible with the Emacs Lisp knowledge base system.

## Overview

The integration provides three approaches:

1. **`kb_openai_generator.py`** - OpenAI API with Outlines for high-quality structured extraction
2. **`kb_data_generator.py`** - Local LLM integration with Outlines (Hugging Face models)
3. **`kb_data_generator_simple.py`** - Simple pattern-based extraction (no dependencies)

## Installation

```bash
# For OpenAI + Outlines integration
pip install outlines[openai] pydantic openai

# For local models + Outlines  
pip install -r requirements.txt

# For simple version (no dependencies)
python kb_data_generator_simple.py --help
```

## Usage

### OpenAI Version (Recommended)

```bash
# Set API key
export OPENAI_API_KEY="your-key-here"

# Extract from file using OpenAI
python kb_openai_generator.py --file input.txt --output facts.el --microtheory ScientistMt

# Extract from text with context
python kb_openai_generator.py --text "Einstein was a physicist" --context "Scientific biography" --output facts.el

# Use different OpenAI model
python kb_openai_generator.py --file input.txt --model gpt-4o --microtheory AdvancedMt
```

### Local Models Version

```bash
# Extract using local Hugging Face models
python kb_data_generator.py --file input.txt --output facts.el --microtheory ScientistMt

# Use different local model
python kb_data_generator.py --file input.txt --model "microsoft/Phi-3-mini-4k-instruct"
```

### Simple Version

```bash
# Extract using pattern matching
python kb_data_generator_simple.py --file test_sample.txt --output facts.el --microtheory MyMt
```

## Generated Format

The scripts generate Elisp code compatible with the KB system:

```elisp
(require 'kb-advanced-system)

;; Initialize if needed
(unless (boundp 'kb-current-mt)
  (kb-init))

;; Create microtheory
(unless (kb-get-microtheory 'ScientistMt)
  (kb-create-microtheory 'ScientistMt 'CommonSenseMt))

(in-microtheory ScientistMt
  ;; Facts in subject-predicate-object format
  (kb-assert 'Einstein 'is-a 'physicist 0.9 nil 'ScientistMt)
  (kb-assert 'Einstein 'worked-at 'Princeton_University 0.85 nil 'ScientistMt)
  (kb-assert 'Einstein 'developed 'theory_of_relativity 0.8 nil 'ScientistMt)
)

;; Trigger reasoning
(kb-reason)
```

## Data Structures

### KnowledgeFact (Outlines version)
- `subject`: String entity name
- `predicate`: String relationship
- `object`: String/number/boolean value
- `certainty`: Float confidence (0.0-1.0)
- `microtheory`: Optional context

### TemporalFact (Outlines version)
- Includes `valid_from` and `valid_to` timestamps
- Maps to `kb-assert-temporal`

### Event (Outlines version)
- `event_type`: Type of event
- `participants`: List of entities
- `location`: Optional location
- `time`: Optional timestamp
- Maps to `kb-create-event`

## Integration with KB System

The generated files can be loaded directly in Emacs:

```elisp
;; Load generated facts
(load-file "test_generated_facts.el")

;; Query the new facts
(kb-query 'Einstein 'is-a)
(kb-query 'Einstein 'worked-at)

;; Switch to the new microtheory
(kb-switch-mt 'ScientistMt)

;; Verify facts were added
(kb-status)
```

## Example Workflow

1. **Prepare text data** - Text file or string with factual information
2. **Run extraction** - Use either script to generate structured facts
3. **Load in Emacs** - Load the generated `.el` file in your Emacs session
4. **Query and reason** - Use the KB system to query and infer new facts

## Extending the System

To add new fact types or extraction patterns:

1. **Outlines version**: Modify the Pydantic models and prompts
2. **Simple version**: Add new regex patterns to `extract_facts_simple()`
3. **Both**: Update the formatting functions for new Elisp constructs

The system is designed to be extensible for domain-specific knowledge extraction while maintaining compatibility with the existing KB architecture.
