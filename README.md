# Knowledge Base System in Emacs

## Overview

This project implements a simple knowledge base system in Emacs Lisp, inspired by the CYC project for storing common sense knowledge. The system allows users to define facts, query the knowledge base, and perform inference using a domain-specific language (DSL).

## Features

- **Knowledge Representation**: Store facts and rules using a structured ontology.
- **Inference Engine**: Apply rules to derive new facts based on existing knowledge.
- **Query Interface**: Use a DSL to run complex queries against the knowledge base.
- **Extensibility**: Easily add new classes, facts, and inference rules.

## Installation

1. **Open Emacs**.
2. **Load the File**:
   - Use `C-x C-f` to open the file containing the code.
   - Navigate to and select the `.el` file.
3. **Evaluate the Code**:
   - Place the cursor at the end of each s-expression and use `C-x C-e` to evaluate it.
   - Alternatively, evaluate the entire buffer with `M-x eval-buffer`.

## Usage

### Adding Facts

You can add facts to the knowledge base using:

```elisp
(kb-system-add-fact 'Socrates 'is-a 'human)
(kb-system-add-fact 'human 'is-mortal t)
```

### Querying the Knowledge Base

To query facts, use:

```elisp
(kb-system-query 'Socrates 'is-a)
```

### Performing Inference

You can infer new facts based on existing ones:

```elisp
(kb-system-infer)
```

### Using the Query DSL

The `kb-system-with-query` macro allows for more complex queries:

1. **Basic Query**:
   ```elisp
   (kb-system-with-query
    (select Socrates is-a))
   ```

2. **Query with Condition**:
   ```elisp
   (kb-system-with-query
    (select human is-mortal)
    (where (> (kb-fact-certainty fact) 0.8)))
   ```

3. **Query with Ordering**:
   ```elisp
   (kb-system-with-query
    (select animal is-a)
    (order-by kb-fact-certainty))
   ```

4. **More Complex Query**:
   ```elisp
   (kb-system-with-query
    (select ?x is-a)
    (where (and (kb-system-is-a (kb-fact-object fact) 'mammal)
                (> (kb-fact-certainty fact) 0.5)))
    (order-by kb-fact-certainty))
   ```

### Inspecting Results

You can print out each fact returned by a query:

```elisp
(let ((results (kb-system-with-query
                (select Socrates is-mortal))))
  (dolist (fact results)
    (message "Fact: %s is %s %s (Certainty: %f)"
             (kb-fact-subject fact)
             (kb-fact-predicate fact)
             (kb-fact-object fact)
             (kb-fact-certainty fact))))
```

### Extending the System

To enhance your knowledge base:

- **Add New Classes**:
  ```elisp
  (kb-system-add-class 'philosopher 'human)
  ```

- **Define New Inference Rules**:
  ```elisp
  (kb-system-add-rule 'mortality-rule
                      '(((?x is-a ?y) (?y is-mortal t)))
                      '(?x is-mortal t))
  ```

### Saving Your Work

Use `C-x C-s` to save your changes to the file.

## Conclusion

This Emacs Lisp knowledge base system provides a flexible framework for representing and reasoning about common sense knowledge. By leveraging Emacs' powerful editing capabilities, users can easily extend and interact with their knowledge base.

For further details on Emacs usage and Elisp interaction, refer to the [Emacs documentation](https://www.gnu.org/software/emacs/manual/).

---

*Current date: Saturday, January 11, 2025*
