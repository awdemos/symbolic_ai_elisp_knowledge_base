## How to Use the kb-microtheories.el Script

This guide will walk you through using the kb-microtheories.el script, which provides a knowledge base system with support for microtheories in Emacs Lisp.

### Setting Up

1. Ensure you have Emacs 25.1 or later installed.
2. Place the kb-microtheories.el file in your Emacs load path.
3. Load the script in your Emacs session:
   ```elisp
   (require 'kb-microtheories)
   ```

### Creating Microtheories

1. Add a new microtheory:
   ```elisp
   (kb-microtheories-add-microtheory 'my-microtheory)
   ```

2. Create a hierarchy of microtheories:
   ```elisp
   (kb-microtheories-add-microtheory 'parent-theory)
   (kb-microtheories-add-submicrotheory 'child-theory 'parent-theory)
   ```

### Adding Knowledge

1. Add assertions to a microtheory:
   ```elisp
   (kb-microtheories-add-assertion 'is-a '(Socrates human) 'my-microtheory)
   ```

2. Add inference rules:
   ```elisp
   (kb-microtheories-add-rule 'mortality-rule
                              '((is-a (?x human)))
                              '(is-a (?x mortal))
                              'my-microtheory)
   ```

### Querying Knowledge

1. Query assertions in a microtheory:
   ```elisp
   (kb-microtheories-query 'is-a 'my-microtheory)
   ```

2. Use the query DSL for more complex queries:
   ```elisp
   (kb-microtheories-with-query my-microtheory
     (select is-a)
     (where (equal (car (kb-assertion-arguments assertion)) 'Socrates)))
   ```

### Inference

Apply inference rules to derive new knowledge:
```elisp
(kb-microtheories-infer 'my-microtheory)
```

### Managing the Knowledge Base

1. Print the entire knowledge base:
   ```elisp
   (kb-microtheories-print-kb)
   ```

2. Save the knowledge base to a file:
   ```elisp
   (kb-microtheories-save-kb "my-kb.el")
   ```

3. Load a knowledge base from a file:
   ```elisp
   (kb-microtheories-load-kb "my-kb.el")
   ```

By following these steps, you can create, manage, and query a sophisticated knowledge base system using microtheories in Emacs Lisp.
