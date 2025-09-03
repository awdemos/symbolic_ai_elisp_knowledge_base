# Using the KB System with Vim-style Emacs

This guide shows how to use the Knowledge Base system with vim-style editing in Emacs.

## Why Emacs (Not Pure Vim)?

The KB system is written in **Emacs Lisp** and requires an Elisp interpreter to run. Pure Vim/Neovim cannot execute Elisp code. However, you can get excellent vim keybindings within Emacs using these solutions.

## Option 1: Evil Mode (Recommended)

Evil provides comprehensive vim emulation in Emacs.

### Installation

```elisp
;; Add to your ~/.emacs.d/init.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install and configure Evil
(unless (package-installed-p 'evil)
  (package-refresh-contents)
  (package-install 'evil))

(require 'evil)
(evil-mode 1)

;; Load the KB system
(add-to-list 'load-path "~/code/symbolic_ai_elisp_knowledge_base/lisp")
(require 'kb-advanced-system)

;; Initialize KB
(kb-init)
```

### Usage

```elisp
;; Use vim keybindings for editing, Emacs commands for KB
;; Press 'i' to enter insert mode, ESC for normal mode

;; KB functions work normally:
M-x kb-demo
M-x kb-interactive-query
M-x kb-status

;; Or use in elisp evaluation:
C-x C-e  ; Evaluate expression (after cursor)
(kb-assert 'Socrates 'is-a 'human)
```

## Option 2: Doom Emacs (Pre-configured)

Doom Emacs comes with Evil mode and optimized configurations.

### Installation

```bash
# Install Doom Emacs
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
~/.config/emacs/bin/doom install
```

### Configuration

Add to `~/.config/doom/config.el`:

```elisp
;; Add KB system to load path
(add-to-list 'load-path "~/code/symbolic_ai_elisp_knowledge_base/lisp")

(after! evil    ;; if you depend on Evil mode
  (require 'kb-advanced-system)
  (kb-init)

  ;; Optional: Add keybindings
  (map! :leader
        (:prefix-map ("k" . "knowledge-base")
         :desc "KB Demo" "d" #'kb-demo
         :desc "KB Query" "q" #'kb-interactive-query
         :desc "KB Assert" "a" #'kb-interactive-assert
         :desc "KB Status" "s" #'kb-status)))

```

### Usage

```
SPC k d  ; Run KB demo
SPC k q  ; Interactive query
SPC k s  ; Show status
```


## Basic Workflow

Regardless of which option you choose, the workflow is:

1. **Start Emacs** with your vim configuration
2. **Load KB system** (automatically if configured)
3. **Use vim keybindings** for text editing
4. **Use Emacs commands** for KB operations:
   - `M-x kb-demo` - Run demonstration
   - `M-x kb-interactive-query` - Query the KB
   - `M-x kb-status` - Show system status

## Evaluating Elisp Code

To run Elisp expressions with vim keybindings:

### In Evil Mode:
```
i                           ; Enter insert mode
(kb-assert 'test 'is-a 'example)  ; Type expression
ESC                         ; Back to normal mode  
C-x C-e                     ; Evaluate expression
```

### In Doom/Spacemacs:
```
i                           ; Insert mode
(kb-query 'test 'is-a)      ; Type expression
ESC                         ; Normal mode
SPC : eval-last-sexp        ; Evaluate (Doom)
SPC e e                     ; Evaluate (Spacemacs)
```

## Loading Generated Facts

When using the Python generators:

```bash
# Generate facts
cd scripts/
export OPENAI_API_KEY="your-key"
python kb_openai_generator.py --file ../test/test_openai_sample.txt

# In Emacs
M-x load-file RET openai_facts.el RET
```

## Troubleshooting

### "Symbol's function definition is void: kb-assert"
```elisp
;; Make sure KB system is loaded:
(require 'kb-advanced-system)
(kb-init)
```

### "Cannot find feature kb-advanced-system"
```elisp
;; Check load path:
(add-to-list 'load-path "/full/path/to/your/kb/lisp")
(require 'kb-advanced-system)
```

### Vim keybindings not working
```elisp
;; For Evil mode:
(require 'evil)
(evil-mode 1)

;; Check if Evil is active:
(evil-mode)  ; Should show current state
```

## Recommended Setup

For the best experience:

1. **Doom Emacs** - Most vim-like, well optimized
2. **Evil mode in vanilla Emacs** - Most control, lightweight  

All three give you excellent vim editing with full Elisp support for the KB system.
