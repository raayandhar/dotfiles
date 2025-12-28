# Org-Roam Hierarchical Workflow Guide

This guide explains how to use org-roam with folder-based organization and transclusion for building comprehensive knowledge bases.

## Philosophy

This setup combines:
- **Hierarchical folders**: Subject → Topic → Notes (for organization)
- **Atomic notes**: Small, focused notes on single concepts
- **Long-running notes**: Aggregate notes that embed atomic notes
- **Transclusion**: Dynamic embedding - edit once, updates everywhere

## Folder Structure

```
~/Github/rivendell/
├── deep-learning/
│   ├── transformers/
│   │   ├── long-running-transformers.org  ← Aggregates everything
│   │   ├── tokenization.org               ← Atomic note
│   │   ├── gqa.org                        ← Atomic note
│   │   ├── attention.org                  ← Atomic note
│   │   └── positional-encoding.org        ← Atomic note
│   └── cnns/
│       ├── long-running-cnns.org
│       └── convolution.org
├── computer-architecture/
│   ├── caches/
│   │   ├── long-running-caches.org
│   │   └── cache-coherence.org
│   └── pipelines/
└── mathematics/
    └── linear-algebra/
```

## Three Types of Notes

### 1. Default Notes
Quick notes without specific organization.

**Create**: `C-c n f` → `d` (default template)

**Location**: Root of rivendell directory

### 2. Atomic Notes
Small, focused notes on single concepts. These are your building blocks.

**Create**: `C-c n f` → `a` (atomic template)

**Prompts**:
- `subject`: e.g., "deep-learning"
- `topic`: e.g., "transformers"
- `title`: e.g., "Tokenization"

**Result**: `deep-learning/transformers/tokenization.org`

**Template structure**:
```org
#+title: Tokenization
#+filetags: :deep-learning:transformers:

* Summary

Brief overview of what tokenization is.

* Details

In-depth explanation, code examples, math, etc.

* Related

- [[roam:Attention Mechanism]]
- [[roam:Embeddings]]
```

### 3. Long-Running Notes
Aggregate notes that embed multiple atomic notes.

**Create**: `C-c n f` → `l` (long-running template)

**Prompts**:
- `subject`: e.g., "deep-learning"
- `topic`: e.g., "transformers"
- `title`: e.g., "Transformers"

**Result**: `deep-learning/transformers/long-running-transformers.org`

**Template structure**:
```org
#+title: Transformers
#+filetags: :deep-learning:transformers:longrunning:

* Overview

Comprehensive guide to transformer architecture.

* Embedded Topics

** Tokenization
#+transclude: [[file:tokenization.org]] :level 3

** Attention Mechanism
#+transclude: [[file:attention.org]] :level 3

** Positional Encoding
#+transclude: [[file:positional-encoding.org]] :level 3
```

## Complete Workflow Example

### Step 1: Create Atomic Notes

Create several focused notes on related topics:

```
C-c n f → a → deep-learning → transformers → Tokenization
C-c n f → a → deep-learning → transformers → Attention
C-c n f → a → deep-learning → transformers → GQA
C-c n f → a → deep-learning → transformers → Position Embeddings
```

Each becomes a standalone note:
```
deep-learning/transformers/tokenization.org
deep-learning/transformers/attention.org
deep-learning/transformers/gqa.org
deep-learning/transformers/position-embeddings.org
```

### Step 2: Fill Out Atomic Notes

Edit each note with focused content:

**tokenization.org**:
```org
#+title: Tokenization
#+filetags: :deep-learning:transformers:

* Summary

Tokenization converts text into numerical tokens for model processing.

* Details

** Byte-Pair Encoding (BPE)
- Merges frequently occurring character pairs
- Used by GPT models

** WordPiece
- Used by BERT
- Similar to BPE but uses likelihood

** SentencePiece
- Language-agnostic
- Used by T5, mT5

** Code Example

#+begin_src python
from transformers import AutoTokenizer

tokenizer = AutoTokenizer.from_pretrained("gpt2")
tokens = tokenizer("Hello world")
print(tokens)
#+end_src

* Related

- [[roam:Embeddings]]
- [[roam:Vocabulary Size]]
```

### Step 3: Create Long-Running Note

```
C-c n f → l → deep-learning → transformers → Transformers
```

### Step 4: Embed Atomic Notes

Edit `long-running-transformers.org`:

```org
#+title: Transformers
#+filetags: :deep-learning:transformers:longrunning:

* Overview

Transformers revolutionized NLP by replacing recurrence with attention.
This note aggregates all transformer-related concepts.

* Architecture Components

** Tokenization
#+transclude: [[file:tokenization.org]] :level 3

** Attention Mechanism
#+transclude: [[file:attention.org]] :level 3

** Position Embeddings
#+transclude: [[file:position-embeddings.org]] :level 3

* Advanced Topics

** Grouped Query Attention (GQA)
#+transclude: [[file:gqa.org]] :level 3

** Multi-Query Attention (MQA)
#+transclude: [[file:mqa.org]] :level 3

* Training Details

** Optimization
#+transclude: [[file:adam-optimizer.org]] :level 3

** Learning Rate Scheduling
#+transclude: [[file:lr-scheduling.org]] :level 3
```

### Step 5: Activate Transclusion

In the long-running note:
```
M-x org-transclusion-mode
```

Or use keybinding: `C-c n T`

Now all the atomic notes' content appears inline! When you edit an atomic note, it updates everywhere it's transcluded.

## Transclusion Features

### Basic Transclusion
```org
#+transclude: [[file:tokenization.org]]
```
Embeds entire file.

### With Heading Level
```org
#+transclude: [[file:tokenization.org]] :level 3
```
Adjusts heading levels (the content's headings become level 3+).

### Transclude Specific Section
```org
#+transclude: [[file:tokenization.org::*Details]]
```
Only embeds the "Details" heading and its content.

### Transclude by ID
```org
#+transclude: [[id:node-id-here]]
```
Works with org-roam node IDs.

### Lines Range
```org
#+transclude: [[file:tokenization.org]] :lines 10-50
```
Only specific line range.

## Keybindings Reference

### Org-Roam Core
| Keybinding | Command                  | Description                    |
|------------|--------------------------|--------------------------------|
| `C-c n f`  | org-roam-node-find       | Find or create note            |
| `C-c n i`  | org-roam-node-insert     | Insert link to note            |
| `C-c n l`  | org-roam-buffer-toggle   | Toggle backlinks sidebar       |
| `C-c n d`  | org-roam-dailies-today   | Go to today's daily note       |
| `C-c n c`  | org-roam-capture         | Capture new note               |

### Org-Transclusion
| Keybinding | Command                         | Description                    |
|------------|---------------------------------|--------------------------------|
| `C-c n t`  | org-transclusion-add            | Add transclusion at point      |
| `C-c n T`  | org-transclusion-mode           | Toggle transclusion mode       |
| `C-c n e`  | org-transclusion-make-from-link | Convert link to transclusion   |

### Within Transclusion Mode
- `C-c n t` - Add/update all transclusions
- Navigate to transcluded content and edit directly
- `C-c C-o` - Open source file

## Tips and Tricks

### 1. Reusing Atomic Notes
The same atomic note can be transcluded in multiple long-running notes:

```org
# In long-running-transformers.org
#+transclude: [[file:attention.org]]

# In long-running-vision-transformers.org
#+transclude: [[file:../transformers/attention.org]]

# In long-running-bert.org
#+transclude: [[file:../transformers/attention.org]]
```

### 2. Mixing Transclusion with Original Content
```org
* Custom Analysis

My thoughts on transformers...

** Background: Attention Mechanism
#+transclude: [[file:attention.org]] :level 3

** My Take

Building on the above, I think...
```

### 3. Export Long-Running Notes
Long-running notes with transclusions can be exported:

```
M-x org-transclusion-mode
C-c C-e h o    # Export to HTML and open
```

The exported file includes all transcluded content!

### 4. Quick Navigation
Use `C-c n l` to see backlinks. This shows:
- What notes link to current note
- Where this note is transcluded

### 5. Visual Graph
```
M-x org-roam-ui-mode
```
Opens browser visualization of your note network.

## Workflow Patterns

### Pattern 1: Bottom-Up Learning
1. Study a topic
2. Create atomic notes for each concept
3. Link related concepts
4. Create long-running note to tie it together

### Pattern 2: Top-Down Planning
1. Create long-running note with outline
2. Identify needed atomic notes
3. Create atomic notes to fill gaps
4. Transclude into long-running note

### Pattern 3: Incremental Building
1. Start with atomic notes
2. Discover patterns/themes
3. Create long-running notes for themes
4. Refactor atomic notes as needed

## Example: Building a Course Guide

### Subject: Deep Learning Course

**Folder structure**:
```
deep-learning/
├── course/
│   ├── long-running-course-notes.org  ← Main course guide
│   ├── week1-intro.org
│   ├── week2-backprop.org
│   └── ...
├── concepts/
│   ├── backpropagation.org
│   ├── gradient-descent.org
│   └── ...
└── projects/
    ├── mnist-classifier.org
    └── ...
```

**Main course note** (`long-running-course-notes.org`):
```org
#+title: Deep Learning Course
#+filetags: :deep-learning:course:

* Week 1: Introduction

** Overview
My notes from Week 1...

** Key Concepts
#+transclude: [[file:../concepts/neural-networks.org]] :level 3

* Week 2: Backpropagation

** Lecture Notes
#+transclude: [[file:week2-backprop.org]] :level 3

** Deep Dive
#+transclude: [[file:../concepts/backpropagation.org]] :level 3
#+transclude: [[file:../concepts/gradient-descent.org]] :level 3

* Projects

** MNIST Classifier
#+transclude: [[file:../projects/mnist-classifier.org]] :level 3
```

## Troubleshooting

### Transclusion Not Showing
- Ensure `org-transclusion-mode` is enabled (`C-c n T`)
- Check file path is correct
- Try `M-x org-transclusion-add` to refresh

### Template Variables Not Working
- Ensure you spell variable names exactly: `${subject}`, `${topic}`, `${title}`, `${slug}`
- Slug is auto-generated from title
- If a variable is not filled, org-roam will prompt you

### Can't Find Notes
- Run `M-x org-roam-db-sync` to rebuild database
- Check `org-roam-directory` points to correct location

### Performance Issues
- With many notes, consider excluding certain folders
- Use `org-roam-db-gc-threshold` to tune garbage collection

## Advanced: Custom Templates

Add more templates to `org-roam-capture-templates` in your config:

```elisp
;; Book notes template
("b" "book note" plain
 "* Metadata\n- Author: %?\n- Published: \n- ISBN: \n\n* Summary\n\n* Key Ideas\n\n* Quotes\n\n"
 :target (file+head "resources/books/${slug}.org"
                    "#+title: ${title}\n#+filetags: :book:resource:\n")
 :unnarrowed t)

;; Meeting notes
("m" "meeting" plain
 "* Attendees\n\n* Agenda\n\n%?\n\n* Action Items\n\n"
 :target (file+head "meetings/%<%Y-%m-%d>-${slug}.org"
                    "#+title: ${title}\n#+date: %<%Y-%m-%d>\n#+filetags: :meeting:\n")
 :unnarrowed t)
```

## Resources

- [Org-roam documentation](https://www.orgroam.com/)
- [Org-transclusion manual](https://github.com/nobiot/org-transclusion)
- Zettelkasten method: [zettelkasten.de](https://zettelkasten.de/)
- Your keybindings: See `~/.emacs.d/KEYBINDINGS.org`

## Quick Start Checklist

- [ ] Reload Emacs config: `C-c r`
- [ ] Create first atomic note: `C-c n f` → `a`
- [ ] Create second atomic note on related topic
- [ ] Create long-running note: `C-c n f` → `l`
- [ ] Add transclusions to long-running note
- [ ] Enable transclusion mode: `C-c n T`
- [ ] Edit an atomic note and see it update in long-running note
- [ ] Open org-roam graph: `M-x org-roam-ui-mode`
- [ ] Explore backlinks: `C-c n l`

Happy note-taking!
