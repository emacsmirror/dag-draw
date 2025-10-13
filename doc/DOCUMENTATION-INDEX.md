# Documentation Index for dag-draw.el

This document provides a roadmap to all documentation in the project and specifies the reading order for new developers.

## Quick Start: Essential Reading Order

For new developers getting started with the codebase, read in this order:

1. **technique-for-drawing-directed-graphs.asciidoc** - The source of truth
2. **ubiquitous-language.org** - Terminology reference
3. **implementation-decisions.md** - Understand the 45+ decisions made
4. **algorithm-specification.md** - Detailed algorithm specifications
5. **gknv-to-ascii-mapping.md** - How paper maps to code structure
6. **ascii-rendering-strategy.md** - ASCII-specific rendering approach
7. **implementation-roadmap.md** - Implementation order and milestones

## Core Documentation (Active)

### Source of Truth

#### technique-for-drawing-directed-graphs.asciidoc
**Status**: Active - Source of Truth
**Purpose**: The GKNV paper "A Technique for Drawing Directed Graphs" by Gansner, Koutsofios, North, and Vo (1993)
**When to use**: Any time you need to verify algorithm behavior, understand the mathematical foundation, or resolve ambiguities
**Notes**: This is the ultimate authority. All implementation decisions must be traceable back to this paper.

### Terminology and Language

#### ubiquitous-language.org
**Status**: Active - Terminology Authority
**Purpose**: Authoritative mapping between GKNV paper concepts and implementation terminology
**When to use**:
- When you encounter unfamiliar terms in code or documentation
- When writing new code (to ensure consistent terminology)
- To find where a concept is defined in the paper
**Organization**: Organized by algorithm phase (Pass 1-4, Global Context, Cross-Pass)
**Contains**:
- Term definitions with paper section references
- Line numbers from the original paper
- Example lookups showing how to use the document

### Implementation Guidance

#### implementation-decisions.md
**Status**: Active - Decision Record
**Purpose**: Documents all 45+ decision points with rationale and paper references
**When to use**:
- Before implementing any feature (check if decision already made)
- When encountering multiple approaches (see which was chosen and why)
- To understand why code is structured a certain way
**Contains**:
- Pass 1 (Ranking): 10 decisions
- Pass 2 (Ordering): 9 decisions
- Pass 3 (Positioning): 7 decisions
- Pass 4 (Spline/Edge Drawing): 12 decisions
- ASCII Rendering: 10 decisions
- Decision summary table
- Classification: GKNV Baseline, Optimizations, Innovations, Enhancements, ASCII-specific
- Remaining ambiguities requiring human decision

#### algorithm-specification.md
**Status**: Active - Technical Specification
**Purpose**: Detailed specifications of all four passes with pseudocode and data structures
**When to use**:
- When implementing a specific pass or function
- To understand data flow between passes
- To see the exact function signatures and data structures
**Contains**:
- Data structures (Node, Edge, Graph, Tree structures)
- Pass 1: Ranking (cycle breaking, network simplex, feasible tree)
- Pass 2: Ordering (virtual nodes, median heuristic, transpose)
- Pass 3: Positioning (auxiliary graph, coordinate assignment)
- Pass 4: Edge Drawing (spline computation, region construction)
- ASCII Rendering (coordinate scaling, edge routing, junctions)
- Integration points between passes
- Verification functions

#### gknv-to-ascii-mapping.md
**Status**: Active - Code Structure Map
**Purpose**: Maps GKNV paper concepts to actual code files and functions
**When to use**:
- To find where a specific algorithm component is implemented
- To understand which code can/cannot be thrown away
- To see the flow of logic through the codebase
**Notes**: Must be kept up-to-date as implementation progresses. This is the key to understanding what code is needed and what can be removed.

#### ascii-rendering-strategy.md
**Status**: Active - ASCII Rendering Guide
**Purpose**: Detailed strategy for rendering GKNV algorithm output as ASCII art
**When to use**:
- When implementing ASCII-specific features
- To understand junction character algorithm
- To see character set choices and scaling strategies
**Contains**:
- Coordinate scaling approach
- Box-drawing character selection
- Edge routing on character grid
- Junction character algorithm details
- Arrow placement strategy
- Node rendering specifications

#### implementation-roadmap.md
**Status**: Active - Project Plan
**Purpose**: Defines implementation order, milestones, and success criteria
**When to use**:
- To understand what phase of implementation we're in
- To see what comes next
- To check if a feature is in baseline vs future enhancement
**Contains**:
- Phase breakdown (baseline implementation phases)
- Milestone definitions
- Success criteria for each phase
- Testing strategy
- Future enhancements list

## Reference Documentation (Active)

### Coding Standards

#### emacs-lisp-style-guide.org
**Status**: Active - Style Reference
**Purpose**: Emacs Lisp coding style guide and best practices
**When to use**:
- When writing or reviewing code
- To resolve style questions
- For naming conventions, formatting, documentation standards
**Topics**:
- Source code layout & organization
- Syntax conventions
- Naming conventions
- Macros and functions
- Comments and docstrings
- Tools (checkdoc, package-lint)

### Emacs Lisp Reference Materials

#### Emacs-*.html files
**Status**: Active - Reference Material
**Purpose**: Excerpts from GNU Emacs Lisp Reference Manual
**When to use**: When you need official Emacs documentation on specific topics
**Files**:
- **Emacs-Coding-Conventions.html** - Official Emacs Lisp coding conventions
- **Emacs-Comment-Tips.html** - How to write comments
- **Emacs-Compilation-Tips.html** - Byte-compilation guidance
- **Emacs-Documentation-Tips.html** - Docstring guidelines
- **Emacs-Key-Binding-Conventions.html** - Key binding standards
- **Emacs-Library-Headers.html** - Standard file headers
- **Emacs-Programming-Tips.html** - General programming advice
- **Emacs-Warning-Tips.html** - Avoiding warnings

## Archived Documentation (Historical)

### archive/

The `doc/archive/` directory contains historical documents that have been superseded. See `doc/archive/README.md` for full explanation of why each document was archived.

**When to use archived docs**:
- For historical context only
- To understand early exploration and decisions
- As source of future enhancement ideas (after baseline complete)

**Archived documents**:
- **archive/summary.txt** - Early understanding with outdated backtracking approach
- **archive/plan-to-correct-discrepancies.txt** - Future enhancement suggestions (not baseline)

**Important**: Do NOT use archived documents for implementation. They contain contradictions with the baseline approach.

## Documentation Maintenance

### When to Update

- **technique-for-drawing-directed-graphs.asciidoc**: Never (source of truth)
- **ubiquitous-language.org**: When new terms are encountered or clarified
- **implementation-decisions.md**: When new decisions are made or ambiguities resolved
- **algorithm-specification.md**: When implementation reveals needed specification details
- **gknv-to-ascii-mapping.md**: Continuously as code structure evolves (critical)
- **ascii-rendering-strategy.md**: When ASCII rendering details are clarified
- **implementation-roadmap.md**: As phases complete and milestones are reached
- **DOCUMENTATION-INDEX.md** (this file): When documentation structure changes

### Documentation Principles

1. **Single Source of Truth**: technique-for-drawing-directed-graphs.asciidoc is the ultimate authority
2. **No Contradictions**: All documentation must be consistent with implementation-decisions.md
3. **Traceability**: Every implementation choice must trace back to the paper or be clearly marked as ASCII-specific
4. **Living Documents**: gknv-to-ascii-mapping.md especially must stay current
5. **Clear Status**: Mark documents as Active, Archived, or Historical
6. **Context Over Explanation**: Focus on what was decided and why, not tutorials

## Finding Information

### "Where is X in the paper?"
1. Check ubiquitous-language.org for the term
2. Look up the section reference provided
3. Read the specific section in technique-for-drawing-directed-graphs.asciidoc

### "What decision was made about Y?"
1. Search implementation-decisions.md for the topic
2. Check the decision ID and rationale
3. Verify it's marked as GKNV Baseline (not future enhancement)

### "How do I implement Z?"
1. Check implementation-decisions.md to see what was decided
2. Read algorithm-specification.md for the detailed specification
3. Look in gknv-to-ascii-mapping.md to see where it should go in the code
4. Reference ubiquitous-language.org for correct terminology

### "What code maps to this paper section?"
1. Go to gknv-to-ascii-mapping.md
2. Find the section in the mapping
3. See which files and functions implement it

### "Is this feature in baseline or future work?"
1. Check implementation-roadmap.md for phase breakdown
2. Cross-reference with implementation-decisions.md
3. Anything marked as "enhancement" or "future" is not baseline

## Development Workflow

### Starting a New Feature

1. Read the relevant section in technique-for-drawing-directed-graphs.asciidoc
2. Check if decision already made in implementation-decisions.md
3. Review specification in algorithm-specification.md
4. Check terminology in ubiquitous-language.org
5. See where it fits in gknv-to-ascii-mapping.md
6. Write tests (TDD)
7. Implement following algorithm-specification.md
8. Update gknv-to-ascii-mapping.md with actual code structure

### Resolving Ambiguity

1. Check technique-for-drawing-directed-graphs.asciidoc first
2. If paper unclear, check implementation-decisions.md "Remaining Ambiguities"
3. If not documented, discuss and add decision to implementation-decisions.md
4. Update related specifications in algorithm-specification.md
5. Update terminology in ubiquitous-language.org if needed

### Code Review Checklist

1. Does it follow a decision in implementation-decisions.md?
2. Does it match specification in algorithm-specification.md?
3. Is it properly mapped in gknv-to-ascii-mapping.md?
4. Does it use correct terminology from ubiquitous-language.org?
5. Does it follow emacs-lisp-style-guide.org?
6. Is it baseline or enhancement? (Baseline only until Phase 5)

## Version History

- **Version 1.0** (2025-10-13): Initial comprehensive documentation index created after documentation cleanup

---

**Document Status**: Active - Index and Guide
**Maintainer**: dag-draw.el development team
**Last Updated**: 2025-10-13
