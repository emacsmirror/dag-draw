# GKNV Algorithm to ASCII Code Mapping

## Overview

This document maintains a living mapping between the GKNV paper algorithms and the dag-draw.el codebase. It serves as the authoritative reference for:
- What code corresponds to which algorithm
- What code can be safely removed
- What code must be kept
- The flow of logic through the codebase

**This document must be updated whenever code is added, modified, or removed.**

## Table of Contents

1. [Main Algorithm Flow](#main-algorithm-flow)
2. [Pass 1: Ranking](#pass-1-ranking)
3. [Pass 2: Ordering](#pass-2-ordering)
4. [Pass 3: Positioning](#pass-3-positioning)
5. [Pass 4: Edge Drawing](#pass-4-edge-drawing)
6. [ASCII Rendering](#ascii-rendering)
7. [Data Structures](#data-structures)
8. [Utility Functions](#utility-functions)
9. [Code Status Legend](#code-status-legend)

---

## Code Status Legend

- ✅ **IMPLEMENTED** - Code exists and is tested
- 🚧 **IN PROGRESS** - Code partially implemented
- ❌ **NOT IMPLEMENTED** - Needs implementation
- ⚠️ **NEEDS REVIEW** - Implemented but needs verification
- 🗑️ **DEPRECATED** - Old code to be removed
- 📝 **DOCUMENTED ONLY** - Specification exists, no code yet

---

## Main Algorithm Flow

### GKNV Figure 1-1: Main Algorithm

```
procedure draw_graph()
begin
  rank();
  ordering();
  position();
  make_splines();
end
```

### Implementation

| Component | File | Function | Status | Notes |
|-----------|------|----------|--------|-------|
| Main entry point | `dag-draw.el` | `dag-draw-graph` | ❌ | Top-level function |
| Validation | `dag-draw.el` | `dag-draw--validate-graph` | ❌ | Input validation |
| Pass orchestration | `dag-draw.el` | `dag-draw--execute-passes` | ❌ | Calls 4 passes |

**Expected code:**
```elisp
(defun dag-draw-graph (graph)
  "Draw GRAPH using GKNV algorithm.
Returns ASCII art string."
  (dag-draw--validate-graph graph)
  (dag-draw-rank graph)              ; Pass 1
  (dag-draw-ordering graph)          ; Pass 2
  (dag-draw-position graph)          ; Pass 3
  (dag-draw-make-splines graph)      ; Pass 4
  (dag-draw-render-ascii graph))     ; ASCII rendering

;; Location: dag-draw.el, line TBD
;; Status: ❌ NOT IMPLEMENTED
```

---

## Pass 1: Ranking

### GKNV Section 2: Optimal Rank Assignment

### GKNV Figure 2-1: Network Simplex

```
procedure rank()
  feasible_tree();
  while (e = leave_edge()) ≠ nil do
    f = enter_edge(e);
    exchange(e,f);
  end
  normalize();
  balance();
end
```

### Implementation Mapping

#### Main Function

| Component | File | Function | Status | GKNV Ref |
|-----------|------|----------|--------|----------|
| Main ranking | `dag-draw-pass1-ranking.el` | `dag-draw-rank` | ❌ | Figure 2-1 line 1 |

**Expected code:**
```elisp
(defun dag-draw-rank (graph)
  "Assign ranks to all nodes using network simplex (GKNV Section 2)."
  (dag-draw--break-cycles graph)
  (dag-draw--merge-constraint-sets graph)
  (let ((tree (dag-draw--feasible-tree graph)))
    (dag-draw--network-simplex tree graph))
  (dag-draw--normalize-ranks graph)
  (dag-draw--balance-ranks graph)
  (dag-draw--unmerge-constraint-sets graph)
  graph)

;; Location: dag-draw-pass1-ranking.el, line TBD
;; Status: ❌ NOT IMPLEMENTED
;; GKNV Reference: Figure 2-1
```

#### Cycle Breaking

| Component | File | Function | Status | GKNV Ref |
|-----------|------|----------|--------|----------|
| Cycle breaking | `dag-draw-cycle-breaking.el` | `dag-draw--break-cycles` | 🚧 | Section 2.1 |
| DFS traversal | `dag-draw-cycle-breaking.el` | `dag-draw--dfs-break-cycles` | 🚧 | Section 2.1 |
| Edge reversal | `dag-draw-cycle-breaking.el` | `dag-draw--reverse-edge` | 🚧 | Section 2.1 |

**Existing code location:**
```elisp
;; File: dag-draw-cycle-breaking.el
;; Functions: dag-draw-break-cycles, dag-draw--dfs-...
;; Status: 🚧 IN PROGRESS - Needs integration with main pass
;; GKNV Reference: Section 2.1, page 6-7
```

#### Feasible Tree Construction

| Component | File | Function | Status | GKNV Ref |
|-----------|------|----------|--------|----------|
| Feasible tree | `dag-draw-pass1-ranking.el` | `dag-draw--feasible-tree` | ❌ | Figure 2-2 line 1 |
| Initial rank | `dag-draw-pass1-ranking.el` | `dag-draw--init-rank` | ❌ | Figure 2-2 line 2 |
| Tight tree | `dag-draw-pass1-ranking.el` | `dag-draw--tight-tree` | ❌ | Figure 2-2 line 3 |
| Min slack edge | `dag-draw-pass1-ranking.el` | `dag-draw--find-min-slack-incident-edge` | ❌ | Figure 2-2 line 4-5 |
| Adjust ranks | `dag-draw-pass1-ranking.el` | `dag-draw--adjust-ranks-for-tight-edge` | ❌ | Figure 2-2 line 6-8 |
| Init cut values | `dag-draw-pass1-ranking.el` | `dag-draw--init-cut-values` | ❌ | Figure 2-2 line 10 |

**Expected code:**
```elisp
;; Location: dag-draw-pass1-ranking.el
;; Status: ❌ NOT IMPLEMENTED
;; GKNV Reference: Figure 2-2, Section 2.3
```

#### Network Simplex Iteration

| Component | File | Function | Status | GKNV Ref |
|-----------|------|----------|--------|----------|
| Leave edge | `dag-draw-network-simplex.el` | `dag-draw--find-leave-edge` | ⚠️ | Figure 2-1 line 3 |
| Enter edge | `dag-draw-network-simplex.el` | `dag-draw--find-enter-edge` | ⚠️ | Figure 2-1 line 4 |
| Exchange | `dag-draw-network-simplex.el` | `dag-draw--exchange-edges` | ⚠️ | Figure 2-1 line 5 |

**Existing code location:**
```elisp
;; File: dag-draw-network-simplex-core-test.el has some test infrastructure
;; File: dag-draw-network-simplex-iteration-test.el has test cases
;; Status: ⚠️ NEEDS REVIEW - Tests exist but implementation may be incomplete
;; GKNV Reference: Figure 2-1, Section 2.3
```

#### Cut Value Computation

| Component | File | Function | Status | GKNV Ref |
|-----------|------|----------|--------|----------|
| Cut value | `dag-draw-pass1-ranking.el` | `dag-draw--compute-cut-value` | ❌ | Section 2.3 |
| Incremental | `dag-draw-pass1-ranking.el` | `dag-draw--compute-cut-value-incremental` | ❌ | Section 2.4 |
| Postorder | `dag-draw-pass1-ranking.el` | `dag-draw--compute-postorder` | ❌ | Section 2.4 |

**Expected code:**
```elisp
;; Location: dag-draw-pass1-ranking.el
;; Status: ❌ NOT IMPLEMENTED
;; GKNV Reference: Section 2.4, pages 11-13
```

#### Normalization and Balancing

| Component | File | Function | Status | GKNV Ref |
|-----------|------|----------|--------|----------|
| Normalize | `dag-draw-pass1-ranking.el` | `dag-draw--normalize-ranks` | ❌ | Figure 2-1 line 7 |
| Balance | `dag-draw-pass1-ranking.el` | `dag-draw--balance-ranks` | ❌ | Figure 2-1 line 8 |

**Expected code:**
```elisp
;; Location: dag-draw-pass1-ranking.el
;; Status: ❌ NOT IMPLEMENTED
;; GKNV Reference: Figure 2-1 lines 7-8, Section 2.3
```

---

## Pass 2: Ordering

### GKNV Section 3: Vertex Ordering Within Ranks

### GKNV Figure 3-1: Vertex Ordering Algorithm

```
procedure ordering()
  order = init_order();
  best = order;
  for i = 0 to Max_iterations do
    wmedian(order,i);
    transpose(order);
    if crossing(order) < crossing(best) then
      best = order;
  end
  return best;
end
```

### Implementation Mapping

#### Main Function

| Component | File | Function | Status | GKNV Ref |
|-----------|------|----------|--------|----------|
| Main ordering | `dag-draw-pass2-ordering.el` | `dag-draw-ordering` | ❌ | Figure 3-1 line 1 |

**Expected code:**
```elisp
(defun dag-draw-ordering (graph)
  "Order nodes within ranks to minimize crossings (GKNV Section 3)."
  (dag-draw--create-virtual-nodes graph)
  (let* ((order (dag-draw--init-order graph))
         (best-order (copy-sequence order))
         (best-crossings (dag-draw--count-crossings order graph)))
    (dotimes (iter 24)
      (dag-draw--wmedian order iter graph)
      (dag-draw--transpose order graph)
      (let ((crossings (dag-draw--count-crossings order graph)))
        (when (< crossings best-crossings)
          (setq best-crossings crossings
                best-order (copy-sequence order)))))
    (dag-draw--apply-ordering best-order graph))
  graph)

;; Location: dag-draw-pass2-ordering.el, line TBD
;; Status: ❌ NOT IMPLEMENTED
;; GKNV Reference: Figure 3-1, Section 3
```

#### Virtual Nodes

| Component | File | Function | Status | GKNV Ref |
|-----------|------|----------|--------|----------|
| Create virtuals | `dag-draw-pass2-ordering.el` | `dag-draw--create-virtual-nodes` | ❌ | Section 3 start |
| Split edge | `dag-draw-pass2-ordering.el` | `dag-draw--split-edge-with-virtuals` | ❌ | Section 3 start |

**Expected code:**
```elisp
;; Location: dag-draw-pass2-ordering.el
;; Status: ❌ NOT IMPLEMENTED
;; GKNV Reference: Section 3, first paragraph
```

#### Initial Ordering

| Component | File | Function | Status | GKNV Ref |
|-----------|------|----------|--------|----------|
| Init order | `dag-draw-pass2-ordering.el` | `dag-draw--init-order` | ❌ | Figure 3-1 line 2 |
| DFS order | `dag-draw-pass2-ordering.el` | `dag-draw--dfs-order` | ❌ | Section 3 remarks |

**Expected code:**
```elisp
;; Location: dag-draw-pass2-ordering.el
;; Status: ❌ NOT IMPLEMENTED
;; GKNV Reference: Figure 3-1 remarks, Section 3 end
```

#### Weighted Median

| Component | File | Function | Status | GKNV Ref |
|-----------|------|----------|--------|----------|
| Wmedian | `dag-draw-pass2-ordering.el` | `dag-draw--wmedian` | ❌ | Figure 3-2 line 1 |
| Median value | `dag-draw-pass2-ordering.el` | `dag-draw--median-value` | ❌ | Figure 3-2 line 12 |
| Adj positions | `dag-draw-pass2-ordering.el` | `dag-draw--adjacent-positions` | ❌ | Figure 3-2 line 13 |

**Expected code:**
```elisp
;; Location: dag-draw-pass2-ordering.el
;; Status: ❌ NOT IMPLEMENTED
;; GKNV Reference: Figure 3-2, Section 3 pages 14-16
```

#### Transpose

| Component | File | Function | Status | GKNV Ref |
|-----------|------|----------|--------|----------|
| Transpose | `dag-draw-pass2-ordering.el` | `dag-draw--transpose` | ❌ | Figure 3-3 line 1 |
| Crossing count | `dag-draw-pass2-ordering.el` | `dag-draw--crossing-vw` | ❌ | Figure 3-3 line 9 |

**Expected code:**
```elisp
;; Location: dag-draw-pass2-ordering.el
;; Status: ❌ NOT IMPLEMENTED
;; GKNV Reference: Figure 3-3, Section 3 page 16
```

---

## Pass 3: Positioning

### GKNV Section 4: Node Coordinates

### GKNV Section 4.2: Optimal Node Placement (Preferred Method)

```
Using auxiliary graph + network simplex
(Section 4.2 describes this as superior to heuristic approach)
```

### Implementation Mapping

#### Main Function

| Component | File | Function | Status | GKNV Ref |
|-----------|------|----------|--------|----------|
| Main positioning | `dag-draw-pass3-positioning.el` | `dag-draw-position` | ❌ | Section 4.2 |

**Expected code:**
```elisp
(defun dag-draw-position (graph)
  "Assign X and Y coordinates using auxiliary graph (GKNV Section 4.2)."
  (dag-draw--assign-y-coordinates graph)
  (let ((aux-graph (dag-draw--build-auxiliary-graph graph)))
    (dag-draw-rank aux-graph)  ; Reuse network simplex from Pass 1
    (dag-draw--extract-x-coordinates graph aux-graph))
  graph)

;; Location: dag-draw-pass3-positioning.el, line TBD
;; Status: ❌ NOT IMPLEMENTED
;; GKNV Reference: Section 4.2
```

#### Y Coordinates

| Component | File | Function | Status | GKNV Ref |
|-----------|------|----------|--------|----------|
| Y assignment | `dag-draw-pass3-positioning.el` | `dag-draw--assign-y-coordinates` | ❌ | Section 4 start |

**Expected code:**
```elisp
;; Location: dag-draw-pass3-positioning.el
;; Status: ❌ NOT IMPLEMENTED
;; GKNV Reference: Section 4, first paragraph
```

#### Auxiliary Graph

| Component | File | Function | Status | GKNV Ref |
|-----------|------|----------|--------|----------|
| Build aux graph | `dag-draw-auxiliary-graph.el` | `dag-draw--build-auxiliary-graph` | ⚠️ | Section 4.2, Figure 4-2 |
| Compute omega | `dag-draw-pass3-positioning.el` | `dag-draw--compute-omega` | ❌ | Section 4, page 17 |
| Compute rho | `dag-draw-pass3-positioning.el` | `dag-draw--compute-rho` | ❌ | Section 4, page 17 |
| Add aux edge | `dag-draw-auxiliary-graph.el` | `dag-draw--add-aux-edge` | ⚠️ | Section 4.2 |

**Existing code location:**
```elisp
;; File: test/dag-draw-auxiliary-graph-test.el has tests
;; Status: ⚠️ NEEDS REVIEW - Tests exist, verify implementation complete
;; GKNV Reference: Section 4.2, pages 19-21
```

#### X Coordinate Extraction

| Component | File | Function | Status | GKNV Ref |
|-----------|------|----------|--------|----------|
| Extract X | `dag-draw-pass3-positioning.el` | `dag-draw--extract-x-coordinates` | ❌ | Section 4.2 |

**Expected code:**
```elisp
;; Location: dag-draw-pass3-positioning.el
;; Status: ❌ NOT IMPLEMENTED
;; GKNV Reference: Section 4.2, page 20
```

#### Node Ports

| Component | File | Function | Status | GKNV Ref |
|-----------|------|----------|--------|----------|
| Port support | `dag-draw-ports.el` | `dag-draw--apply-port-offsets` | 🚧 | Section 4.2, Figure 4-3 |
| Port delta | `dag-draw-ports.el` | `dag-draw--compute-port-delta` | 🚧 | Section 4.2, Figure 4-4 |

**Existing code location:**
```elisp
;; File: dag-draw-ports.el has port-related code
;; File: test/dag-draw-port-calculation-test.el has tests
;; Status: 🚧 IN PROGRESS
;; GKNV Reference: Section 4.2, Figure 4-3, Figure 4-4
```

---

## Pass 4: Edge Drawing

### GKNV Section 5: Drawing Edges

### Main Algorithm (Implicit from Section 5)

```
For each edge (shortest first):
  1. Find region (boxes)
  2. Compute path through region
  3. Generate spline fitting path
  4. Update virtual node boxes
```

### Implementation Mapping

#### Main Function

| Component | File | Function | Status | GKNV Ref |
|-----------|------|----------|--------|----------|
| Main splines | `dag-draw-pass4-splines.el` | `dag-draw-make-splines` | ❌ | Section 5 |
| Sort by length | `dag-draw-pass4-splines.el` | `dag-draw--sort-edges-by-length` | ❌ | Section 5.1.1 |

**Expected code:**
```elisp
(defun dag-draw-make-splines (graph)
  "Compute spline control points for all edges (GKNV Section 5)."
  (let ((edges-by-length (dag-draw--sort-edges-by-length graph)))
    (dolist (edge edges-by-length)
      (dag-draw--compute-edge-spline edge graph)))
  graph)

;; Location: dag-draw-pass4-splines.el, line TBD
;; Status: ❌ NOT IMPLEMENTED
;; GKNV Reference: Section 5
```

#### Region Construction

| Component | File | Function | Status | GKNV Ref |
|-----------|------|----------|--------|----------|
| Build region | `dag-draw-pass4-splines.el` | `dag-draw--build-edge-region` | ❌ | Section 5.1 |
| Port boxes | `dag-draw-pass4-splines.el` | `dag-draw--make-port-box` | ❌ | Section 5.1 |
| Inter-rank boxes | `dag-draw-pass4-splines.el` | `dag-draw--make-inter-rank-box` | ❌ | Section 5.1.1 |
| Virtual boxes | `dag-draw-pass4-splines.el` | `dag-draw--make-virtual-node-box` | ❌ | Section 5.1.1 |

**Expected code:**
```elisp
;; Location: dag-draw-pass4-splines.el
;; Status: ❌ NOT IMPLEMENTED
;; GKNV Reference: Section 5.1, pages 23-25
```

#### Spline Computation

| Component | File | Function | Status | GKNV Ref |
|-----------|------|----------|--------|----------|
| Compute spline | `dag-draw-pass4-splines.el` | `dag-draw--compute-spline-in-region` | ❌ | Figure 5-2 line 1 |
| L segments | `dag-draw-pass4-splines.el` | `dag-draw--compute-L-segments` | ❌ | Figure 5-2 line 2 |
| Compute path | `dag-draw-pass4-splines.el` | `dag-draw--compute-path` | ❌ | Figure 5-3 line 1 |
| Line fits | `dag-draw-pass4-splines.el` | `dag-draw--line-fits-p` | ❌ | Figure 5-3 line 2 |
| Line split | `dag-draw-pass4-splines.el` | `dag-draw--compute-linesplit` | ❌ | Figure 5-3 line 3 |
| Generate spline | `dag-draw-pass4-splines.el` | `dag-draw--generate-bezier-spline` | ❌ | Figure 5-3 line 10 |
| Spline fits | `dag-draw-pass4-splines.el` | `dag-draw--spline-fits-p` | ❌ | Figure 5-3 line 12 |
| Straighten | `dag-draw-pass4-splines.el` | `dag-draw--straighten-spline` | ❌ | Figure 5-3 line 13 |
| Refine | `dag-draw-pass4-splines.el` | `dag-draw--refine-spline` | ❌ | Figure 5-3 line 18 |

**Expected code:**
```elisp
;; Location: dag-draw-pass4-splines.el
;; Status: ❌ NOT IMPLEMENTED
;; GKNV Reference: Section 5.2, Figure 5-2, Figure 5-3, pages 27-30
```

#### Special Edge Types

| Component | File | Function | Status | GKNV Ref |
|-----------|------|----------|--------|----------|
| Flat edges | `dag-draw-pass4-splines.el` | `dag-draw--route-flat-edge` | ❌ | Section 5.1.2 |
| Adjacent flat | `dag-draw-pass4-splines.el` | `dag-draw--route-adjacent-flat-edge` | ❌ | Section 5.1.2 |
| Self-loops | `dag-draw-pass4-splines.el` | `dag-draw--route-self-loop` | ❌ | Section 5.1.3 |
| Multi-edges | `dag-draw-pass4-splines.el` | `dag-draw--route-multi-edges` | ❌ | Section 5.1.1 |

**Expected code:**
```elisp
;; Location: dag-draw-pass4-splines.el
;; Status: ❌ NOT IMPLEMENTED
;; GKNV Reference: Section 5.1.2, 5.1.3
```

---

## ASCII Rendering

### No direct GKNV equivalent - ASCII-specific adaptation

### Implementation Mapping

#### Main Function

| Component | File | Function | Status | Notes |
|-----------|------|----------|--------|-------|
| Main render | `dag-draw-render.el` | `dag-draw-render-ascii` | 🚧 | Top-level ASCII render |

**Existing code location:**
```elisp
;; File: dag-draw-render.el has some rendering infrastructure
;; File: dag-draw-ascii-grid.el has grid code
;; Status: 🚧 IN PROGRESS
;; Reference: doc/ascii-rendering-strategy.md
```

#### Coordinate Scaling

| Component | File | Function | Status | Reference |
|-----------|------|----------|--------|-----------|
| Find bounds | `dag-draw-render.el` | `dag-draw--find-bounds` | ❌ | ascii-rendering-strategy.md |
| Scale coords | `dag-draw-render.el` | `dag-draw--scale-coordinates` | ❌ | ascii-rendering-strategy.md |
| Grid size | `dag-draw-render.el` | `dag-draw--compute-grid-size` | ❌ | ascii-rendering-strategy.md |

**Expected code:**
```elisp
;; Location: dag-draw-render.el
;; Status: ❌ NOT IMPLEMENTED
;; Reference: doc/ascii-rendering-strategy.md, Coordinate Scaling section
```

#### Grid Structure

| Component | File | Function | Status | Reference |
|-----------|------|----------|--------|-----------|
| Create grid | `dag-draw-ascii-grid.el` | `dag-draw--create-grid` | 🚧 | ascii-rendering-strategy.md |
| Get cell | `dag-draw-ascii-grid.el` | `dag-draw--get-cell` | 🚧 | ascii-rendering-strategy.md |
| Set cell | `dag-draw-ascii-grid.el` | `dag-draw--set-cell` | 🚧 | ascii-rendering-strategy.md |
| Grid cell struct | `dag-draw-ascii-grid.el` | `dag-draw-grid-cell` | 🚧 | ascii-rendering-strategy.md |

**Existing code location:**
```elisp
;; File: dag-draw-ascii-grid.el
;; Status: 🚧 IN PROGRESS
;; Reference: doc/ascii-rendering-strategy.md, Grid Structure section
```

#### Node Rendering

| Component | File | Function | Status | Reference |
|-----------|------|----------|--------|-----------|
| Render node | `dag-draw-render.el` | `dag-draw--render-node` | ❌ | ascii-rendering-strategy.md |
| Node size | `dag-draw-render.el` | `dag-draw--compute-node-size` | ❌ | ascii-rendering-strategy.md |

**Expected code:**
```elisp
;; Location: dag-draw-render.el
;; Status: ❌ NOT IMPLEMENTED
;; Reference: doc/ascii-rendering-strategy.md, Node Rendering section
```

#### Edge Routing

| Component | File | Function | Status | Reference |
|-----------|------|----------|--------|-----------|
| Route edges | `dag-draw-render.el` | `dag-draw--route-edges` | ❌ | ascii-rendering-strategy.md |
| Route edge | `dag-draw-render.el` | `dag-draw--route-edge` | ❌ | ascii-rendering-strategy.md |
| Sample spline | `dag-draw-render.el` | `dag-draw--sample-spline` | ❌ | ascii-rendering-strategy.md |
| Spline to grid | `dag-draw-render.el` | `dag-draw--spline-to-grid-path` | ❌ | ascii-rendering-strategy.md |
| Draw segment | `dag-draw-render.el` | `dag-draw--draw-segment` | ❌ | ascii-rendering-strategy.md |
| Draw horizontal | `dag-draw-render.el` | `dag-draw--draw-horizontal` | ❌ | ascii-rendering-strategy.md |
| Draw vertical | `dag-draw-render.el` | `dag-draw--draw-vertical` | ❌ | ascii-rendering-strategy.md |

**Expected code:**
```elisp
;; Location: dag-draw-render.el
;; Status: ❌ NOT IMPLEMENTED
;; Reference: doc/ascii-rendering-strategy.md, Edge Routing section
```

#### Junction Characters

| Component | File | Function | Status | Reference |
|-----------|------|----------|--------|-----------|
| Apply junctions | `dag-draw-render.el` | `dag-draw--apply-junction-characters` | ❌ | ascii-rendering-strategy.md, CLAUDE.md |
| Walk edge | `dag-draw-render.el` | `dag-draw--walk-edge-for-junctions` | ❌ | ascii-rendering-strategy.md |
| Port junction | `dag-draw-render.el` | `dag-draw--set-port-junction` | ❌ | ascii-rendering-strategy.md |
| Corner junction | `dag-draw-render.el` | `dag-draw--set-corner-junction` | ❌ | ascii-rendering-strategy.md |
| Multi-edge junction | `dag-draw-render.el` | `dag-draw--set-multi-edge-junction` | ❌ | ascii-rendering-strategy.md |
| Corner char | `dag-draw-render.el` | `dag-draw--select-corner-char` | ❌ | ascii-rendering-strategy.md |

**Expected code:**
```elisp
;; Location: dag-draw-render.el
;; Status: ❌ NOT IMPLEMENTED
;; Reference: doc/ascii-rendering-strategy.md, Junction Character Rules
;; Also: CLAUDE.md, Implementation Principles section
```

#### Arrow Placement

| Component | File | Function | Status | Reference |
|-----------|------|----------|--------|-----------|
| Place arrows | `dag-draw-render.el` | `dag-draw--place-arrows` | ❌ | ascii-rendering-strategy.md |
| Place arrow | `dag-draw-render.el` | `dag-draw--place-arrow` | ❌ | ascii-rendering-strategy.md |
| Arrow char | `dag-draw-render.el` | `dag-draw--arrow-char` | ❌ | ascii-rendering-strategy.md |

**Expected code:**
```elisp
;; Location: dag-draw-render.el
;; Status: ❌ NOT IMPLEMENTED
;; Reference: doc/ascii-rendering-strategy.md, Arrow Placement section
```

#### Grid to String

| Component | File | Function | Status | Reference |
|-----------|------|----------|--------|-----------|
| Grid to string | `dag-draw-render.el` | `dag-draw--grid-to-string` | ❌ | ascii-rendering-strategy.md |

**Expected code:**
```elisp
;; Location: dag-draw-render.el
;; Status: ❌ NOT IMPLEMENTED
;; Reference: doc/ascii-rendering-strategy.md
```

---

## Data Structures

### Core Structures

| Structure | File | Status | GKNV Ref |
|-----------|------|--------|----------|
| `dag-draw-node` | `dag-draw.el` | ❌ | Section 1.2, throughout |
| `dag-draw-edge` | `dag-draw.el` | ❌ | Section 1.2, throughout |
| `dag-draw-graph` | `dag-draw.el` | ❌ | Section 1.2 |
| `dag-draw-tree-edge` | `dag-draw.el` | ❌ | Section 2.3 |
| `dag-draw-spanning-tree` | `dag-draw.el` | ❌ | Section 2.3 |
| `dag-draw-grid-cell` | `dag-draw-ascii-grid.el` | 🚧 | ASCII-specific |

**Expected code:**
```elisp
;; Location: dag-draw.el
;; Status: ❌ NOT IMPLEMENTED (except partial grid-cell)
;; Reference: doc/algorithm-specification.md, Data Structures section
```

---

## Utility Functions

### Graph Operations

| Function | File | Status | Purpose |
|----------|------|--------|---------|
| `dag-draw--get-all-nodes` | `dag-draw.el` | ❌ | Get list of all nodes |
| `dag-draw--get-all-edges` | `dag-draw.el` | ❌ | Get list of all edges |
| `dag-draw--get-source-nodes` | `dag-draw.el` | ❌ | Get nodes with no in-edges |
| `dag-draw--get-sink-nodes` | `dag-draw.el` | ❌ | Get nodes with no out-edges |
| `dag-draw--is-acyclic-p` | `dag-draw.el` | ❌ | Check if graph is acyclic |

### Verification Functions

| Function | File | Status | Purpose |
|----------|------|--------|---------|
| `dag-draw--verify-ranking` | `dag-draw.el` | ❌ | Verify ranks respect constraints |
| `dag-draw--verify-ordering` | `dag-draw.el` | ❌ | Verify ordering is valid |
| `dag-draw--verify-positioning` | `dag-draw.el` | ❌ | Verify coords respect separation |

**Expected code:**
```elisp
;; Location: dag-draw.el
;; Status: ❌ NOT IMPLEMENTED
;; Reference: doc/algorithm-specification.md, Verification Functions
```

---

## Code Removal Candidates

### Files/Code to Review for Removal

These files may contain old approaches or incomplete implementations that should be reviewed:

| File | Status | Reason | Action |
|------|--------|--------|--------|
| `dag-draw-test-harness.el` | 🗑️? | May be old testing approach | Review against new test strategy |
| Heuristic positioning code | 🗑️? | If exists, GKNV prefers aux graph | Remove in favor of Section 4.2 |
| Old cycle breaking heuristic | 🗑️? | If exists, use DFS only | Keep only DFS approach |

**Note:** Do NOT remove code without:
1. Reviewing what it does
2. Checking if tests depend on it
3. Documenting why it's being removed
4. Ensuring replacement exists

---

## Usage Examples

### How to Use This Mapping

**When implementing a function:**

1. Find the function in this mapping
2. Note the GKNV reference
3. Read that section of the GKNV paper
4. Check doc/algorithm-specification.md for pseudocode
5. Implement the function
6. Update this mapping with implementation location and status
7. Write tests

**When reviewing code:**

1. Find the code in this mapping
2. Verify it implements the correct GKNV algorithm
3. Check that it matches the specification in algorithm-specification.md
4. Update status if needed

**When refactoring:**

1. Check this mapping to understand what the code does
2. Verify GKNV reference
3. Ensure refactoring maintains algorithm correctness
4. Update mapping if function names/locations change

---

## Update Instructions

### When Adding New Code

```markdown
| Component | File | Function | Status | GKNV Ref |
|-----------|------|----------|--------|----------|
| [Name] | [file.el] | [function-name] | ✅ | [Section X.Y] |
```

Add code location comment:
```elisp
;; Location: file.el, line 42
;; Status: ✅ IMPLEMENTED
;; GKNV Reference: Section X.Y, page N
```

### When Changing Status

Update status emoji:
- ❌ → 🚧 (start implementation)
- 🚧 → ⚠️ (needs review)
- ⚠️ → ✅ (reviewed and tested)

### When Removing Code

1. Mark as 🗑️ DEPRECATED
2. Document why in Notes column
3. After removal, delete row from mapping

---

## Quick Status Summary

### By Pass

- **Pass 1 (Ranking)**: ❌ Core not implemented, 🚧 cycle breaking partial, ⚠️ some tests exist
- **Pass 2 (Ordering)**: ❌ Not implemented
- **Pass 3 (Positioning)**: ⚠️ Aux graph has tests, ❌ main code not implemented
- **Pass 4 (Splines)**: ❌ Not implemented
- **ASCII Rendering**: 🚧 Grid infrastructure partial, ❌ rendering not implemented

### Overall Progress

- ✅ Implemented: ~5%
- 🚧 In Progress: ~10%
- ⚠️ Needs Review: ~5%
- ❌ Not Implemented: ~80%

**Next Priorities:**
1. Complete Pass 1 data structures and main functions
2. Complete cycle breaking integration
3. Implement network simplex core
4. Begin Pass 2 implementation

---

**Document Version**: 1.0
**Last Updated**: 2025-10-13
**Maintainer**: dag-draw.el development team

**Important:** This document MUST be updated with every code change. It is the single source of truth for what exists in the codebase.
