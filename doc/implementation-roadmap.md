# Implementation Roadmap for dag-draw.el

## Overview

This document provides a quick-reference implementation guide, summarizing decisions, priorities, test strategies, and clear separation between GKNV baseline and future enhancements.

## Table of Contents

1. [Quick Decision Reference](#quick-decision-reference)
2. [Implementation Priorities](#implementation-priorities)
3. [Test Strategy](#test-strategy)
4. [GKNV Baseline vs Enhancements](#gknv-baseline-vs-enhancements)
5. [Implementation Phases](#implementation-phases)
6. [Success Criteria](#success-criteria)

---

## Quick Decision Reference

### Pass 1: Ranking

| Component | Decision | Reference |
|-----------|----------|-----------|
| Cycle breaking | DFS back-edge reversal | D1.1, Section 2.1 |
| Initial ranking | Queue-based topological sort | D1.2, Section 2.3 |
| Feasible tree | Incremental tight tree | D1.3, Section 2.3 |
| Leave edge | Cyclic search | D1.4, Section 2.4 |
| Cut values | Incremental + postorder | D1.8, Section 2.4 |
| Normalization | Min rank to 0 | D1.6 |
| Balancing | Greedy rank balancing | D1.7 |

**Key Data Structures:**
- Node: id, rank
- Edge: tail, head, weight, delta, reversed-p
- Tree edge: tail, head, cut-value
- Spanning tree: edges, parent, low, lim

---

### Pass 2: Ordering

| Component | Decision | Reference |
|-----------|----------|-----------|
| Virtual nodes | Create before ordering | D2.8, Section 3 |
| Initial order | Two-phase DFS (forward & backward) | D2.1, Section 3 |
| Median method | Weighted interpolation | D2.2, Section 3 |
| Max iterations | 24 (fixed) | D2.3 |
| Transpose | Iterative to local optimum | D2.4, Section 3 |
| Flat edges | Transitive closure + respect order | D2.6, Section 3 |
| Ties | Flip on alternate iterations | D2.7 |

**Key Data Structures:**
- Virtual nodes: virtual-p, original-edge
- Order: vector of rank -> list of nodes
- Edge virtual chain: list of virtual nodes

---

### Pass 3: Positioning

| Component | Decision | Reference |
|-----------|----------|-----------|
| Method | Network simplex on auxiliary graph | D3.1, Section 4.2 |
| Omega values | 1 (real-real), 2 (real-virtual), 8 (virtual-virtual) | D3.2, Section 4 |
| Node separation | (xsize(a) + xsize(b))/2 + nodesep | D3.3, Section 4 |
| Aux graph init | Exploit structure for fast init | D3.4, Section 4.3 |
| Node ports | X-offset ports supported | D3.5, Section 4.2 |
| Y coordinates | Fixed ranksep | D3.6 |

**Key Data Structures:**
- Node: x, y (float coordinates)
- Auxiliary node: for each edge
- Auxiliary edges: separation constraints

---

### Pass 4: Edge Drawing

| Component | Decision | Reference |
|-----------|----------|-----------|
| Method | Region-constrained splines | D4.1, Section 5 |
| Edge order | Shortest edges first | D4.2, Section 5.1.1 |
| Vertical sections | Hybrid (straight for nearly vertical) | D4.3, Section 5.1.1 |
| Terminal spacing | Subdivide inter-rank space | D4.4, Section 5.1.1 |
| Multi-edges | Offset by nodesep multiple | D4.5, Section 5.1.1 |
| Path computation | Recursive subdivision | D4.9, Section 5.2 |
| Spline fitting | Hybrid straighten/refine | D4.10, Section 5.2 |
| Continuity | C1 (tangent) | D4.11, Section 5.2 |

**Key Data Structures:**
- Edge: control-points (list of (x . y))
- Box region: list of (x-min y-min x-max y-max)
- L segments: line intersections between boxes

---

### ASCII Rendering

| Component | Decision | Reference |
|-----------|----------|-----------|
| Scaling | Independent X/Y to fit grid | D5.1 |
| Characters | Unicode box-drawing (U+2500 block) | D5.2 |
| Edge routing | Hybrid spline approximation | D5.3 |
| Junctions | Walk-based local analysis | D5.4 |
| Arrows | At port boundary | D5.5 |
| Node render | Label size + padding | D5.6 |

**Key Data Structures:**
- Grid: 2D array of cells
- Cell: char, edges (list), edge-directions (hash), node
- Scaled graph: integer grid coordinates

---

## Implementation Priorities

### Phase 0: Foundation (Week 1)

**Goal:** Establish core data structures and infrastructure

**Tasks:**
1. Define data structures (node, edge, graph, tree-edge, spanning-tree)
2. Implement graph construction from input format
3. Create basic graph operations (add-node, add-edge, get-nodes, get-edges)
4. Set up test harness with Buttercup
5. Create example graphs (simple tree, diamond, complex DAG)

**Success Criteria:**
- Can create graphs programmatically
- Can access nodes and edges
- Test framework runs
- Have 3-5 test graphs

---

### Phase 1: Pass 1 - Ranking (Weeks 2-3)

**Goal:** Implement network simplex ranking algorithm

**Implementation Order:**

1. **Cycle Breaking (Week 2, Day 1-2)**
   - Implement DFS traversal
   - Detect back edges
   - Reverse back edges, set reversed-p flag
   - Test: cyclic graph becomes acyclic

2. **Initial Ranking (Week 2, Day 3)**
   - Implement queue-based topological sort
   - Respect delta constraints
   - Test: ranks respect edge lengths

3. **Feasible Tree Construction (Week 2, Day 4-5)**
   - Implement tight_tree() to find maximal tight edges
   - Implement find_min_slack_incident_edge()
   - Adjust ranks to make edges tight
   - Test: produces feasible spanning tree

4. **Cut Value Computation (Week 3, Day 1-2)**
   - Implement incremental cut value calculation
   - Implement postorder traversal with low/lim
   - Test: cut values correct for known trees

5. **Network Simplex Iteration (Week 3, Day 3-4)**
   - Implement find_leave_edge() with cyclic search
   - Implement find_enter_edge()
   - Implement exchange_edges()
   - Test: converges to optimal ranking

6. **Normalization and Balancing (Week 3, Day 5)**
   - Implement normalize_ranks()
   - Implement balance_ranks()
   - Test: min rank is 0, balanced distribution

**Test Strategy for Pass 1:**

```elisp
(describe "Pass 1: Ranking"
  (describe "Cycle Breaking"
    (it "detects and reverses back edges"
      (let ((graph (make-cyclic-graph)))
        (dag-draw--break-cycles graph)
        (expect (dag-draw--is-acyclic-p graph) :to-be t)))
    (it "preserves acyclic graphs"
      (let ((graph (make-tree-graph)))
        (dag-draw--break-cycles graph)
        (expect (dag-draw--is-acyclic-p graph) :to-be t))))

  (describe "Initial Ranking"
    (it "assigns ranks respecting delta"
      (let ((graph (make-simple-graph)))
        (dag-draw--init-rank graph)
        (expect (dag-draw--verify-delta-constraints graph) :to-be t)))
    (it "handles multiple sources"
      (let ((graph (make-multi-source-graph)))
        (dag-draw--init-rank graph)
        (expect (dag-draw--all-nodes-ranked-p graph) :to-be t))))

  (describe "Network Simplex"
    (it "produces feasible ranking"
      (let ((graph (make-test-graph)))
        (dag-draw-rank graph)
        (expect (dag-draw--verify-ranking graph) :to-be t)))
    (it "produces optimal ranking"
      (let ((graph (make-known-optimal-graph)))
        (dag-draw-rank graph)
        (expect (dag-draw--compute-ranking-cost graph)
                :to-equal (known-optimal-cost))))
    (it "normalizes to min rank 0"
      (let ((graph (make-test-graph)))
        (dag-draw-rank graph)
        (expect (dag-draw--min-rank graph) :to-equal 0)))))
```

---

### Phase 2: Pass 2 - Ordering (Weeks 4-5)

**Goal:** Implement vertex ordering to minimize crossings

**Implementation Order:**

1. **Virtual Node Creation (Week 4, Day 1)**
   - Implement split_edge_with_virtuals()
   - Create chains of virtual nodes
   - Test: long edges split correctly

2. **Initial Ordering (Week 4, Day 2)**
   - Implement DFS-based ordering (forward)
   - Implement DFS-based ordering (backward)
   - Test: both orderings, choose better

3. **Weighted Median (Week 4, Day 3-4)**
   - Implement adjacent_positions()
   - Implement median_value() with weighted interpolation
   - Implement wmedian() with up/down sweeps
   - Test: nodes reordered toward median

4. **Transpose (Week 4, Day 5)**
   - Implement crossing_vw() to count crossings
   - Implement transpose() iterative optimization
   - Test: reduces crossings

5. **Flat Edges (Week 5, Day 1)**
   - Implement transitive closure for flat edges
   - Ensure ordering respects partial order
   - Test: flat edges maintain direction

6. **Integration (Week 5, Day 2)**
   - Combine all components
   - Implement 24-iteration loop
   - Test: full ordering pass on various graphs

**Test Strategy for Pass 2:**

```elisp
(describe "Pass 2: Ordering"
  (describe "Virtual Nodes"
    (it "creates virtual nodes for long edges"
      (let ((graph (make-long-edge-graph)))
        (dag-draw--create-virtual-nodes graph)
        (expect (dag-draw--count-virtual-nodes graph) :to-be-greater-than 0)))
    (it "preserves edge connectivity"
      (let ((graph (make-test-graph)))
        (let ((paths-before (dag-draw--all-paths graph)))
          (dag-draw--create-virtual-nodes graph)
          (expect (dag-draw--all-paths graph)
                  :to-have-same-items-as paths-before)))))

  (describe "Median Heuristic"
    (it "computes correct median for odd neighbors"
      (let ((positions '(1 3 5)))
        (expect (dag-draw--median-from-positions positions) :to-equal 3)))
    (it "computes weighted median for even neighbors"
      (let ((positions '(1 2 8 9)))
        (expect (dag-draw--median-from-positions positions)
                :to-be-close-to 2.5 0.5))))

  (describe "Transpose"
    (it "reduces crossings"
      (let ((order (make-crossy-ordering)))
        (let ((before (dag-draw--count-total-crossings order graph)))
          (dag-draw--transpose order graph)
          (expect (dag-draw--count-total-crossings order graph)
                  :to-be-less-than before)))))

  (describe "Full Ordering"
    (it "produces valid ordering"
      (let ((graph (make-test-graph)))
        (dag-draw-rank graph)
        (dag-draw-ordering graph)
        (expect (dag-draw--verify-ordering graph) :to-be t)))
    (it "handles trees without crossings"
      (let ((graph (make-tree-graph)))
        (dag-draw-rank graph)
        (dag-draw-ordering graph)
        (expect (dag-draw--count-total-crossings
                (dag-draw--get-ordering graph) graph)
                :to-equal 0)))))
```

---

### Phase 3: Pass 3 - Positioning (Weeks 6-7)

**Goal:** Assign X and Y coordinates optimally

**Implementation Order:**

1. **Y Coordinates (Week 6, Day 1)**
   - Implement assign_y_coordinates()
   - Simple rank-based placement
   - Test: Y increases with rank

2. **Auxiliary Graph Construction (Week 6, Day 2-3)**
   - Implement build_auxiliary_graph()
   - Create node for each edge
   - Create separation edges
   - Test: auxiliary graph structure correct

3. **Omega Computation (Week 6, Day 4)**
   - Implement compute_omega() for edge types
   - Test: returns 1, 2, or 8 appropriately

4. **Rho Computation (Week 6, Day 4)**
   - Implement compute_rho() for node separation
   - Test: accounts for node sizes and nodesep

5. **Initial Feasible Tree for Aux Graph (Week 6, Day 5)**
   - Implement fast initialization exploiting structure
   - Test: feasible tree constructed quickly

6. **Network Simplex on Aux Graph (Week 7, Day 1-2)**
   - Reuse Pass 1 network simplex
   - Apply to auxiliary graph
   - Test: produces optimal X coordinates

7. **X Coordinate Extraction (Week 7, Day 3)**
   - Implement extract_x_coordinates()
   - Map aux graph ranks to X coords
   - Test: X coordinates extracted correctly

8. **Node Ports (Week 7, Day 4)**
   - Implement X-offset port support
   - Adjust delta values in aux graph
   - Test: ports work correctly

**Test Strategy for Pass 3:**

```elisp
(describe "Pass 3: Positioning"
  (describe "Y Coordinates"
    (it "assigns Y based on rank"
      (let ((graph (make-ranked-graph)))
        (dag-draw--assign-y-coordinates graph)
        (expect (dag-draw--y-increases-with-rank-p graph) :to-be t))))

  (describe "Auxiliary Graph"
    (it "creates auxiliary nodes for edges"
      (let ((graph (make-test-graph))
            (aux (dag-draw--build-auxiliary-graph graph)))
        (expect (hash-table-count (dag-draw-graph-nodes aux))
                :to-be-greater-than
                (hash-table-count (dag-draw-graph-nodes graph)))))
    (it "creates separation edges"
      (let ((graph (make-ranked-ordered-graph))
            (aux (dag-draw--build-auxiliary-graph graph)))
        (expect (dag-draw--has-separation-edges-p aux) :to-be t))))

  (describe "Omega Computation"
    (it "returns 1 for real-real edges"
      (let ((edge (make-real-real-edge)))
        (expect (dag-draw--compute-omega edge) :to-equal 1)))
    (it "returns 8 for virtual-virtual edges"
      (let ((edge (make-virtual-virtual-edge)))
        (expect (dag-draw--compute-omega edge) :to-equal 8))))

  (describe "Full Positioning"
    (it "assigns coordinates to all nodes"
      (let ((graph (make-test-graph)))
        (dag-draw-rank graph)
        (dag-draw-ordering graph)
        (dag-draw-position graph)
        (expect (dag-draw--all-nodes-positioned-p graph) :to-be t)))
    (it "respects separation constraints"
      (let ((graph (make-test-graph)))
        (dag-draw-rank graph)
        (dag-draw-ordering graph)
        (dag-draw-position graph)
        (expect (dag-draw--verify-positioning graph) :to-be t)))))
```

---

### Phase 4: Pass 4 - Edge Drawing (Week 8)

**Goal:** Compute spline control points for edges

**Implementation Order:**

1. **Sort Edges by Length (Week 8, Day 1)**
   - Implement sort_edges_by_length()
   - Test: shortest edges first

2. **Region Construction (Week 8, Day 2)**
   - Implement build_edge_region()
   - Create boxes for ports, inter-rank, virtual nodes
   - Test: region covers edge path

3. **L Segment Computation (Week 8, Day 2)**
   - Implement compute_L_segments()
   - Find box intersections
   - Test: L segments correct

4. **Path Computation (Week 8, Day 3)**
   - Implement compute_path() recursive subdivision
   - Implement line_fits_p()
   - Implement find_line_split()
   - Test: path stays in region

5. **Spline Generation (Week 8, Day 4)**
   - Implement generate_bezier_spline()
   - Implement spline_fits_p()
   - Implement straighten/refine
   - Test: spline fits region

6. **Multi-Edges, Flat Edges, Self-Loops (Week 8, Day 5)**
   - Implement special cases
   - Test: all edge types work

**Test Strategy for Pass 4:**

```elisp
(describe "Pass 4: Edge Drawing"
  (describe "Region Construction"
    (it "builds region for simple edge"
      (let ((edge (make-simple-edge)))
        (let ((region (dag-draw--build-edge-region edge graph)))
          (expect (length region) :to-be-greater-than 0))))
    (it "includes virtual node boxes"
      (let ((edge (make-long-edge-with-virtuals)))
        (let ((region (dag-draw--build-edge-region edge graph)))
          (expect (dag-draw--region-includes-virtuals-p region edge)
                  :to-be t)))))

  (describe "Spline Computation"
    (it "generates spline for straight edge"
      (let ((edge (make-straight-edge)))
        (dag-draw--compute-edge-spline edge graph)
        (expect (dag-draw-edge-control-points edge) :not :to-be nil)))
    (it "generates smooth spline for bent edge"
      (let ((edge (make-bent-edge)))
        (dag-draw--compute-edge-spline edge graph)
        (let ((cps (dag-draw-edge-control-points edge)))
          (expect (dag-draw--spline-is-smooth-p cps) :to-be t)))))

  (describe "Full Spline Pass"
    (it "computes splines for all edges"
      (let ((graph (make-complete-graph)))
        (dag-draw-make-splines graph)
        (expect (dag-draw--all-edges-have-splines-p graph) :to-be t)))))
```

---

### Phase 5: ASCII Rendering (Weeks 9-10)

**Goal:** Render graph as ASCII art

**Implementation Order:**

1. **Coordinate Scaling (Week 9, Day 1)**
   - Implement find_bounds()
   - Implement scale_coordinates()
   - Test: coordinates fit grid

2. **Grid Data Structure (Week 9, Day 1)**
   - Implement grid creation
   - Implement cell structure
   - Test: can set/get cells

3. **Node Rendering (Week 9, Day 2)**
   - Implement render_node()
   - Draw boxes with borders
   - Test: nodes appear correctly

4. **Edge Routing on Grid (Week 9, Day 3)**
   - Implement sample_spline()
   - Implement spline_to_grid_path()
   - Implement draw_segment()
   - Test: edges routed

5. **Junction Character Algorithm (Week 9, Day 4-5)**
   - Implement walk_edge_for_junctions()
   - Implement all junction types
   - Test: correct characters at junctions

6. **Arrow Placement (Week 10, Day 1)**
   - Implement place_arrows()
   - Test: arrows at correct positions

7. **Special Edge Types (Week 10, Day 2)**
   - Flat edges
   - Self-loops
   - Multi-edges
   - Test: all types render

8. **Grid to String (Week 10, Day 3)**
   - Implement grid_to_string()
   - Test: produces valid string

**Test Strategy for ASCII Rendering:**

```elisp
(describe "ASCII Rendering"
  (describe "Coordinate Scaling"
    (it "scales to fit grid"
      (let ((graph (make-large-graph)))
        (dag-draw--scale-coordinates graph 80 40)
        (expect (dag-draw--all-coords-in-bounds-p graph 80 40) :to-be t))))

  (describe "Node Rendering"
    (it "renders node box"
      (let ((grid (make-grid 80 40))
            (node (make-node "Test" 10 5)))
        (dag-draw--render-node node grid)
        (expect (dag-draw--grid-has-box-p grid 10 5) :to-be t))))

  (describe "Edge Routing"
    (it "routes simple vertical edge"
      (let ((grid (make-grid 80 40))
            (edge (make-vertical-edge)))
        (dag-draw--route-edge edge grid)
        (expect (dag-draw--grid-has-vertical-line-p grid) :to-be t))))

  (describe "Junction Characters"
    (it "places corner at direction change"
      (let ((grid (make-grid 80 40))
            (edge (make-corner-edge)))
        (dag-draw--route-edge edge grid)
        (dag-draw--apply-junctions graph grid)
        (expect (dag-draw--grid-has-corner-p grid) :to-be t)))
    (it "places cross at crossing"
      (let ((grid (make-grid 80 40))
            (edges (make-crossing-edges)))
        (dolist (edge edges)
          (dag-draw--route-edge edge grid))
        (dag-draw--apply-junctions graph grid)
        (expect (dag-draw--grid-has-cross-p grid) :to-be t))))

  (describe "Full Rendering"
    (it "renders complete graph"
      (let ((graph (make-test-graph)))
        (dag-draw-rank graph)
        (dag-draw-ordering graph)
        (dag-draw-position graph)
        (dag-draw-make-splines graph)
        (let ((ascii (dag-draw-render-ascii graph)))
          (expect ascii :not :to-be nil)
          (expect (stringp ascii) :to-be t)
          (expect (length ascii) :to-be-greater-than 0))))))
```

---

## GKNV Baseline vs Enhancements

### GKNV Baseline (Must Implement First)

These are core algorithm components from the paper that must be implemented for a working system:

#### Pass 1 - Ranking
- ✓ DFS cycle breaking
- ✓ Queue-based initial ranking
- ✓ Incremental tight tree construction
- ✓ Network simplex with cut values
- ✓ Postorder optimization (low/lim)
- ✓ Cyclic leave-edge search
- ✓ Rank normalization
- ✓ Greedy rank balancing

#### Pass 2 - Ordering
- ✓ Virtual node creation
- ✓ DFS initial ordering (forward & backward)
- ✓ Weighted median heuristic
- ✓ Transpose heuristic
- ✓ 24 iterations
- ✓ Flat edge handling
- ✓ Tie-breaking by flipping

#### Pass 3 - Positioning
- ✓ Y coordinate by rank
- ✓ Auxiliary graph construction
- ✓ Omega values (1, 2, 8)
- ✓ Rho separation formula
- ✓ Network simplex on aux graph
- ✓ Fast aux graph initialization
- ✓ X-offset port support

#### Pass 4 - Edge Drawing
- ✓ Sort edges by length
- ✓ Region construction (boxes)
- ✓ L segment computation
- ✓ Recursive path subdivision
- ✓ Bezier spline generation
- ✓ Spline fitting (straighten/refine)
- ✓ C1 continuity
- ✓ Multi-edge offset
- ✓ Flat edge handling
- ✓ Self-loop handling

#### ASCII Rendering
- ✓ Coordinate scaling
- ✓ Grid structure
- ✓ Node box rendering
- ✓ Spline sampling and routing
- ✓ Junction character rules
- ✓ Arrow placement

### Future Enhancements (Defer)

These can be added later to improve results:

#### Adaptive Strategies
- Adaptive iteration termination (vs fixed 24)
- Adaptive transpose iteration
- Adaptive spline refinement iterations

#### Symmetry Improvements
- Zero-cut edge symmetry balancing (Pass 3)
- Global rank balancing optimization

#### Path Optimization
- Convex hull shortest path (vs recursive subdivision)
- Better flat edge label positioning

#### User Interface
- Interactive graph editing
- Incremental redrawing for animation
- Multiple output formats (SVG, PDF, etc.)

#### Advanced Features
- Clustered graphs (subgraphs)
- Port specification languages
- Edge bundling
- Node size optimization

---

## Implementation Phases Summary

| Phase | Component | Duration | Dependencies |
|-------|-----------|----------|--------------|
| 0 | Foundation | 1 week | None |
| 1 | Pass 1: Ranking | 2 weeks | Phase 0 |
| 2 | Pass 2: Ordering | 2 weeks | Phase 1 |
| 3 | Pass 3: Positioning | 2 weeks | Phase 2 |
| 4 | Pass 4: Edge Drawing | 1 week | Phase 3 |
| 5 | ASCII Rendering | 2 weeks | Phase 4 |
| **Total** | **Core Implementation** | **10 weeks** | |

### Post-Core Phases

| Phase | Component | Duration |
|-------|-----------|----------|
| 6 | Integration Testing | 1 week |
| 7 | Performance Optimization | 1 week |
| 8 | Documentation & Examples | 1 week |
| 9 | Bug Fixes & Refinement | 2 weeks |
| **Total** | **Polish & Release** | **5 weeks** |

### Enhancement Phases (Optional)

| Phase | Component | Duration |
|-------|-----------|----------|
| 10 | Adaptive Strategies | 2 weeks |
| 11 | Advanced Symmetry | 2 weeks |
| 12 | Alternative Output Formats | 3 weeks |
| 13 | Interactive Features | 4 weeks |

---

## Success Criteria

### Phase 1 Success (Ranking)
- [ ] All nodes have rank assigned
- [ ] Ranks respect delta constraints (verified)
- [ ] Rank assignment is optimal (minimal weighted length)
- [ ] Cycles broken and edges reversed correctly
- [ ] Min rank is 0
- [ ] Ranks balanced reasonably

### Phase 2 Success (Ordering)
- [ ] All nodes have order within rank
- [ ] Virtual nodes created for long edges
- [ ] Ordering reduces crossings significantly
- [ ] Flat edges maintain consistent direction
- [ ] Transpose improves ordering
- [ ] Two-phase ordering works

### Phase 3 Success (Positioning)
- [ ] All nodes have X,Y coordinates
- [ ] Separation constraints satisfied
- [ ] Positioning is optimal (minimal weighted horizontal length)
- [ ] Y coordinates follow ranks
- [ ] Node ports work correctly
- [ ] Virtual nodes positioned

### Phase 4 Success (Edge Drawing)
- [ ] All edges have control points
- [ ] Splines stay within regions
- [ ] Splines are smooth (C1 continuity)
- [ ] Multi-edges don't overlap
- [ ] Flat edges route correctly
- [ ] Self-loops work

### Phase 5 Success (ASCII Rendering)
- [ ] Graph renders as ASCII art
- [ ] Nodes appear as boxes with labels
- [ ] Edges route correctly
- [ ] Junction characters correct at all junction types
- [ ] Arrows indicate direction
- [ ] Output is visually clear
- [ ] All test cases render

### Final Success (Complete System)
- [ ] All phases complete
- [ ] All tests pass
- [ ] Performance acceptable (< 1 second for 50-node graphs)
- [ ] Code is documented
- [ ] Examples work
- [ ] README complete
- [ ] Ready for use

---

## Key Risks and Mitigations

### Risk: Network Simplex Complexity

**Mitigation:**
- Implement simple version first
- Add optimizations incrementally
- Test on small graphs first
- Reference paper's pseudocode closely

### Risk: ASCII Junction Characters

**Mitigation:**
- Start with simple cases (single edge, no junctions)
- Add junction types one at a time
- Visual inspection of output
- Create comprehensive test cases for each junction type

### Risk: Coordinate Scaling

**Mitigation:**
- Test scaling independently
- Verify bounds calculation
- Check for off-by-one errors
- Test with various grid sizes

### Risk: Spline Approximation Quality

**Mitigation:**
- Start with dense sampling (50+ points)
- Reduce sampling if performance issues
- Visual inspection of results
- Compare to expected output

---

## Development Workflow

### Daily Workflow

1. **Read** relevant section of GKNV paper
2. **Plan** component to implement (refer to this roadmap)
3. **Write** failing test (TDD)
4. **Implement** minimal code to pass test
5. **Refactor** if needed
6. **Verify** tests still pass
7. **Document** any decisions or deviations
8. **Commit** with clear message

### Weekly Workflow

1. **Review** progress against roadmap
2. **Update** roadmap if needed
3. **Integration test** if completing phase
4. **Demo** progress on visual examples
5. **Plan** next week's work

### Code Review Checklist

- [ ] Follows GKNV paper specification
- [ ] Has tests
- [ ] Tests pass
- [ ] Code is documented
- [ ] Function signatures match specification
- [ ] Data structures match specification
- [ ] No performance issues
- [ ] Edge cases handled

---

## Reference Documents

When implementing, always refer to:

1. **doc/technique-for-drawing-directed-graphs.asciidoc** - The GKNV paper (source of truth)
2. **doc/implementation-decisions.md** - All decision points and rationale
3. **doc/algorithm-specification.md** - Detailed pseudocode and data structures
4. **doc/ascii-rendering-strategy.md** - ASCII-specific details
5. **doc/gknv-to-ascii-mapping.md** - Code to algorithm mapping (update as you build)

---

## Questions During Implementation

If you encounter ambiguity:

1. **Check implementation-decisions.md** - May already be addressed
2. **Check GKNV paper** - Read relevant section carefully
3. **Check examples** - What do figures 1-2, 1-3, etc. show?
4. **Document the question** - Add to "Remaining Ambiguities" section
5. **Make reasonable decision** - Document it clearly
6. **Move forward** - Can be refined later

---

**Document Version**: 1.0
**Date**: 2025-10-13
**Author**: dag-draw.el development team
