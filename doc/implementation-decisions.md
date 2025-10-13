# Implementation Decisions for dag-draw.el

## Overview

This document records all major decision points identified in the GKNV paper "A Technique for Drawing Directed Graphs" and the specific choices made for the dag-draw.el implementation. Each decision is documented with:
- The decision point from the paper
- The choice made for this implementation
- Full rationale with paper references
- Whether this is GKNV baseline or a proposed enhancement

## Table of Contents

1. [Pass 1: Ranking Decisions](#pass-1-ranking-decisions)
2. [Pass 2: Ordering Decisions](#pass-2-ordering-decisions)
3. [Pass 3: Positioning Decisions](#pass-3-positioning-decisions)
4. [Pass 4: Spline/Edge Drawing Decisions](#pass-4-splineedge-drawing-decisions)
5. [ASCII Rendering Decisions](#ascii-rendering-decisions)
6. [Decision Summary Table](#decision-summary-table)

---

## Pass 1: Ranking Decisions

### D1.1: Cycle Breaking Strategy

**Paper Options:**
- Minimal feedback arc set (NP-complete)
- Heuristic counting cycles and reversing maximal edge
- Depth-first search with back edge reversal

**Decision: Depth-first search with back edge reversal**

**Rationale:**
GKNV Section 2.1 states: "From the standpoint of stability, the depth-first cycle-breaking heuristic seems preferable. It also makes more informative drawings than would be obtained by collapsing all the nodes in a cycle into one node."

The paper explicitly tested both heuristics and found DFS superior for:
- Stability (respects natural input order)
- Preserving semantic structure (top-level functions stay at top)
- Simplicity of implementation

**Type: GKNV Baseline**

---

### D1.2: Network Simplex Initial Ranking Method

**Paper Options:**
- Simple topological sort (minimal elements at rank 0)
- BFS-based ranking
- DFS-based ranking

**Decision: Queue-based topological ranking (GKNV init_rank)**

**Rationale:**
GKNV Section 2.3, Figure 2-2, line 2: "Our version keeps nodes in a queue. Nodes are placed in the queue when they have no unscanned in-edges. As nodes are taken off the queue, they are assigned the least rank that satisfies their in-edges."

This is essentially a topological sort that respects minimum edge lengths δ(e).

**Type: GKNV Baseline**

---

### D1.3: Initial Feasible Tree Construction

**Paper Options:**
- Start with any spanning tree and adjust until feasible
- Incremental tight tree construction (GKNV method)
- Start with all edges and prune

**Decision: Incremental tight tree construction**

**Rationale:**
GKNV Section 2.3, Figure 2-2: The feasible_tree() procedure builds a maximal tight tree incrementally by finding edges with minimal slack and adjusting ranks to make them tight.

Lines 3-9: "while tight_tree() < V do... finds an edge to a non-tree node that is adjacent to the tree, and adjusts the ranks of the tree nodes to make this edge tight."

**Type: GKNV Baseline**

---

### D1.4: Leave Edge Selection Strategy

**Paper Options:**
- Any edge with negative cut value
- Most negative cut value first
- Cyclic search through edges

**Decision: Cyclic search through tree edges**

**Rationale:**
GKNV Section 2.4, end: "The network simplex is also very sensitive to the choice of the negative edge to replace. We observed that searching cyclically through all the tree edges, instead of searching from the beginning of the list of tree edges every time, can save many iterations."

**Type: GKNV Baseline (optimization)**

---

### D1.5: Edge Weight Interpretation

**Paper Options:**
- All edges weight 1
- User-specified weights
- Automatically computed based on edge type

**Decision: User-specified weights with default 1**

**Rationale:**
GKNV Section 1.2: "ω(e) Weight of an edge e, usually 1. The weight signifies the edge's importance, which translates to keeping the edge short and vertically aligned."

**Type: GKNV Baseline**

---

### D1.6: Rank Normalization

**Paper Options:**
- Leave ranks as computed
- Set minimum rank to 0
- Center ranks around 0

**Decision: Set minimum rank to 0**

**Rationale:**
GKNV Section 2.3, Figure 2-1, line 7: "normalize(); The solution is normalized by setting the least rank to zero."

**Type: GKNV Baseline**

---

### D1.7: Rank Balancing

**Paper Options:**
- No balancing
- Greedy balancing (GKNV)
- Global optimal balancing

**Decision: Greedy balancing**

**Rationale:**
GKNV Section 2.3, Figure 2-1, line 8: "balance(); Nodes having equal in- and out-edge weights and multiple feasible ranks are moved to a feasible rank with the fewest nodes. The purpose is to reduce crowding and improve the aspect ratio of the drawing, following principle A4. Nodes are adjusted in a greedy fashion, which works sufficiently well."

**Type: GKNV Baseline**

---

### D1.8: Cut Value Computation

**Paper Options:**
- Naive O(VE) per edge
- Incremental from leaves inward O(E)
- Postorder with low/lim optimization

**Decision: Incremental from leaves with postorder optimization**

**Rationale:**
GKNV Section 2.4: Both optimizations are described:
1. "Computing cut values incrementally... By thus computing cut values incrementally, we can ensure that every edge is examined only twice."
2. "Another valuable optimization... is to perform a postorder traversal of the tree, starting from some fixed root node v_root, and labeling each node v with its postorder traversal number lim(v), the least number low(v) of any descendant in the search."

Both are crucial for performance on large graphs.

**Type: GKNV Baseline (optimization)**

---

### D1.9: Handling S_min, S_max, S_k Constraints

**Paper Options:**
- Ignore constraints
- Merge nodes temporarily
- Add constraint edges

**Decision: Merge nodes temporarily and add constraint edges**

**Rationale:**
GKNV Section 2: "For this pass, each of the nonempty sets S_max, S_min, S_0, ..., S_k is temporarily merged into one node."

GKNV Section 2.1: "Also, for all nodes v with no in-edge, we make a temporary edge (S_min, v) with δ = 0, and for all nodes v with no out-edge, we make a temporary edge (v, S_max) with δ = 0."

**Type: GKNV Baseline**

---

### D1.10: Delta (Minimum Length) Handling

**Paper Options:**
- All edges δ = 1
- User-specified δ per edge
- Internally computed δ for labels

**Decision: Support both user-specified and internally computed δ**

**Rationale:**
GKNV Section 2: "δ(e) is usually 1, but can take any non-negative integer value. δ(e) may be set internally for technical reasons as described below, or externally if the user wants to adjust the rank assignment."

Later sections describe setting δ = 2 for edges with labels.

**Type: GKNV Baseline**

---

## Pass 2: Ordering Decisions

### D2.1: Initial Ordering Method

**Paper Options:**
- Random
- Depth-first search from sources
- Breadth-first search from sources
- Two-phase (DFS from sources + DFS from sinks)

**Decision: Two-phase approach (both forward and backward DFS)**

**Rationale:**
GKNV Section 3, end: "One final point is that it is generally worth the extra cost to run the vertex ordering algorithm twice: once for an initial order determined by starting with vertices of minimal rank and searching out-edges, and the second time by starting with vertices of maximal rank and searching in-edges. This allows one to pick the better of two different solutions."

**Type: GKNV Baseline**

---

### D2.2: Median Calculation for Even Number of Adjacent Nodes

**Paper Options:**
- Left median only
- Right median only
- Average of two medians
- Weighted interpolation based on spacing

**Decision: Weighted interpolation based on spacing**

**Rationale:**
GKNV Section 3, Figure 3-2, lines 21-24:
```
left = P[m-1] - P[0];
right = P[P-1] - P[m];
return (P[m-1]*right + P[m]*left)/(left+right);
```

"Generally, the weighted median is biased toward the side where vertices are more closely packed."

**Type: GKNV Baseline (innovation)**

---

### D2.3: Maximum Iterations for Ordering

**Paper Options:**
- Fixed number (GKNV uses 24)
- Adaptive based on improvement
- Continue until no improvement

**Decision: Fixed at 24 with adaptive consideration**

**Rationale:**
GKNV Section 3, Figure 3-1, line 4: "We set Max_iterations to 24."

However, the paper notes: "In an actual implementation, one might prefer an adaptive strategy that iterates as long as the solution has improved at least a few percent over the last several iterations."

**Type: GKNV Baseline (with enhancement noted)**

---

### D2.4: Transpose Heuristic

**Paper Options:**
- No transpose (median only)
- Single pass transpose
- Iterative transpose until local optimum

**Decision: Iterative transpose until local optimum**

**Rationale:**
GKNV Section 3, Figure 3-3: The transpose procedure iterates "as long as the number of edge crossings can be reduced by transpositions."

This innovation provides "an additional 20-50% reduction in edge crossings" (Section 3).

**Type: GKNV Baseline (major innovation)**

---

### D2.5: Handling Nodes with No Adjacent Vertices

**Paper Options:**
- Place at left
- Place at right
- Keep in current position
- Distribute evenly

**Decision: Keep in current position**

**Rationale:**
GKNV Section 3, Figure 3-2, lines 15-16: "Nodes with no adjacent vertices are given a median value of -1. This is used within the sort function to indicate that these nodes should be left in their current positions."

**Type: GKNV Baseline**

---

### D2.6: Flat Edges (Same Rank) Ordering

**Paper Options:**
- Ignore flat edges in ordering
- Compute transitive closure and respect partial order
- Allow arbitrary ordering

**Decision: Compute transitive closure and respect partial order**

**Rationale:**
GKNV Section 3, end: "If there are flat edges, their transitive closure is computed before finding the vertex order. The vertex order must always embed this partial order. In particular, the initial order must be consistent with it, and the transpose and the sort routines must not exchange nodes against the partial order."

**Type: GKNV Baseline**

---

### D2.7: Handling Ties (Equal Medians/Crossings)

**Paper Options:**
- Stable sort (maintain order)
- Arbitrary tie-breaking
- Flip on alternating iterations

**Decision: Flip on alternating iterations**

**Rationale:**
GKNV Section 3, end: "We have found it helpful, and in keeping with the spirit of A4, to flip nodes with equal values during the sorting or transposing passes on every other forward and backward traversal."

This improves symmetry (aesthetic A4).

**Type: GKNV Baseline**

---

### D2.8: Virtual Node Creation Timing

**Paper Options:**
- Create before ordering
- Create after ordering
- Don't create (handle long edges differently)

**Decision: Create before ordering**

**Rationale:**
GKNV Section 3, start: "After rank assignment, edges between nodes more than one rank apart are replaced by chains of unit length edges between temporary or 'virtual' nodes. The virtual nodes are placed on the intermediate ranks, converting the original graph into one whose edges connect only nodes on adjacent ranks."

**Type: GKNV Baseline**

---

### D2.9: Self-Loop and Multi-Edge Handling in Ordering

**Paper Options:**
- Include in ordering
- Ignore in ordering
- Merge multi-edges

**Decision: Ignore self-loops, merge multi-edges**

**Rationale:**
GKNV Section 3, start: "Self-edges are ignored in this pass, and multi-edges are merged as in the previous pass."

**Type: GKNV Baseline**

---

## Pass 3: Positioning Decisions

### D3.1: Coordinate Assignment Method

**Paper Options:**
- Heuristic iterative approach (Section 4.1)
- Network simplex on auxiliary graph (Section 4.2)
- Linear programming with simplex

**Decision: Network simplex on auxiliary graph**

**Rationale:**
GKNV Section 4.2: The authors initially implemented heuristics but found them "complicated to program and the results are sometimes noticeably imperfect." The network simplex approach is "much simpler code and produces optimal solutions."

Section 4.2, end: "With these improvements, our implementation runs as fast or faster than the heuristic implementation."

**Type: GKNV Baseline (authors' preferred method)**

---

### D3.2: Omega (Internal Edge Weight) Values

**Paper Options:**
- All edges same weight
- Different weights by edge type
- GKNV values: 1, 2, 8

**Decision: GKNV values (1, 2, 8)**

**Rationale:**
GKNV Section 4: "If e, f, and g are edges of types (1), (2), and (3), respectively, then Ω(e) ≤ Ω(f) ≤ Ω(g). Our implementation uses 1, 2, and 8."

Where types are:
1. Both real nodes
2. One real, one virtual
3. Both virtual nodes

This favors straightening long edges (type 3).

**Type: GKNV Baseline**

---

### D3.3: Node Separation Function ρ

**Paper Options:**
- Fixed separation
- Based on node sizes
- GKNV formula

**Decision: GKNV formula**

**Rationale:**
GKNV Section 4: "ρ(a,b) = (xsize(a) + xsize(b))/2 + nodesep(G)"

This accounts for both node box sizes and desired separation.

**Type: GKNV Baseline**

---

### D3.4: Initial Feasible Tree for Auxiliary Graph

**Paper Options:**
- Use general network simplex initialization
- Exploit auxiliary graph structure (GKNV optimization)

**Decision: Exploit auxiliary graph structure**

**Rationale:**
GKNV Section 4.3: "Further improvement is possible by noting that it is easy to construct an initial feasible tree for the auxiliary graph by taking advantage of its structure. To construct a feasible tree, use all edges connecting nodes in the same rank. For each pair of adjacent ranks, pick an edge f = (u,v) between the ranks and add both f_u and f_v in G' to the tree."

This optimization is crucial: "Without these improvements, using network simplex to position the nodes took 5 to 10 times longer."

**Type: GKNV Baseline (critical optimization)**

---

### D3.5: Node Port Support

**Paper Options:**
- Node center only
- X-offset ports (GKNV)
- Full 2D offset ports

**Decision: X-offset ports**

**Rationale:**
GKNV Section 4.2: Extensive discussion of node ports with X-offsets, Figure 4-3 example, and Figure 4-4 showing the δ calculation for ports.

"Using the auxiliary graph also permits the specification of 'node ports,' or edge endpoints offset in the X direction from the center of the node."

**Type: GKNV Baseline**

---

### D3.6: Y Coordinate Assignment

**Paper Options:**
- Fixed ranksep
- Variable ranksep based on edge slopes
- Optimization problem

**Decision: Fixed ranksep with optional adjustment**

**Rationale:**
GKNV Section 4, start: "The Y coordinate assignment maintains the minimum separation ranksep(G) between node boxes. Optionally, the separation between adjacent ranks can be increased to improve the slope of nearly horizontal edges to make them more readable."

**Type: GKNV Baseline**

---

### D3.7: Symmetry Improvement via Zero-Cut Edges

**Paper Options:**
- No post-processing
- Scan for zero-cut edges and balance slack
- Global symmetry optimization

**Decision: Scan for zero-cut edges and balance slack**

**Rationale:**
GKNV Section 4.3, end: "Tree edges whose cut value is exactly 0 identify subgraphs that may be adjusted to equalize the slack on their incident edges without changing the cost of the solution. This could be used to increase symmetry, such as centering a node with an even number of descendants."

**Type: GKNV Enhancement (mentioned but optional)**

---

## Pass 4: Spline/Edge Drawing Decisions

### D4.1: Edge Drawing Method

**Paper Options:**
- Line segments between nodes
- Line segments through virtual nodes
- Heuristic splines (dag)
- Region-constrained optimal splines (dot)

**Decision: Region-constrained optimal splines**

**Rationale:**
GKNV Section 5: The authors describe their evolution from simple lines to heuristic splines to the region-constrained approach: "It is better to try to find the smoothest curve between two points that avoids the 'obstacles' of other nodes or splines."

For ASCII, this will be adapted to character grid routing.

**Type: GKNV Baseline (but requires ASCII adaptation)**

---

### D4.2: Spline Order

**Paper Options:**
- Draw in edge definition order
- Shortest edges first
- Longest edges first
- Random order

**Decision: Shortest edges first**

**Rationale:**
GKNV Section 5.1.1: "Because splines are drawn by a 'greedy' strategy, they depend on the order in which they are computed. It seems reasonable to route the shorter splines first because they can often be drawn as straight lines, but the order does not seem to affect the drawing quality much."

**Type: GKNV Baseline**

---

### D4.3: Handling Nearly Vertical Edge Sections

**Paper Options:**
- Draw as spline throughout
- Convert to straight vertical lines
- Hybrid approach

**Decision: Hybrid approach (vertical lines for nearly vertical sections)**

**Rationale:**
GKNV Section 5.1.1: "Second, when an edge has a section that is almost vertical, it looks better to just draw it as a vertical line. This is most obvious when edges run alongside each other, because parallel line segments look better than long segments with slightly different slopes."

**Type: GKNV Baseline**

---

### D4.4: Avoiding Accidental Intersections at Terminal Nodes

**Paper Options:**
- No special handling
- Subdivide inter-rank space near terminals
- Global intersection detection

**Decision: Subdivide inter-rank space near terminals**

**Rationale:**
GKNV Section 5.1.1: "Third, when several splines approach a common termination point, it is important to avoid 'accidental' intersections. To do this, we check if there are previously computed splines with the same endpoint. If so, we find the closest ones to the right and the left. We then subdivide the inter-rank space..."

**Type: GKNV Baseline**

---

### D4.5: Multi-Edge Spacing

**Paper Options:**
- Draw on top of each other
- Fixed offset between edges
- Offset by nodesep multiple

**Decision: Offset by nodesep multiple**

**Rationale:**
GKNV Section 5.1.1: "When these exist, a spline is computed for one of the edges, and the rest of the edges are drawn by adding an increasing X coordinate displacement to each one (multiples of nodesep(G) work well)."

**Type: GKNV Baseline**

---

### D4.6: Flat Edge Drawing

**Paper Options:**
- Same as inter-rank edges
- Special formula for adjacent nodes
- Spline with Y displacement for multi-edges

**Decision: Special formula for adjacent nodes**

**Rationale:**
GKNV Section 5.1.2: For adjacent flat edges, specific control point formula is given. For multiple flat edges, "succeeding edges are drawn by adding Y coordinate displacements."

**Type: GKNV Baseline**

---

### D4.7: Self-Loop Drawing

**Paper Options:**
- Don't support
- Single fixed-size loop
- Size based on nodesep and ysize
- Support port specification

**Decision: Size based on nodesep and ysize, with port support**

**Rationale:**
GKNV Section 5.1.3: Detailed formulas given for self-loop control points using nodesep(G) and ysize(v)/2. "If an edge specifies tail or head ports, a polygonal region is generated that connects the two ports."

**Type: GKNV Baseline**

---

### D4.8: Region Box Handling for Crossing Edges

**Paper Options:**
- Strict box boundaries
- Ignore virtual nodes for nearby crossings
- Complex polygon regions

**Decision: Ignore virtual nodes for nearby crossings (within 2 ranks)**

**Rationale:**
GKNV Section 5.1.1: "When edges cross, they should not constrain each other too much. Otherwise, a spline may have an awkward, sharp turn. This is easily avoided by making an adjustment to the boxes. When setting the size of a box, we ignore virtual nodes to the left or right that correspond to edges that cross within two ranks."

**Type: GKNV Baseline**

---

### D4.9: Spline Path Computation

**Paper Options:**
- Straight line if possible
- Recursive subdivision at furthest L segment
- Shortest path using convex hulls

**Decision: Recursive subdivision at furthest L segment**

**Rationale:**
GKNV Section 5.2, Figure 5-3: The compute_p_array procedure uses recursive subdivision. "If the (q,s) line does not fit, compute_linesplit finds the L segment that is the furthest from the (q,s) line and subdivides B_array and L_array along that segment."

The paper notes shortest path "could be found in linear time using convex hulls [Su]" but doesn't implement it.

**Type: GKNV Baseline**

---

### D4.10: Spline Fitting Strategy

**Paper Options:**
- Force spline to fit by subdivision only
- Straighten spline if it doesn't fit
- Refine spline by adjusting curvature
- Hybrid approach

**Decision: Hybrid approach**

**Rationale:**
GKNV Section 5.2, Figure 5-3:
- Lines 11-13: For single-segment paths, straighten spline iteratively
- Lines 14-30: For multi-segment paths, try refining curvature, then subdivide if needed

**Type: GKNV Baseline**

---

### D4.11: Spline Continuity

**Paper Options:**
- C0 (position only)
- C1 (continuous tangent)
- C2 (continuous curvature)

**Decision: C1 (continuous tangent)**

**Rationale:**
GKNV Section 5.2, lines 25-26: "To force the two curves to join smoothly at the subdivision point, we also force the two splines to have the same unit tangent vector at that point. This guaranties C1 continuity at the subdivision point."

The paper explicitly rejects C2: "Forcing C2 continuity does not seem to produce better results and is also much more expensive to compute."

**Type: GKNV Baseline**

---

### D4.12: Edge Label Placement

**Paper Options:**
- Next to midpoint (dag)
- As off-center virtual nodes (dot)
- Sophisticated map-style placement

**Decision: As off-center virtual nodes**

**Rationale:**
GKNV Section 5.3: "In dot, edge labels on inter-rank edges are represented as off-center virtual nodes. This guarantees that labels never overlap other nodes, edges or labels. Certain adjustments are needed to make sure that adding labels does not affect the length of edges. Setting the minimum edge length to 2 (effectively doubling the ranks when virtual nodes are created) and halving the separation between ranks compensates for the label nodes."

**Type: GKNV Baseline (dot version)**

---

## ASCII Rendering Decisions

These decisions are specific to adapting the GKNV algorithm for ASCII output, as the paper describes graphical (PostScript/graphics) output.

### D5.1: Coordinate Scaling Strategy

**Paper Options:**
- Direct mapping (1 unit = 1 character)
- Scale to preserve aspect ratio
- Scale X and Y independently

**Decision: Scale X and Y independently based on content**

**Rationale:**
ASCII characters have fixed aspect ratio (~2:1 width:height). We need to:
1. Scale X coordinates to character grid (accounting for node widths)
2. Scale Y coordinates to character grid (accounting for node heights)
3. Maintain minimum separation in character units

**Type: ASCII-specific enhancement**

---

### D5.2: Box-Drawing Character Selection

**Paper Options:**
- ASCII only (|, -, +)
- Unicode box-drawing characters
- Unicode with ASCII fallback

**Decision: Unicode box-drawing characters**

**Rationale:**
Modern terminals support Unicode. Box-drawing characters (U+2500 block) provide:
- Clear visual distinction between edges and node boundaries
- Professional appearance
- Unambiguous junction points

**Type: ASCII-specific enhancement**

---

### D5.3: Edge Routing on Character Grid

**Paper Options:**
- Convert splines to polylines
- A* pathfinding on grid
- Orthogonal routing (Manhattan paths)
- Hybrid spline approximation

**Decision: Hybrid spline approximation with orthogonal segments**

**Rationale:**
1. Sample spline at regular intervals
2. Convert to character grid coordinates
3. Connect points with orthogonal (horizontal/vertical) segments
4. Apply junction character rules at all intersections/turns

This approximates the smooth splines while respecting grid constraints.

**Type: ASCII-specific enhancement**

---

### D5.4: Junction Character Algorithm

**Paper Options:**
- Fixed characters at all intersections
- Context-aware selection
- Walk-based local analysis

**Decision: Walk-based local analysis**

**Rationale:**
As documented in CLAUDE.md, walk each edge to determine:
- Starting port junction (where edge leaves node)
- Direction change junctions (corners)
- Merge/split junctions (where edges share segments)
- Cross junctions (where edges cross)
- Ending port junction (where edge enters node)

**Type: ASCII-specific enhancement**

---

### D5.5: Arrow Placement

**Paper Options:**
- At port boundary
- One character before port
- Adjacent to node box

**Decision: At port boundary (adjacent to node box)**

**Rationale:**
Arrows should clearly indicate direction while being visually connected to destination node. Place arrow at the character position where edge meets node boundary.

Available arrows: ▲ ▼ ◀ ▶ (Unicode) or ^ v < > (ASCII)

**Type: ASCII-specific enhancement**

---

### D5.6: Node Rendering

**Paper Options:**
- Fixed-size boxes
- Size based on label length
- Minimum size with padding

**Decision: Size based on label length with minimum size and padding**

**Rationale:**
1. Calculate label length in characters
2. Add padding (1 char left + 1 char right minimum)
3. Enforce minimum width (e.g., 5 characters)
4. Height based on ranks (usually 3 characters: top border, label, bottom border)

**Type: ASCII-specific enhancement**

---

### D5.7: Virtual Node Representation

**Paper Options:**
- Invisible (just route through)
- Show as dots
- Show as junction characters
- Size based on routing needs

**Decision: Size based on routing needs, render as junctions**

**Rationale:**
Virtual nodes are routing points for long edges. They should:
- Take minimal space (typically 1x1 characters)
- Be expandable if multiple edges route through them
- Render as appropriate junction characters based on routing

**Type: ASCII-specific enhancement**

---

### D5.8: Handling Dense Regions

**Paper Options:**
- Allow overlap
- Force minimum separation
- Increase scale in dense regions
- Accept spacing violations

**Decision: Force minimum separation, accept some compression**

**Rationale:**
ASCII has hard grid constraints. When regions are dense:
1. Maintain minimum 1-character separation between elements
2. Allow node boxes to compress slightly if needed
3. Prefer clarity over perfect spacing
4. Trust network simplex to minimize issues

**Type: ASCII-specific enhancement**

---

### D5.9: Multi-Edge Rendering in ASCII

**Paper Options:**
- Draw only one edge
- Stack edges with spacing
- Use double-line characters
- Label edges numerically

**Decision: Stack edges with 1-character spacing**

**Rationale:**
For inter-rank multi-edges, route parallel paths with minimum 1-character separation. For flat multi-edges, stack vertically with Y offsets.

**Type: ASCII-specific enhancement**

---

### D5.10: Self-Loop Rendering in ASCII

**Paper Options:**
- Don't support
- Fixed ASCII pattern
- Sized to fit nodesep

**Decision: Fixed ASCII patterns for common loop positions**

**Rationale:**
Define standard patterns for loops on each side:
- Right side: Use ┐, │, ┘, ─ characters
- Left side: Mirror pattern
- Top/bottom: Horizontal patterns

**Type: ASCII-specific enhancement**

---

## Decision Summary Table

| ID | Decision Point | Choice | Type | Section |
|----|---------------|--------|------|---------|
| D1.1 | Cycle breaking | DFS back-edge reversal | GKNV | 2.1 |
| D1.2 | Initial ranking | Queue-based topological | GKNV | 2.3 |
| D1.3 | Feasible tree | Incremental tight tree | GKNV | 2.3 |
| D1.4 | Leave edge | Cyclic search | GKNV opt | 2.4 |
| D1.5 | Edge weights | User-specified, default 1 | GKNV | 1.2 |
| D1.6 | Normalization | Min rank to 0 | GKNV | 2.3 |
| D1.7 | Balancing | Greedy balancing | GKNV | 2.3 |
| D1.8 | Cut values | Incremental + postorder | GKNV opt | 2.4 |
| D1.9 | Rank constraints | Merge + constraint edges | GKNV | 2, 2.1 |
| D1.10 | Delta handling | User + internal | GKNV | 2 |
| D2.1 | Initial order | Two-phase DFS | GKNV | 3 |
| D2.2 | Median calc | Weighted interpolation | GKNV innov | 3 |
| D2.3 | Max iterations | 24 fixed (adaptive possible) | GKNV | 3 |
| D2.4 | Transpose | Iterative to local optimum | GKNV innov | 3 |
| D2.5 | No-adjacent nodes | Keep current position | GKNV | 3 |
| D2.6 | Flat edges | Transitive closure + order | GKNV | 3 |
| D2.7 | Ties | Flip alternate iterations | GKNV | 3 |
| D2.8 | Virtual nodes | Create before ordering | GKNV | 3 |
| D2.9 | Self/multi edges | Ignore/merge | GKNV | 3 |
| D3.1 | Positioning | Network simplex + aux graph | GKNV pref | 4.2 |
| D3.2 | Omega values | 1, 2, 8 for edge types | GKNV | 4 |
| D3.3 | Node separation | (xsize_a + xsize_b)/2 + sep | GKNV | 4 |
| D3.4 | Aux tree init | Exploit structure | GKNV opt | 4.3 |
| D3.5 | Node ports | X-offset ports | GKNV | 4.2 |
| D3.6 | Y coordinates | Fixed ranksep + optional adj | GKNV | 4 |
| D3.7 | Symmetry | Zero-cut edge balancing | GKNV enh | 4.3 |
| D4.1 | Edge drawing | Region-constrained splines | GKNV | 5 |
| D4.2 | Spline order | Shortest first | GKNV | 5.1.1 |
| D4.3 | Vertical sections | Hybrid with straight lines | GKNV | 5.1.1 |
| D4.4 | Terminal intersect | Subdivide inter-rank space | GKNV | 5.1.1 |
| D4.5 | Multi-edges | Offset by nodesep multiple | GKNV | 5.1.1 |
| D4.6 | Flat edges | Special formula + Y displace | GKNV | 5.1.2 |
| D4.7 | Self-loops | Size by nodesep/ysize + ports | GKNV | 5.1.3 |
| D4.8 | Cross boxes | Ignore nearby (2 ranks) | GKNV | 5.1.1 |
| D4.9 | Spline path | Recursive at furthest L | GKNV | 5.2 |
| D4.10 | Spline fitting | Hybrid straighten/refine | GKNV | 5.2 |
| D4.11 | Continuity | C1 (tangent) | GKNV | 5.2 |
| D4.12 | Labels | Off-center virtual nodes | GKNV dot | 5.3 |
| D5.1 | Coord scaling | Independent X/Y scale | ASCII | N/A |
| D5.2 | Characters | Unicode box-drawing | ASCII | N/A |
| D5.3 | Edge routing | Hybrid spline approx | ASCII | N/A |
| D5.4 | Junctions | Walk-based analysis | ASCII | N/A |
| D5.5 | Arrows | At port boundary | ASCII | N/A |
| D5.6 | Node render | Label size + padding | ASCII | N/A |
| D5.7 | Virtual render | Size for routing + junctions | ASCII | N/A |
| D5.8 | Dense regions | Force min sep, accept compress | ASCII | N/A |
| D5.9 | Multi-edge ASCII | Stack with 1-char spacing | ASCII | N/A |
| D5.10 | Loop ASCII | Fixed patterns by side | ASCII | N/A |

---

## Classification Summary

**GKNV Baseline**: 29 decisions
- These follow the paper's explicit recommendations and examples

**GKNV Optimizations**: 3 decisions
- These are performance improvements described in the paper as crucial

**GKNV Innovations**: 2 decisions
- These are novel contributions highlighted by the authors (weighted median, transpose)

**GKNV Enhancements**: 1 decision
- Optional improvements mentioned but not required (zero-cut symmetry)

**GKNV Preferred**: 1 decision
- Where authors tested multiple approaches and stated preference (aux graph vs heuristic)

**ASCII-Specific**: 10 decisions
- Required adaptations for character-grid rendering not in paper

---

## Remaining Ambiguities Requiring Human Decision

### A1: Adaptive vs Fixed Iteration Counts

**Context**: Section 3 mentions both fixed (24) and adaptive iteration strategies.

**Question**: Should we implement adaptive termination or stick with fixed?

**Recommendation**: Start with fixed 24, add adaptive as future enhancement.

---

### A2: Zero-Cut Symmetry Enhancement

**Context**: Section 4.3 mentions this but doesn't provide implementation details.

**Question**: Should this be implemented initially?

**Recommendation**: Defer to future enhancement after baseline works.

---

### A3: Convex Hull Shortest Path

**Context**: Section 5.2 mentions linear-time shortest path using convex hulls.

**Question**: Should we implement this optimization?

**Recommendation**: Defer. The subdivision approach works well per the paper.

---

### A4: ASCII Grid Resolution

**Context**: Not in paper - ASCII specific.

**Question**: What resolution should we use for coordinate-to-grid conversion?

**Recommendation**:
- Default: 1 X-unit = 2 characters, 1 Y-unit = 1 character (rough aspect ratio match)
- Make configurable for experimentation

---

### A5: Flat Edge Label Positioning

**Context**: Section 5.3 states "At present we are still working on this problem."

**Question**: How should we handle flat edge labels in ASCII?

**Recommendation**: Place label at midpoint of edge, allow overlap initially. Document as known issue.

---

## References

All section numbers refer to: Gansner, E. R., Koutsofios, E., North, S. C., & Vo, K. P. (1993). "A Technique for Drawing Directed Graphs." IEEE Transactions on Software Engineering.

---

**Document Version**: 1.0
**Date**: 2025-10-13
**Author**: dag-draw.el development team
