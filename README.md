# dag-draw.el

**Drawing graphs that don't suck.**

## The Problem

Ever tried to draw a flowchart? You spend hours moving boxes around, trying to untangle crossing lines, and the result still looks like spaghetti. The same thing happens when visualizing:

- Project dependencies ("Task A must finish before B and C")
- Course prerequisites ("You need Calc I before Calc II")
- File dependencies ("main.c includes utils.h")
- Workflow steps ("Design → Build → Test → Deploy")

**The manual approach is painful.** You fiddle with positions, redraw arrows, realize you need to move everything down to make room... and start over.

## The Solution

dag-draw.el does the hard part for you. You focus on the **structure** (what connects to what), and it handles the **layout** (where everything goes).

```elisp
;; You write this:
(dag-draw-add-node graph 'design "Design")
(dag-draw-add-node graph 'build "Build")
(dag-draw-add-node graph 'test "Test")
(dag-draw-add-edge graph 'design 'build)
(dag-draw-add-edge graph 'design 'test)
(dag-draw-add-edge graph 'build 'test)

;; It produces this:
```
```
┌──────┐
│Design│
└───┬──┘
    │
    ├────────────────┐
    │                │
    ▼                ▼
┌───────┐        ┌──────┐
│Build  │───────▶│Test  │
└───────┘        └──────┘
```

**No manual positioning. No tangled arrows. Just clean, hierarchical layouts.**

## Your First Graph (30 Seconds)

Let's draw something simple—a three-step workflow:

```elisp
(require 'dag-draw)

;; 1. Create a graph
(setq my-graph (dag-draw-create-graph))

;; 2. Add your nodes
(dag-draw-add-node my-graph 'start "Start Here")
(dag-draw-add-node my-graph 'middle "Do Work")
(dag-draw-add-node my-graph 'done "Finish")

;; 3. Connect them
(dag-draw-add-edge my-graph 'start 'middle)
(dag-draw-add-edge my-graph 'middle 'done)

;; 4. Layout and render
(dag-draw-layout-graph my-graph)
(dag-draw-render-graph my-graph 'ascii)
```

**Output:**
```
┌────────────┐
│Start Here  │
└──────┬─────┘
       │
       ▼
┌────────────┐
│Do Work     │
└──────┬─────┘
       │
       ▼
┌────────────┐
│Finish      │
└────────────┘
```

**Congratulations!** You just created a professionally-laid-out graph. You specified the structure; dag-draw handled everything else.

## Understanding Graph Layout

### What's a Directed Graph?

Think of a **family tree**, but where arrows show direction. Or a **course prerequisite chart** where arrows point from required courses to courses that need them.

More formally:
- **Nodes** (boxes) represent things: tasks, people, files, concepts
- **Edges** (arrows) represent relationships: "A depends on B", "A comes before B"

### What's "Acyclic"?

It means **no loops**. You can't follow the arrows and end up back where you started.

Valid (acyclic):
```
A → B → C
```

Invalid (has a cycle):
```
A → B → C → A  (cycle!)
```

**Why does this matter?** Acyclic graphs represent **hierarchies** and **dependencies**. You can't be your own ancestor. A task can't depend on itself. These graphs have a natural "top-to-bottom" flow.

### Why Does Layout Matter?

Look at the same graph with bad vs good layout:

**Bad (manual) layout:**
```
┌─┐        ┌─┐
│A│───┐    │C│
└─┘   │    └─┘
      │     │
      │  ┌──┘
      │  │
      ▼  ▼
      ┌─┐
      │D│◀───┐
      └─┘    │
             │
      ┌─┐    │
      │B│────┘
      └─┘
```
Crossing lines! Hard to follow!

**Good (dag-draw) layout:**
```
┌─┐  ┌─┐
│A│  │B│
└┬┘  └┬┘
 │    │
 ├────┴───┐
 │        │
 ▼        ▼
┌─┐      ┌─┐
│C│      │D│
└─┘      └─┘
```
Clear hierarchy! Easy to read!

**This is what dag-draw does:** It takes your messy graph and organizes it beautifully.

## How It Works: From Nodes to Diagrams

When you call `(dag-draw-layout-graph graph)`, four things happen automatically:

### Pass 1: Ranking (Vertical Levels)

**Goal:** Figure out which nodes go on which "level."

```
Level 0:  [A] [B]        (no dependencies)
Level 1:  [C] [D]        (depend on level 0)
Level 2:  [E]            (depends on level 1)
```

**Why:** This creates the top-to-bottom flow. Nodes that nothing depends on go at the top. Everything flows downward.

### Pass 2: Ordering (Horizontal Positions)

**Goal:** Within each level, arrange nodes to minimize crossing arrows.

```
Level 1:  [C] [D]    (fewer crossings)
   vs
Level 1:  [D] [C]    (more crossings)
```

**Why:** Crossing lines are confusing. The algorithm tries hundreds of arrangements to find one with minimal crossings.

### Pass 3: Positioning (Exact Coordinates)

**Goal:** Calculate precise X,Y positions that:
- Keep enough space between nodes
- Align connected nodes nicely
- Make edges as short as practical

**Why:** "Minimize crossings" isn't enough. We need actual positions for rendering.

### Pass 4: Splines (Smooth Edges)

**Goal:** Draw edges as smooth curves (not just straight lines) that avoid overlapping nodes.

**Why:** Straight lines look harsh. Smooth curves look professional and are easier to follow.

**The Result:** Your nodes and edges are now positioned beautifully. Time to render!

## Choosing Your Output: ASCII vs SVG

dag-draw supports two output formats. **When do you use each?**

### ASCII: Quick and Portable

```elisp
(dag-draw-render-graph my-graph 'ascii)
```

**Best for:**
- Quick previews in your terminal
- Including in source code comments
- Email or plain-text documentation
- README files (like this one!)
- Debugging layouts

**Characteristics:**
- Uses box-drawing characters (┌─┐│└┘├┤┬┴┼)
- Looks good in monospace fonts
- Copy-paste anywhere
- Instant visual feedback

### SVG: Publication Quality

```elisp
(dag-draw-render-graph my-graph 'svg)
```

**Best for:**
- Documentation websites
- Presentations
- Scalable diagrams (zoom without pixelation)
- Custom styling (colors, fonts)
- Interactive web applications

**Characteristics:**
- Smooth Bézier curves
- Crisp at any size
- Can embed in HTML
- Professional appearance

**Rule of thumb:** Use ASCII for quick checks, SVG for final output.

## Customizing Your Graphs

Now that you understand the basics, let's make graphs look the way you want.

### Adjusting Spacing

Graph too cramped?

```elisp
;; More space between nodes horizontally
(setq dag-draw-default-node-separation 30)

;; More space between vertical levels
(setq dag-draw-default-rank-separation 50)

(dag-draw-layout-graph my-graph)
```

### Adding Visual Attributes

Want colored nodes or custom shapes?

```elisp
(dag-draw-add-node my-graph 'critical "Critical Task"
  (ht ("color" "red")
      ("shape" "diamond")))

(dag-draw-add-node my-graph 'optional "Optional"
  (ht ("color" "gray")
      ("style" "dashed")))
```

These attributes are used by the SVG renderer for styling.

### Edge Weights

Some dependencies are stronger than others:

```elisp
;; High-priority dependency
(dag-draw-add-edge my-graph 'a 'b
  (ht ("weight" 10)))

;; Low-priority optional dependency
(dag-draw-add-edge my-graph 'a 'c
  (ht ("weight" 1)))
```

Higher weights pull nodes closer together.

## Going Deeper: The GKNV Algorithm

Want to understand how this actually works under the hood?

### The Research Behind It

dag-draw implements the **GKNV algorithm** from a seminal 1993 paper:

> Gansner, E. R., Koutsofios, E., North, S. C., & Vo, K. P. (1993).
> "A Technique for Drawing Directed Graphs."
> IEEE Transactions on Software Engineering, 19(3), 214-230.

This algorithm is used by **Graphviz** (the industry-standard graph layout tool) and has been cited thousands of times. It's the gold standard for hierarchical graph layout.

### Why This Algorithm?

Other approaches exist (force-directed, circular, etc.), but GKNV is optimal for **hierarchical graphs** because:

1. **It respects hierarchy:** Graphs flow in a consistent direction
2. **It minimizes crossings:** Uses sophisticated heuristics proven in practice
3. **It's fast:** Polynomial time complexity handles large graphs
4. **It's extensible:** Each pass can be customized independently

### The Four Passes in Detail

If you read the code, you'll see four modules corresponding to the passes:

**Pass 1: `dag-draw-pass1-ranking.el`**
- Implements network simplex algorithm
- Solves an optimization problem: minimize total edge length
- Assigns each node a "rank" (vertical level)

**Pass 2: `dag-draw-pass2-ordering.el`**
- Implements weighted median heuristic
- Uses local transposition for fine-tuning
- Minimizes edge crossings within constraints

**Pass 3: `dag-draw-pass3-positioning.el`**
- Constructs auxiliary graph for X-coordinate assignment
- Another network simplex optimization
- Handles node separation constraints

**Pass 4: `dag-draw-pass4-splines.el`**
- Generates Bézier curve control points
- Routes edges around nodes
- Calculates port positions (where edges attach)

### Implementation Philosophy

The codebase follows the paper closely, using the same variable names (λ for rank, ρ for separation) and algorithm structure. If you want to understand graph layout deeply, read the paper alongside the code.

## Architecture for Hackers

If you're interested in the internal design (or want to contribute), here's how it's structured.

### Bounded Contexts (DDD)

The codebase uses **Domain-Driven Design** with four bounded contexts:

1. **GKNV Algorithm Context** (Pure layout logic)
   - Modules: `dag-draw-pass*.el`
   - Coordinates: World coordinates (floats, can be negative)
   - Responsibility: Graph layout only

2. **Coordinate Transform Layer** (ASCII-specific)
   - Module: `dag-draw-coord-transform.el`
   - Responsibility: Convert world coords → grid coords
   - Why: ASCII needs non-negative integer positions

3. **ASCII Rendering Context**
   - Modules: `dag-draw-ascii-grid.el`, `dag-draw-ascii-junctions.el`
   - Coordinates: Grid coordinates (integers, non-negative)
   - Responsibility: Box-drawing character output

4. **SVG Rendering Context**
   - Module: `dag-draw-svg.el`
   - Coordinates: World coordinates (directly from GKNV)
   - Responsibility: Scalable vector graphics output

### Key Insight

The GKNV algorithm produces **one layout** (in world coordinates) that serves **both rendering paths**:

```
GKNV Algorithm (world coords)
        │
    ┌───┴────┐
    │        │
  ASCII     SVG
   Path     Path
    │        │
Transform  Direct
 (to grid)
    │        │
    ▼        ▼
  ASCII     SVG
 Output   Output
```

**ASCII** needs transformation because character grids require integers.
**SVG** uses world coordinates directly because SVG supports floats and negatives.

This separation means:
- Adding PDF output? Just create a new rendering module
- Optimizing ASCII? Won't affect SVG
- GKNV algorithm stays pure and format-agnostic

### Module Organization

```
dag-draw-core.el         # Data structures (graph, node, edge)
dag-draw-pass1-ranking.el      # Network simplex ranking
dag-draw-pass2-ordering.el     # Crossing reduction
dag-draw-pass3-positioning.el  # X-coordinate assignment
dag-draw-pass4-splines.el      # Bézier curve generation

dag-draw-coord-transform.el    # World → Grid (ASCII-specific)
dag-draw-ascii-grid.el         # ASCII grid management
dag-draw-ascii-junctions.el    # Junction character enhancement
dag-draw-svg.el                # SVG rendering

dag-draw-render.el             # Orchestration layer
```

### Testing Philosophy

The project uses **Test-Driven Development**:

- 580+ tests covering algorithm correctness
- Acceptance tests for visual quality
- Each bounded context tested independently
- Tests document expected behavior

Run tests: `eldev test`

## API Reference

### Graph Creation

```elisp
(dag-draw-create-graph &optional attributes)
```
Creates a new empty graph. Optional attributes is a hash table.

### Node Operations

```elisp
(dag-draw-add-node graph node-id label &optional attributes)
```
Adds a node with unique `node-id`, display `label`, and optional attributes.

```elisp
(dag-draw-get-node graph node-id)
```
Retrieves node by ID.

```elisp
(dag-draw-node-label node)
(dag-draw-node-x-coord node)
(dag-draw-node-y-coord node)
(dag-draw-node-rank node)
```
Access node properties (after layout).

### Edge Operations

```elisp
(dag-draw-add-edge graph from-id to-id &optional attributes)
```
Adds a directed edge from `from-id` to `to-id`.

```elisp
(dag-draw-get-edges graph)
```
Returns list of all edges.

### Layout

```elisp
(dag-draw-layout-graph graph)
```
Runs the four-pass GKNV algorithm. Modifies graph in-place, adding coordinates to nodes and spline points to edges.

### Rendering

```elisp
(dag-draw-render-graph graph format)
```
Renders graph in specified format. Returns a string.

Formats:
- `'ascii` - Box-drawing characters
- `'svg` - Scalable Vector Graphics
- `'dot` - Graphviz DOT language

### Configuration

```elisp
dag-draw-default-node-separation     ;; Default: 20
dag-draw-default-rank-separation     ;; Default: 25
dag-draw-ascii-node-separation       ;; Default: 6 (ASCII mode)
dag-draw-ascii-rank-separation       ;; Default: 5 (ASCII mode)

dag-draw-render-svg-node-fill        ;; Default: "#f0f0f0"
dag-draw-render-svg-node-stroke      ;; Default: "#000000"
dag-draw-render-svg-edge-stroke      ;; Default: "#666666"
```

## Installation

### From Source (Development)

```bash
git clone https://github.com/example/dag-draw.el.git
cd dag-draw.el
eldev test  # Run test suite
```

### Using package.el (When Released)

```elisp
(package-install 'dag-draw)
```

### Dependencies

- Emacs 26.1+
- `dash` library (list manipulation)
- `ht` library (hash tables)

## Performance

The GKNV algorithm has polynomial time complexity but is fast in practice:

- Graphs with **100 nodes**: <1 second
- Graphs with **500 nodes**: ~5 seconds
- Graphs with **1000+ nodes**: May take minutes

Bottlenecks are in Pass 1 (ranking) and Pass 3 (positioning), both of which use network simplex optimization.

## Troubleshooting

### "Graph has cycles" Error

**Problem:** Your graph has a cycle (A → B → C → A).

**Solution:** DAGs must be acyclic. Remove the edge that creates the cycle.

**Debug:** Use `dag-draw-find-cycle` to identify the problematic edges.

### Nodes Overlap in ASCII Output

**Problem:** Nodes with long labels overlap in ASCII rendering.

**Solution:** Increase `dag-draw-ascii-node-separation`:

```elisp
(setq dag-draw-ascii-node-separation 10)
```

### SVG Looks Cramped

**Problem:** SVG output has nodes too close together.

**Solution:** Increase separation before layout:

```elisp
(setq dag-draw-default-node-separation 30)
(setq dag-draw-default-rank-separation 50)
(dag-draw-layout-graph my-graph)
```

### Performance Issues

**Problem:** Layout takes too long.

**Solutions:**
- Reduce graph size (combine nodes where possible)
- Simplify edges (remove redundant dependencies)
- Use caching (layout once, render multiple times)

## Contributing

We use **Acceptance Test-Driven Development**:

1. Fork the repository
2. Write a failing test first (describe expected behavior)
3. Implement the feature
4. Ensure all tests pass: `eldev test`
5. Submit a pull request

### Running Tests

```bash
# All tests
eldev test

# Specific test file
eldev test test/dag-draw-pass1-test.el

# Watch mode (reruns on file change)
eldev test --watch

# With debugging output
eldev -dtT test
```

## License

GPL-3.0 or later. See LICENSE file for details.

## References

### Papers
- Gansner et al. (1993). "A Technique for Drawing Directed Graphs." IEEE TSE.
- Sugiyama et al. (1981). "Methods for Visual Understanding of Hierarchical System Structures."

### Related Projects
- **Graphviz**: Industry-standard graph visualization (C++)
- **ELK**: Eclipse Layout Kernel (Java)
- **Dagre**: JavaScript implementation of similar algorithm

### Documentation Philosophy
This README follows Steve Losh's ["Teach, Don't Tell"](https://stevelosh.com/blog/2013/09/teach-dont-tell/) philosophy. We teach you to understand graph layout, not just list API functions.

## Questions?

- **Issues**: https://github.com/example/dag-draw.el/issues
- **Discussions**: https://github.com/example/dag-draw.el/discussions
- **Email**: (maintainer email)

---

**Built with** ❤️ **and test-driven development**
