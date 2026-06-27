# dag-draw.el

**Draw directed acyclic graphs that don't suck.**

`dag-draw` is an Emacs Lisp library that turns a description of *what connects
to what* into a clean, hierarchical drawing. You declare the structure of a DAG
(dependencies, prerequisites, workflow steps), and dag-draw lays it out for you
using the GKNV algorithm — the same hierarchical layout method behind
Graphviz's `dot`. No manual box-shuffling, no tangled arrows. It renders to
ASCII art for the terminal, SVG for the web, or Graphviz DOT for further
processing.

## Installation

dag-draw requires Emacs 26.1+ and the `dash` and `ht` packages.

```elisp
;; with use-package and a manual clone on your load-path
(use-package dag-draw
  :load-path "/path/to/dag-draw.el")
```

Or load it directly:

```elisp
(add-to-list 'load-path "/path/to/dag-draw.el")
(require 'dag-draw)
```

## Quickstart (30 seconds)

Build a graph, lay it out, render it:

```elisp
(require 'dag-draw)

(let ((g (dag-draw-create-graph)))
  (dag-draw-add-node g 'design "Design")
  (dag-draw-add-node g 'build  "Build")
  (dag-draw-add-node g 'test   "Test")
  (dag-draw-add-node g 'ship   "Ship")
  (dag-draw-add-edge g 'design 'build)
  (dag-draw-add-edge g 'design 'test)
  (dag-draw-add-edge g 'build  'ship)
  (dag-draw-add-edge g 'test   'ship)
  (dag-draw-layout-graph g)
  (princ (dag-draw-render-graph g 'ascii)))
```

Output:

```
┌────────┐
│ Design │
└────┬───┘
     │
     ├─────────────┐
┌────▼──┐      ┌───▼───┐
│ Build │      │ Test  │
└───┬───┘      └───┬───┘
    │              │
    ├──────────────┘
┌───▼───┐
│ Ship  │
└───────┘
```

You described the structure; dag-draw chose the positions, routed the edges
around the boxes, and drew the junction characters.

## The three steps

Every drawing follows the same pipeline:

1. **Build** a graph — `dag-draw-create-graph` plus `dag-draw-add-node` /
   `dag-draw-add-edge`, or build it all at once from a declarative spec with
   `dag-draw-create-from-spec`.
2. **Lay out** the graph — `dag-draw-layout-graph` assigns ranks, orders nodes
   to reduce crossings, and computes coordinates and edge routes.
3. **Render** the graph — `dag-draw-render-graph` with a format symbol.

## Output formats

`dag-draw-render-graph` takes a format symbol: `'ascii` for monospace terminal
art (Unicode box-drawing), `'svg` for scalable web graphics with styling and
tooltips, and `'dot` for Graphviz DOT source you can pipe into other tools.

## Documentation

The full manual ships as an Emacs Info manual. After installing, read it with
`C-h i` and choose **Dag Draw**, or open `doc/dag-draw.texi`. It is organized
along the four [Diataxis](https://diataxis.fr/) quadrants:

- **Tutorials** — learning-oriented walkthroughs. Start here if you are new:
  build your first graph, then build one from a declarative spec.
- **How-to guides** — task-oriented recipes: query a graph's structure, detect
  cycles, highlight a node, produce SVG or DOT, and tune spacing.
- **Reference** — the complete, verified public API: every function signature,
  configuration variable, and node/edge attribute.
- **Explanation** — the ideas behind the tool: the GKNV layout passes, how
  ASCII edges and junctions are drawn, and why dag-draw is DAG-only.

## API at a glance

| Task | Functions |
|------|-----------|
| Build | `dag-draw-create-graph`, `dag-draw-create-from-spec`, `dag-draw-add-node`, `dag-draw-add-edge` |
| Lay out | `dag-draw-layout-graph` |
| Render | `dag-draw-render-graph` |
| Query | `dag-draw-get-node`, `dag-draw-graph-edges`, `dag-draw-get-edges-from`, `dag-draw-get-edges-to`, `dag-draw-find-edge`, `dag-draw-get-node-ids`, `dag-draw-node-count`, `dag-draw-edge-count`, `dag-draw-get-successors`, `dag-draw-get-predecessors`, `dag-draw-get-source-nodes`, `dag-draw-get-graph-bounds`, `dag-draw-detect-cycles` |
| Mutate | `dag-draw-remove-node`, `dag-draw-remove-edge` |

See the Reference section of the manual for full signatures and the complete
list of configuration variables and visual attributes.

## License

GPL-3.0-or-later. See [LICENSE](LICENSE).
