# dag-draw.el

An Emacs package for drawing directed graphs using the Gansner-Koutsofios-North-Vo algorithm.

## Overview

This package implements the classic directed graph drawing algorithm described in "A Technique for Drawing Directed Graphs" by Gansner, Koutsofios, North, and Vo (1993). The algorithm produces high-quality hierarchical layouts with minimal edge crossings and smooth spline edges.

## Algorithm

The implementation follows the original four-pass algorithm:

1. **Rank Assignment**: Uses network simplex optimization to assign nodes to hierarchical ranks
2. **Vertex Ordering**: Minimizes edge crossings within each rank using weighted median heuristics
3. **Node Positioning**: Calculates precise X,Y coordinates using auxiliary graph optimization
4. **Spline Generation**: Creates smooth Bézier curve edges that avoid node overlaps

## Features

- Pure Elisp implementation of the GKNV algorithm
- Support for multiple output formats (SVG, ASCII, DOT)
- Customizable node and edge attributes
- Integration with Emacs workflows (org-mode, graphviz-dot-mode)
- Test-driven development with comprehensive test suite

## Installation

### Using eldev (development)

```bash
git clone https://github.com/example/dag-draw.el.git
cd dag-draw.el
eldev test
```

### Using package.el (when released)

```elisp
(package-install 'dag-draw)
```

## Usage

### Basic Example

```elisp
(require 'dag-draw)

;; Create a new graph
(setq graph (dag-draw-create-graph))

;; Add nodes
(dag-draw-add-node graph 'a "Node A")
(dag-draw-add-node graph 'b "Node B")
(dag-draw-add-node graph 'c "Node C")

;; Add edges
(dag-draw-add-edge graph 'a 'b)
(dag-draw-add-edge graph 'a 'c)
(dag-draw-add-edge graph 'b 'c)

;; Layout and render
(dag-draw-layout-graph graph)
(dag-draw-render-graph graph 'svg)
```

### Customization

```elisp
;; Set custom node separation
(setq dag-draw-default-node-separation 30)

;; Set custom rank separation  
(setq dag-draw-default-rank-separation 50)

;; Add node with custom attributes
(dag-draw-add-node graph 'special "Special Node" 
                   (ht ("color" "red") ("shape" "diamond")))
```

## Development

This project uses acceptance test-driven development (ATDD) with:

- **eldev**: Build system and dependency management
- **buttercup**: BDD-style testing framework
- **@tdd**: Test-driven development workflow support

### Running Tests

```bash
# Run all tests
eldev test

# Run tests in watch mode
eldev dag-draw-test-watch

# Run specific test file
eldev test test/dag-draw-rank-test.el
```

### Project Structure

```
dag-draw.el/
├── Eldev                    # Build configuration
├── dag-draw.el             # Main package file
├── dag-draw-core.el        # Core utilities
├── dag-draw-rank.el        # Rank assignment (network simplex)
├── dag-draw-order.el       # Vertex ordering
├── dag-draw-position.el    # Node positioning
├── dag-draw-splines.el     # Spline generation
├── dag-draw-render.el      # Output rendering
├── test/                   # Test suite
│   ├── dag-draw-test.el    # Integration tests
│   └── ...                 # Module-specific tests
└── README.md               # This file
```

## Algorithm Details

### 1. Rank Assignment

The rank assignment pass uses network simplex optimization to minimize total edge length while respecting hierarchical constraints. This ensures that the graph flows in a consistent direction (typically top-to-bottom).

### 2. Vertex Ordering

Within each rank, nodes are ordered to minimize edge crossings using:
- Weighted median heuristic for initial ordering
- Local transposition optimization for fine-tuning
- Support for flat edges (same-rank connections)

### 3. Node Positioning

X-coordinates are calculated by constructing an auxiliary graph and solving another network simplex problem. This approach handles:
- Node separation constraints
- Port-based edge connections
- Optimal edge straightening

### 4. Spline Generation

Edges are drawn as smooth Bézier curves that:
- Avoid overlapping with nodes
- Minimize visual clutter
- Support edge labels and multiple edges
- Handle self-loops and flat edges

## Performance

The algorithm has polynomial time complexity and handles graphs with hundreds of nodes efficiently. Performance characteristics:

- Rank assignment: O(V²E) in practice, often much faster
- Vertex ordering: O(VE) with iterative improvement
- Node positioning: O(V²E) via network simplex
- Spline generation: O(E) for typical graphs

## Comparison with Graphviz

This implementation aims to match the quality of Graphviz's `dot` tool while providing:

- Native Emacs integration
- Elisp extensibility
- Educational transparency
- Custom layout algorithms

## Contributing

1. Fork the repository
2. Create a feature branch
3. Write tests first (TDD approach)
4. Implement the feature
5. Ensure all tests pass
6. Submit a pull request

## License

GPL-3.0 or later. See LICENSE file for details.

## References

- Gansner, E. R., Koutsofios, E., North, S. C., & Vo, K. P. (1993). A technique for drawing directed graphs. IEEE Transactions on Software Engineering, 19(3), 214-230.
- Graphviz - Graph Visualization Software: https://graphviz.org/
- Network Simplex Algorithm: Ahuja, Magnanti, and Orlin (1993)