# Changelog

All notable changes to dag-draw.el will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.1.0] - 2026-06-27

Code-cleanup and bugfix release. No changes to the documented public API.

### Fixed

- **ASCII edge routing**: rank-skipping ("transitive") edges are no longer drawn
  straight through an intervening node box (which overwrote its label, e.g.
  `Build` becoming `Bui│d`). Edges now route around node boxes in grid space.
- **ASCII port junctions**: a node's bottom border now shows the source-port
  junction where an edge leaves it (`┬`, or `╦` for highlighted/selected nodes),
  matching the documented junction behaviour.
- **ASCII whitespace**: rendered output no longer pads every line out to a
  uniform width or appends trailing blank lines — no more trailing whitespace.
  Interior and leading spaces are preserved.
- **Docs accuracy**: the README referenced two functions that never existed
  (`dag-draw-get-edges`, `dag-draw-find-cycle`); these now point at the real
  accessors and at `dag-draw-detect-cycles`.

### Changed

- Documentation rewritten following the [Diataxis](https://diataxis.fr/)
  framework (tutorials / how-to / reference / explanation). The README was
  condensed from ~1700 lines to ~120. `doc/dag-draw.org` remains the single
  source for the bundled Info manual.

### Removed

- Removed undocumented GKNV-name function aliases that were never part of the
  documented public API: `dag-draw-rank`, `dag-draw-ordering`,
  `dag-draw-position`, `dag-draw-make-splines`, `dag-draw-init-rank`,
  `dag-draw-generate-spline`, `dag-draw-ρ`. Use `dag-draw-layout-graph`, which
  runs all four GKNV passes.
- Large internal cleanup: removed six whole modules' worth of dead/unreachable
  code and dozens of unused helper functions (~3,900 lines), with no effect on
  the documented public API. The shipped behaviour is unchanged except for the
  rendering fixes above.

## [1.0.0] - 2025-11-17

### Added

- **Complete GKNV Algorithm Implementation** (~85% verified compliance)
  - Pass 1: Ranking with network simplex optimization
  - Pass 2: Node ordering with weighted median heuristic
  - Pass 3: Coordinate positioning with auxiliary graph method
  - Pass 4: Spline generation with Bézier curves

- **Dual Rendering Support**
  - ASCII output with Unicode box-drawing characters
  - SVG output for high-quality graphics
  - DOT format export for Graphviz integration

- **Advanced ASCII Features**
  - Context-aware junction character selection
  - Automatic arrow placement
  - Node boundary detection and handling
  - Edge routing with corner characters

- **Batch Graph Creation API** - Declarative graph construction from data structures
  - `dag-draw-create-from-spec` - Create graphs from property list specifications
  - Node format: `(node-id :label "Label" &rest attributes)`
  - Edge format: `(from-id to-id &rest attributes)`
  - User-friendly property lists automatically converted to internal hash tables
  - Comprehensive validation with descriptive error messages
  - 19 comprehensive tests covering all validation scenarios
  - Use cases: Generating graphs from external data, configuration-driven diagrams

- **Visual Properties System** - Customize node and edge appearance
  - ASCII properties:
    - `:ascii-marker` - Prefix string for node labels (e.g., "✓ ", "→ ")
    - `:ascii-highlight` - Double-line box borders (╔═╗╚╝║) for emphasis
  - SVG properties:
    - `:svg-fill` - Background color (CSS color values)
    - `:svg-stroke` - Border color (CSS color values)
    - `:svg-stroke-width` - Border thickness (numeric values)
  - Works with both imperative API (`dag-draw-add-node`) and batch API (`dag-draw-create-from-spec`)
  - Property list interface - no `ht` library required for users
  - Use cases: Status indicators, priority visualization, semantic highlighting

- **Node Selection Visualization** - All rendering formats support highlighting a selected node
  - ASCII: Selected nodes rendered with double-line box characters (╔═╗╚╝║)
  - SVG: Selected nodes rendered with blue glow filter effect
  - DOT: Selected nodes rendered with style=bold attribute
  - Added optional `selected` parameter to `dag-draw-render-graph` and format-specific renderers
  - Graceful handling of invalid node IDs (renders normally without error)
  - 37 comprehensive tests covering selection in all formats
  - Use cases: Interactive viewers, tutorials, debugging, workflow status

- **Comprehensive API**
  - `dag-draw-create-graph` - Graph creation
  - `dag-draw-add-node` - Node management with visual properties
  - `dag-draw-add-edge` - Edge creation with weights and labels
  - `dag-draw-create-from-spec` - Batch graph creation from data
  - `dag-draw-layout-graph` - GKNV layout algorithm
  - `dag-draw-render` - Multi-format rendering with selection support
  - Mathematical notation aliases (λ, δ, ω, ρ) for academic correspondence

- **Developer Features**
  - 647 comprehensive tests with 100% pass rate
  - Zero compilation warnings
  - Bounded context architecture (DDD)
  - Extensive documentation with GKNV paper references

### Changed

- **Network Simplex Algorithm Enhancement**
  - Removed arbitrary max-iterations timeout
  - Algorithm now relies on GKNV-proven termination guarantees
  - Fixed success-check ordering for trivial graphs (V=0, V=1)

### Removed

- **Fear-Based Fallback Code Cleanup**
  - Removed `dag-draw--ensure-all-nodes-have-coordinates` defensive fallback
  - Removed unused junction type handlers: `port-start`, `port-end`, `edge-join`, `edge-split`
  - Removed unreachable direction-change corner cases
  - Removed unreachable t-junction cases
  - Eliminated all fallback characters (?+ and ?┼) - algorithm is now deterministic

### Fixed

- Network simplex now correctly handles empty graphs (V=0)
- Network simplex now correctly handles single-node graphs (V=1)
- Junction character selection is now purely deterministic based on local grid analysis
- SVG viewBox bug: Fixed by including margins in viewBox dimensions
- SVG coordinate scaling bug: Fixed by implementing 10x scaling factor for ASCII coordinates

### Documentation

- Comprehensive README with "Teach, Don't Tell" philosophy
- Complete batch creation documentation with progressive tutorials
- Visual properties documentation with real-world examples
- GKNV algorithm specification documentation
- Architecture documentation with bounded context explanations
- Implementation decision rationale (D1.1-D5.8)
- ASCII rendering strategy documentation
- Full GKNV reference paper included

### Technical Details

- **Package Requirements**: Emacs 26.1+, dash 2.19.1+, ht 2.3+
- **License**: GPL-3.0-or-later
- **Algorithm Compliance**: GKNV (1993) IEEE Transactions on Software Engineering
- **Test Coverage**: 647 tests across all algorithm passes and rendering modes
- **Performance**: Handles graphs with 100 nodes in <1 second

## [Unreleased]

### Planned Features

- Full network simplex implementation for X-coordinate optimization
- Enhanced junction detection with spline analysis
- Extended edge features (δ(e) > 1 minimum lengths, same-rank constraints)
- Performance benchmarks for large graphs

---

[1.0.0]: https://codeberg.org/trevoke/dag-draw.el/releases/tag/v1.0.0
