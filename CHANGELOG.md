# Changelog

All notable changes to dag-draw.el will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2025-11-03

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

- **Comprehensive API**
  - `dag-draw-create-graph` - Graph creation
  - `dag-draw-add-node` - Node management
  - `dag-draw-add-edge` - Edge creation with weights and labels
  - `dag-draw-layout-graph` - GKNV layout algorithm
  - `dag-draw-render` - Multi-format rendering
  - Mathematical notation aliases (λ, δ, ω, ρ) for academic correspondence

- **Developer Features**
  - 574 comprehensive tests with 100% pass rate
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

### Documentation

- Comprehensive README with "Teach, Don't Tell" philosophy
- GKNV algorithm specification documentation
- Architecture documentation with bounded context explanations
- Implementation decision rationale (D1.1-D5.8)
- ASCII rendering strategy documentation
- Full GKNV reference paper included

### Technical Details

- **Package Requirements**: Emacs 26.1+, dash 2.19.1+, ht 2.3+
- **License**: GPL-3.0-or-later
- **Algorithm Compliance**: GKNV (1993) IEEE Transactions on Software Engineering
- **Test Coverage**: 574 tests across all algorithm passes and rendering modes
- **Performance**: Handles graphs with 100 nodes in <1 second

## [Unreleased]

### Planned Features

- Full network simplex implementation for X-coordinate optimization
- Enhanced junction detection with spline analysis
- Extended edge features (δ(e) > 1 minimum lengths, same-rank constraints)
- Performance benchmarks for large graphs

---

[1.0.0]: https://codeberg.org/trevoke/dag-draw.el/releases/tag/v1.0.0
