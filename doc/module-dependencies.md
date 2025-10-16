# Module Dependencies

## Overview

This document provides a comprehensive view of module dependencies in the dag-draw package, organized by bounded context.

## Three-Tier Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                   GKNV Algorithm Context                        │
│                    (World Coordinates)                          │
│                                                                 │
│  ┌────────────────────────────────────────────────────────┐   │
│  │                  dag-draw-core.el                      │   │
│  │  • Graph data structures (nodes, edges)                │   │
│  │  • Basic graph operations (add/remove/query)           │   │
│  │  • Node/edge properties (rank, x-coord, y-coord)       │   │
│  └─────────┬──────────────────────────────────────────────┘   │
│            │                                                    │
│            ├──→ dag-draw-topological.el                        │
│            │    • Topological sorting                          │
│            │    • Source/sink node detection                   │
│            │                                                    │
│            ├──→ dag-draw-cycle-breaking.el                     │
│            │    • Cycle detection                              │
│            │    • Edge reversal for DAG creation               │
│            │                                                    │
│            ├──→ dag-draw-pass1-ranking.el                      │
│            │    • Network simplex algorithm                    │
│            │    • Rank assignment (minimize edge lengths)      │
│            │                                                    │
│            ├──→ dag-draw-pass2-ordering.el                     │
│            │    • Crossing reduction                           │
│            │    • Barycentric/median heuristics                │
│            │                                                    │
│            ├──→ dag-draw-pass3-positioning.el                  │
│            │    • X-coordinate assignment                      │
│            │    • Node separation (ρ function)                 │
│            │    • Bounding box calculation                     │
│            │                                                    │
│            └──→ dag-draw-pass4-splines.el                      │
│                 • Edge routing with splines                    │
│                 • Port calculation                             │
│                 • Bezier curve generation                      │
│                                                                 │
└─────────────────────────────┬───────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│              Coordinate Transform Layer (ACL)                   │
│                   (Translation Layer)                           │
│                                                                 │
│  ┌────────────────────────────────────────────────────────┐   │
│  │            dag-draw-coord-transform.el                 │   │
│  │                                                         │   │
│  │  Depends on:                                           │   │
│  │    • dag-draw-core.el (for graph structure access)    │   │
│  │    • ht (hash table library)                           │   │
│  │                                                         │   │
│  │  Provides:                                             │   │
│  │    • dag-draw--world-to-grid-coord                     │   │
│  │    • dag-draw--world-to-grid-size                      │   │
│  │    • dag-draw--create-ascii-coordinate-context         │   │
│  │    • dag-draw--ascii-world-to-grid                     │   │
│  │    • dag-draw--ascii-get-bounds                        │   │
│  └────────────────────────────────────────────────────────┘   │
└─────────────────────────────┬───────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                  ASCII Rendering Context                        │
│                    (Grid Coordinates)                           │
│                                                                 │
│  ┌────────────────────────────────────────────────────────┐   │
│  │              dag-draw-ascii-grid.el                    │   │
│  │                                                         │   │
│  │  Depends on:                                           │   │
│  │    • dag-draw-core.el                                  │   │
│  │    • dag-draw-coord-transform.el ← ACL                 │   │
│  │    • dag-draw-ascii-junctions.el                       │   │
│  │    • ht (hash table library)                           │   │
│  │                                                         │   │
│  │  Provides:                                             │   │
│  │    • ASCII grid creation/management                    │   │
│  │    • Scale calculation                                 │   │
│  │    • Node collision detection/resolution               │   │
│  │    • Grid-to-string conversion                         │   │
│  └────────────────────────────────────────────────────────┘   │
│                                                                 │
│  ┌────────────────────────────────────────────────────────┐   │
│  │            dag-draw-ascii-junctions.el                 │   │
│  │                                                         │   │
│  │  Depends on:                                           │   │
│  │    • dag-draw-core.el                                  │   │
│  │                                                         │   │
│  │  Provides:                                             │   │
│  │    • Junction character selection                      │   │
│  │    • Junction type detection (5 types)                 │   │
│  │    • Grid context analysis                             │   │
│  │    • Junction application to grid                      │   │
│  └────────────────────────────────────────────────────────┘   │
│                                                                 │
│  ┌────────────────────────────────────────────────────────┐   │
│  │                 dag-draw-render.el                     │   │
│  │                                                         │   │
│  │  Depends on:                                           │   │
│  │    • dag-draw-core.el                                  │   │
│  │    • dag-draw-ascii-grid.el                            │   │
│  │    • All GKNV passes (orchestration)                   │   │
│  │                                                         │   │
│  │  Provides:                                             │   │
│  │    • High-level rendering API                          │   │
│  │    • dag-draw-render-ascii (main entry point)          │   │
│  │    • Coordinate mode selection                         │   │
│  └────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────┘
```

## Dependency Hierarchy

### Core Layer (No Dependencies)
- `dag-draw-core.el` - Foundation for everything

### GKNV Algorithm Layer
All depend on `dag-draw-core.el`:
- `dag-draw-topological.el`
- `dag-draw-cycle-breaking.el`
- `dag-draw-pass1-ranking.el`
- `dag-draw-pass2-ordering.el`
- `dag-draw-pass3-positioning.el`
- `dag-draw-pass4-splines.el`

### Anti-Corruption Layer
- `dag-draw-coord-transform.el`
  - Depends on: `dag-draw-core.el`, `ht`
  - Used by: ASCII rendering modules

### ASCII Rendering Layer
- `dag-draw-ascii-junctions.el`
  - Depends on: `dag-draw-core.el`

- `dag-draw-ascii-grid.el`
  - Depends on: `dag-draw-core.el`, `dag-draw-coord-transform.el`, `dag-draw-ascii-junctions.el`, `ht`

- `dag-draw-render.el`
  - Depends on: Everything (orchestration layer)

## Import/Require Chain

```elisp
;; Core
(require 'ht)  ; External dependency

;; GKNV Algorithm Context
(require 'dag-draw-core)
(require 'dag-draw-topological)      ; ← dag-draw-core
(require 'dag-draw-cycle-breaking)   ; ← dag-draw-core
(require 'dag-draw-pass1-ranking)    ; ← dag-draw-core
(require 'dag-draw-pass2-ordering)   ; ← dag-draw-core
(require 'dag-draw-pass3-positioning); ← dag-draw-core
(require 'dag-draw-pass4-splines)    ; ← dag-draw-core

;; Anti-Corruption Layer
(require 'dag-draw-coord-transform)  ; ← dag-draw-core, ht

;; ASCII Rendering Context
(require 'dag-draw-ascii-junctions)  ; ← dag-draw-core
(require 'dag-draw-ascii-grid)       ; ← dag-draw-core, dag-draw-coord-transform,
                                     ;   dag-draw-ascii-junctions, ht
(require 'dag-draw-render)           ; ← all of the above
```

## Key Architectural Rules

### 1. GKNV Context Independence
**Rule**: GKNV algorithm modules MUST NOT depend on ASCII rendering modules

**Reason**: GKNV algorithm is pure and could be used for SVG, PDF, or other output formats

**Enforced by**: Module structure - GKNV modules only require `dag-draw-core.el`

### 2. Anti-Corruption Layer Mediation
**Rule**: ASCII rendering MUST NOT directly access GKNV world coordinates

**Reason**: Prevents coordinate system contamination

**Enforced by**: All coordinate conversions go through `dag-draw-coord-transform.el`

### 3. Junction Character Isolation
**Rule**: Junction logic is self-contained in `dag-draw-ascii-junctions.el`

**Reason**: Visual enhancement is separate concern from grid management

**Enforced by**: Dedicated module with clear interface

### 4. One-Way Dependencies
**Rule**: Dependencies flow downward only (GKNV → Transform → ASCII)

**Reason**: Prevents circular dependencies and maintains layered architecture

**Enforced by**: Module require statements

## Module Size Overview

| Module | Lines | Functions | Purpose |
|--------|-------|-----------|---------|
| dag-draw-core.el | ~800 | ~50 | Data structures & operations |
| dag-draw-coord-transform.el | 138 | 5 | Coordinate conversion |
| dag-draw-ascii-junctions.el | 621 | 18 | Junction enhancement |
| dag-draw-ascii-grid.el | ~440 | ~15 | Grid management |
| dag-draw-render.el | ~300 | ~10 | Rendering orchestration |

## Testing Implications

### Unit Testing by Context

**GKNV Context**:
- Test with world coordinates (floats, can be negative)
- Verify algorithm compliance with paper
- Check GKNV aesthetics (A1-A5)
- No ASCII rendering concerns

**Transform Layer**:
- Test conversion accuracy
- Verify non-negative output
- Check offset calculations
- Test edge cases (negative inputs, zero, large values)

**ASCII Context**:
- Test with grid coordinates (integers, non-negative)
- Verify visual quality
- Check junction character selection
- Test with various graph structures

### Integration Testing

**GKNV → Transform**:
- Layout graph, verify transform produces valid grid coords
- Test with graphs that produce negative world coords
- Verify proportions are preserved

**Transform → ASCII**:
- Verify grid coords are always valid array indices
- Check that ASCII output matches GKNV layout structure
- Test junction characters at transformed positions

## Future Extension Points

### Adding New Output Formats

To add SVG/PDF/PNG rendering:
1. Create new bounded context (e.g., `dag-draw-svg-render.el`)
2. Reuse `dag-draw-coord-transform.el` or create format-specific transform
3. Implement format-specific rendering logic
4. GKNV algorithm remains untouched

### Adding Interactive Features

To add mouse/keyboard interaction:
1. Create interaction bounded context
2. Transform screen coords to world coords (inverse transform)
3. Manipulate GKNV graph structure
4. Re-render using existing ASCII/SVG contexts

## References

- DDD Architecture: doc/ddd-architecture.md
- GKNV Paper: doc/technique-for-drawing-directed-graphs.asciidoc
- Implementation Decisions: doc/implementation-decisions.md
- Refactoring Plan: refactoring.md
