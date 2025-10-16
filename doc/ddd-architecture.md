# DDD Architecture Documentation

## Overview

The `dag-draw` package is structured using Domain-Driven Design (DDD) principles with three distinct bounded contexts. This architecture ensures clean separation of concerns between the GKNV graph layout algorithm and ASCII rendering implementation.

## Bounded Contexts

### 1. GKNV Algorithm Context (Core Domain)

**Authority**: "A Technique for Drawing Directed Graphs" by Gansner, Koutsofios, North, and Vo

**Purpose**: Implements the four-pass GKNV graph layout algorithm

**Coordinates**: World coordinates (floating-point, can be negative)

**Modules**:
- `dag-draw-core.el` - Core data structures and graph operations
- `dag-draw-pass1-ranking.el` - Rank assignment (network simplex)
- `dag-draw-pass2-ordering.el` - Vertex ordering (crossing reduction)
- `dag-draw-pass3-positioning.el` - Node positioning (coordinate assignment)
- `dag-draw-pass4-splines.el` - Edge routing (spline generation)
- `dag-draw-topological.el` - Topological sorting utilities
- `dag-draw-cycle-breaking.el` - Cycle detection and breaking

**Key Characteristics**:
- Pure GKNV algorithm implementation
- No knowledge of ASCII rendering
- Output: Positioned graph with world coordinates
- Authority for all layout decisions

### 2. ASCII Rendering Context (Supporting Domain)

**Authority**: Implementation decisions (doc/implementation-decisions.md D5.1-D5.8)

**Purpose**: Render graphs as ASCII art with box-drawing characters

**Coordinates**: Grid coordinates (integer, always non-negative)

**Modules**:
- `dag-draw-ascii-grid.el` - ASCII grid management and rendering
- `dag-draw-ascii-junctions.el` - Junction character enhancement
- `dag-draw-render.el` - High-level rendering orchestration

**Key Characteristics**:
- Discrete character grid (row/column positions)
- Unicode box-drawing characters for visual quality
- Junction character algorithm for proper semigraphics
- No knowledge of GKNV algorithm internals

**Junction Types** (CLAUDE.md specification):
1. **Port Start/End** - At node boundaries where edges begin/end
2. **Direction Changes** - Corners where edges turn (┌ ┐ └ ┘)
3. **Edge Joins** - T-junctions where edges merge
4. **Edge Splits** - T-junctions where edges separate
5. **Edge Crossings** - Where two edges cross (┼)

### 3. Coordinate Transform Layer (Anti-Corruption Layer)

**Authority**: Translation between GKNV and ASCII contexts

**Purpose**: Prevent GKNV and ASCII contexts from corrupting each other

**Module**: `dag-draw-coord-transform.el`

**Key Characteristics**:
- Single source of truth for coordinate conversions
- Maintains coordinate context (offsets, scale factors)
- Ensures ASCII coordinates are always non-negative
- Preserves GKNV layout proportions

**Functions**:
- `dag-draw--world-to-grid-coord` - Convert single coordinate
- `dag-draw--world-to-grid-size` - Convert size/dimension
- `dag-draw--create-ascii-coordinate-context` - Create transformation context
- `dag-draw--ascii-world-to-grid` - Convert with context (handles offsets)
- `dag-draw--ascii-get-bounds` - Get ASCII-safe bounds

## Architecture Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                     GKNV Algorithm Context                  │
│                    (World Coordinates)                      │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐     │
│  │   Pass 1:    │  │   Pass 2:    │  │   Pass 3:    │     │
│  │   Ranking    │→ │   Ordering   │→ │  Positioning │     │
│  │  (network    │  │  (crossing   │  │ (coordinate  │     │
│  │   simplex)   │  │  reduction)  │  │  assignment) │     │
│  └──────────────┘  └──────────────┘  └──────────────┘     │
│                                              ↓              │
│                                       ┌──────────────┐     │
│                                       │   Pass 4:    │     │
│                                       │   Splines    │     │
│                                       │   (edge      │     │
│                                       │   routing)   │     │
│                                       └──────────────┘     │
└────────────────────────────────┬────────────────────────────┘
                                 ↓
        ┌────────────────────────────────────────────┐
        │   Coordinate Transform Layer (ACL)         │
        │   dag-draw-coord-transform.el              │
        │                                            │
        │   • World → Grid conversion                │
        │   • Offset management                      │
        │   • Scale factor application               │
        │   • Non-negative coordinate guarantee      │
        └────────────────┬───────────────────────────┘
                         ↓
┌─────────────────────────────────────────────────────────────┐
│                  ASCII Rendering Context                    │
│                   (Grid Coordinates)                        │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐     │
│  │    ASCII     │  │   Junction   │  │   Rendering  │     │
│  │     Grid     │← │   Character  │← │     Main     │     │
│  │  Management  │  │  Enhancement │  │              │     │
│  └──────────────┘  └──────────────┘  └──────────────┘     │
│                                                             │
│  Output: ASCII art with box-drawing characters             │
└─────────────────────────────────────────────────────────────┘
```

## Module Dependencies

```
dag-draw-core
    ↓
    ├─→ dag-draw-pass1-ranking
    ├─→ dag-draw-pass2-ordering
    ├─→ dag-draw-pass3-positioning
    └─→ dag-draw-pass4-splines
            ↓
dag-draw-coord-transform ← (depends on dag-draw-core)
            ↓
            ├─→ dag-draw-ascii-grid
            └─→ dag-draw-ascii-junctions
                    ↓
            dag-draw-render (orchestrates everything)
```

## Data Flow

### 1. Graph Input
```elisp
(dag-draw-create-graph)
(dag-draw-add-node graph 'node-id "Label")
(dag-draw-add-edge graph 'from 'to)
```

### 2. GKNV Layout (World Coordinates)
```elisp
(dag-draw-layout-graph graph)
;; Result: Nodes have x-coord, y-coord (float, can be negative)
;; Example: Node A at (5.5, -2.3)
```

### 3. Coordinate Transformation
```elisp
(let ((context (dag-draw--create-ascii-coordinate-context graph)))
  ;; Context contains:
  ;; - offset-x: 0.0 (or positive if world coords were negative)
  ;; - offset-y: 2.3 (to make y non-negative)
  ;; - original-bounds: (0.5 -2.3 10.5 8.7)
  ;; - ascii-bounds: (0 0 10.0 11.0)

  (dag-draw--ascii-world-to-grid 5.5 -2.3 context scale)
  ;; Returns: (grid-x grid-y) where both are non-negative integers
)
```

### 4. ASCII Rendering (Grid Coordinates)
```elisp
(dag-draw-render-ascii graph)
;; Result: String with box-drawing characters
;; Example:
;;   ┌──────┐
;;   │Node A│
;;   └───┬──┘
;;       │
;;       ▼
;;   ┌──────┐
;;   │Node B│
;;   └──────┘
```

## Key Design Decisions

### D1: Separation of Coordinate Systems

**Decision**: Maintain distinct coordinate systems for GKNV (world) and ASCII (grid)

**Rationale**:
- GKNV algorithm operates on continuous coordinates
- ASCII rendering requires discrete grid positions
- Mixing the two creates confusion and bugs

**Implementation**: Anti-corruption layer (`dag-draw-coord-transform.el`)

### D2: Junction Characters as ASCII Concern

**Decision**: Junction character logic belongs in ASCII Rendering Context

**Rationale**:
- GKNV paper describes graphical (PostScript) output, not ASCII
- Junction characters are purely a visual enhancement for character grids
- ASCII context is the authority for visual quality decisions

**Implementation**: Dedicated module (`dag-draw-ascii-junctions.el`)

### D3: Single Source of Truth for Transformations

**Decision**: All coordinate conversions go through transform layer

**Rationale**:
- Prevents duplicate transformation logic
- Ensures consistent behavior across codebase
- Makes testing and debugging easier
- Clear responsibility assignment

**Implementation**: Transform layer functions are the only place where world↔grid conversion happens

### D4: Context Objects for Transformation State

**Decision**: Use context hash tables to track transformation parameters

**Rationale**:
- Offsets and bounds need to be consistent across multiple conversions
- Passing context avoids recalculating offsets repeatedly
- Makes transformation state explicit and testable

**Implementation**: `dag-draw--create-ascii-coordinate-context` returns hash table

## Testing Strategy

### GKNV Algorithm Tests
- Test world coordinate output
- Verify GKNV aesthetics (A1-A5)
- Check algorithm compliance with paper
- Use floating-point assertions

### Transform Layer Tests
- Test coordinate conversion accuracy
- Verify non-negative guarantee
- Check offset calculations
- Test with negative world coordinates

### ASCII Rendering Tests
- Test junction character selection
- Verify visual output quality
- Check grid coordinate validity
- Test with various graph structures

## Future Extensions

### Potential Bounded Contexts

1. **SVG Rendering Context**
   - Would also use coordinate transform layer
   - Different visual concerns than ASCII
   - Could share GKNV layout results

2. **Interactive Context**
   - Mouse/keyboard input handling
   - Node selection and highlighting
   - Would translate between screen coords and world coords

3. **Animation Context**
   - Transition between graph states
   - Interpolation in world coordinates
   - Transform to display coordinates

## References

- GKNV Paper: doc/technique-for-drawing-directed-graphs.asciidoc
- Implementation Decisions: doc/implementation-decisions.md
- Junction Spec: CLAUDE.md "Implementation Principles: Junction characters for ASCII graphs"
- Refactoring Plan: refactoring.md

## Glossary

- **World Coordinates**: Floating-point (x,y) positions from GKNV algorithm
- **Grid Coordinates**: Integer (row,col) positions in ASCII character grid
- **Bounded Context**: A explicit boundary within which a domain model is defined
- **Anti-Corruption Layer**: Translation layer between bounded contexts
- **Junction Character**: Box-drawing character at edge connection point (┬┴├┤┌┐└┘┼)
- **GKNV Aesthetics**: Quality criteria defined in original paper (A1-A5)
- **Scale Factor**: Ratio for converting world distances to grid distances
- **Offset**: Translation value to ensure non-negative coordinates
