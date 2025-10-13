# Documentation Archive

This directory contains historical and superseded documentation that has been archived to prevent confusion.

## Why These Documents Are Archived

These documents were created during early exploration and planning phases. They have been superseded by the official implementation decisions documented in:
- `implementation-decisions.md`
- `algorithm-specification.md`
- `ascii-rendering-strategy.md`
- `implementation-roadmap.md`

## Archived Documents

### summary.txt
**Archived**: 2025-10-13
**Reason**: Contains outdated information about backtracking and iterative refinement between passes. The implementation decisions clarify that the baseline GKNV algorithm does NOT include multi-pass backtracking as a core feature. This document described iteration strategies that are not part of the GKNV baseline specification.

**Historical Value**: Provides insight into early understanding of the algorithm structure. Useful for understanding what was clarified during implementation planning.

### plan-to-correct-discrepancies.txt
**Archived**: 2025-10-13
**Reason**: Contains suggested enhancements that are NOT part of the GKNV paper baseline algorithm. These enhancements (backtracking from Pass 3 to Pass 2, adaptive parameters, size-aware ordering beyond what GKNV specifies, etc.) were proposed before the comprehensive implementation decisions were made. Decision D-FUTURE confirmed these are future enhancements, not baseline features.

**Historical Value**: Good source of future enhancement ideas once the baseline implementation is complete and working. The test strategies described may be valuable when implementing enhancements.

## What to Use Instead

For current, authoritative documentation, see these files in the parent `doc/` directory:

1. **technique-for-drawing-directed-graphs.asciidoc** - Source of truth (GKNV paper)
2. **gknv-to-ascii-mapping.md** - Maps paper concepts to code structure
3. **ubiquitous-language.org** - Terminology reference
4. **implementation-decisions.md** - All 45+ decisions with rationale
5. **algorithm-specification.md** - Detailed specifications with pseudocode
6. **ascii-rendering-strategy.md** - ASCII-specific rendering approach
7. **implementation-roadmap.md** - Implementation order and milestones

## Archive Policy

Documents are archived rather than deleted when:
- They contain historically interesting exploration or analysis
- They might provide value for future enhancement work
- They document decisions that were considered but not chosen
- They provide context for why certain approaches were taken

Documents are clearly marked as ARCHIVED to prevent confusion.
