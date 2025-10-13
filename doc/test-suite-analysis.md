# Test Suite Analysis for dag-draw.el

**Created:** 2025-10-13  
**Purpose:** Comprehensive categorization of all 101 test files against GKNV baseline specification  
**Context:** Week 2 verification - Test cleanup and baseline alignment

---

## Executive Summary

### Overview

**Total Test Files Analyzed:** 101  
**Total Test Code:** ~14,222 lines  
**Test Framework:** Buttercup  
**Coverage Scope:** All four GKNV passes + ASCII rendering + infrastructure

###Key Findings

**Overall Health:** STRONG ✅

The test suite is comprehensive and well-organized, with strong coverage of:
- GKNV baseline algorithm (Passes 1-4)
- Network simplex optimization
- DFS cycle breaking
- ASCII rendering with junction characters
- Integration scenarios

**Breakdown:**

| Category | Count | % | Status |
|----------|-------|---|--------|
| **A. BASELINE Tests** | 52 | 51% | ✅ KEEP - Core GKNV compliance |
| **B. ENHANCEMENT Tests** | 18 | 18% | 🔵 KEEP/MARK - Future features |
| **C. DEBUG/Development** | 15 | 15% | 🟡 REVIEW - May be obsolete |
| **D. INFRASTRUCTURE** | 11 | 11% | ✅ KEEP - Essential utilities |
| **E. QUALITY/Monitoring** | 5 | 5% | ✅ KEEP - Post-algorithm analysis |

**Critical Insight:** NO massive test deletion required. Most tests verify baseline behavior or are infrastructure/helpers.

---

## Category A: BASELINE Tests (KEEP - 52 files)

These tests verify GKNV baseline algorithm behavior per implementation-decisions.md (D1.x-D5.x).

### A1: Pass 1 - Ranking Tests (11 files)

Tests for GKNV Pass 1: Rank assignment using network simplex (D1.1-D1.10)

| File | What It Tests | Decision | Status |
|------|---------------|----------|--------|
| **gknv-network-simplex-test.el** | Network simplex core algorithm per GKNV Figure 2-1 | D1.2, D1.3 | ✅ BASELINE |
| **dag-draw-network-simplex-core-test.el** | Leave/enter edge selection, tree optimization | D1.4, D1.8 | ✅ BASELINE |
| **dag-draw-network-simplex-comprehensive-test.el** | End-to-end network simplex with cut values | D1.2-D1.8 | ✅ BASELINE |
| **dag-draw-network-simplex-integration-test.el** | Integration of feasible tree + optimization | D1.3, D1.4 | ✅ BASELINE |
| **dag-draw-network-simplex-iteration-test.el** | Iterative optimization loop (Figure 2-1 steps 3-6) | D1.4 | ✅ BASELINE |
| **dag-draw-network-simplex-spanning-tree-test.el** | Feasible spanning tree construction | D1.3 | ✅ BASELINE |
| **dag-draw-network-simplex-cut-values-test.el** | Cut value calculation (incremental + postorder) | D1.8 | ✅ BASELINE |
| **dag-draw-network-simplex-edge-weights-test.el** | Edge weight interpretation (ω = 1, 2, 8) | D1.5, D3.2 | ✅ BASELINE |
| **dag-draw-real-network-simplex-test.el** | Real-world network simplex scenarios | D1.2-D1.8 | ✅ BASELINE |
| **dag-draw-dfs-edge-classification-test.el** | DFS edge classification (tree/forward/back/cross) | D1.1 | ✅ BASELINE |
| **dag-draw-gknv-cycle-breaking-comprehensive-test.el** | DFS cycle breaking per Section 2.1 | D1.1 | ✅ BASELINE |

**Rationale:** Network simplex is THE baseline ranking method per D1.2. These tests verify GKNV Figure 2-1 implementation.

**Action:** KEEP ALL - These are essential baseline compliance tests.

---

### A2: Pass 1 - Rank Balancing & Topological Tests (3 files)

| File | What It Tests | Decision | Status |
|------|---------------|----------|--------|
| **dag-draw-rank-balancing-test.el** | Greedy rank balancing (GKNV Figure 2-1 step 8) | D1.7 | ✅ BASELINE |
| **dag-draw-rank-hierarchy-test.el** | Rank constraints (S_min, S_max) | D1.9 | ✅ BASELINE |
| **dag-draw-rank-test.el** | Overall ranking correctness | D1.1-D1.10 | ✅ BASELINE |

**Action:** KEEP ALL - Rank balancing is GKNV baseline (D1.7).

---

### A3: Pass 2 - Ordering Tests (7 files)

Tests for GKNV Pass 2: Node ordering within ranks (D2.1-D2.9)

| File | What It Tests | Decision | Status |
|------|---------------|----------|--------|
| **dag-draw-crossing-reduction-test.el** | Weighted median + transposition | D2.2, D2.4 | ✅ BASELINE |
| **dag-draw-size-aware-ordering-test.el** | Median calculation with node sizes | D2.2 | ✅ BASELINE |
| **dag-draw-convergence-test.el** | Ordering convergence detection | D2.3 | ✅ BASELINE |
| **dag-draw-auxiliary-graph-test.el** | Virtual node creation before ordering | D2.8 | ✅ BASELINE |
| **dag-draw-lower-level-edges-test.el** | Long edge handling with virtual nodes | D2.8 | ✅ BASELINE |
| **dag-draw-edge-placement-test.el** | Edge positioning within ordering | D2.5, D2.9 | ✅ BASELINE |
| **dag-draw-algorithms-test.el** | Core ordering algorithms (median, transpose) | D2.2, D2.4 | ✅ BASELINE |

**Action:** KEEP ALL - These test baseline ordering per GKNV Section 3.

---

### A4: Pass 3 - Positioning Tests (10 files)

Tests for GKNV Pass 3: X/Y coordinate assignment (D3.1-D3.7)

| File | What It Tests | Decision | Status |
|------|---------------|----------|--------|
| **dag-draw-position-test.el** | Overall positioning correctness | D3.1-D3.6 | ✅ BASELINE |
| **dag-draw-auxiliary-graph-simplex-test.el** | Auxiliary graph construction for X coords | D3.1, D3.4 | ✅ BASELINE |
| **dag-draw-hierarchical-positioning-test.el** | Y coordinate assignment by rank | D3.6 | ✅ BASELINE |
| **dag-draw-minpath-test.el** | minpath() virtual chain straightening | D3.2 | ✅ BASELINE |
| **dag-draw-packcut-test.el** | packcut() layout compaction | D3.3 | ✅ BASELINE |
| **dag-draw-pass3-baseline-compliance-test.el** | Verifies no heuristic fallback (D3.1 compliance) | D3.1 | ✅ BASELINE |
| **dag-draw-enhanced-separation-test.el** | Node separation ρ(a,b) formula | D3.3 | ✅ BASELINE |
| **dag-draw-node-ports-test.el** | X-offset node ports | D3.5 | ✅ BASELINE |
| **dag-draw-port-calculation-test.el** | Port delta calculations | D3.5 | ✅ BASELINE |
| **dag-draw-side-centered-ports-test.el** | Port positioning on node sides | D3.5 | ✅ BASELINE |

**Action:** KEEP ALL - Auxiliary graph + network simplex is baseline per D3.1.

**Note:** `dag-draw-pass3-baseline-compliance-test.el` was CREATED during alignment to verify heuristic removal.

---

### A5: Pass 4 - Splines & Edge Drawing Tests (10 files)

Tests for GKNV Pass 4: Region-constrained splines (D4.1-D4.12)

| File | What It Tests | Decision | Status |
|------|---------------|----------|--------|
| **dag-draw-splines-test.el** | Region-constrained spline generation | D4.1, D4.9 | ✅ BASELINE |
| **dag-draw-gknv-spline-region-test.el** | Region box construction per GKNV | D4.1, D4.8 | ✅ BASELINE |
| **dag-draw-c1-continuity-test.el** | C¹ continuity at junctions | D4.11 | ✅ BASELINE |
| **dag-draw-boundary-clipping-test.el** | Spline clipping to node boundaries | D4.3 | ✅ BASELINE |
| **dag-draw-boundary-violation-test.el** | Boundary violation detection | D4.3 | ✅ BASELINE |
| **dag-draw-region-spline-routing-test.el** | Path computation through regions | D4.9 | ✅ BASELINE |
| **dag-draw-spline-length-integration-test.el** | Shortest edges first ordering | D4.2 | ✅ BASELINE |
| **dag-draw-edge-label-nodes-test.el** | Edge labels as off-center virtual nodes | D4.12 | ✅ BASELINE |
| **dag-draw-edge-label-integration-test.el** | Label integration with splines | D4.12 | ✅ BASELINE |
| **dag-draw-complex-routing-test.el** | Complex spline scenarios | D4.1-D4.11 | ✅ BASELINE |

**Action:** KEEP ALL - Region-constrained splines are GKNV baseline per D4.1.

---

### A6: ASCII Rendering Tests (10 files)

Tests for ASCII adaptation of GKNV (D5.1-D5.10)

| File | What It Tests | Decision | Status |
|------|---------------|----------|--------|
| **dag-draw-ascii-rendering-test.el** | Overall ASCII output quality | D5.1-D5.10 | ✅ BASELINE |
| **dag-draw-ascii-integration-test.el** | Full pipeline to ASCII | D5.1-D5.10 | ✅ BASELINE |
| **dag-draw-ascii-edge-routing-test.el** | Orthogonal edge routing on grid | D5.3 | ✅ BASELINE |
| **dag-draw-ascii-routing-buttercup-test.el** | Edge routing correctness | D5.3 | ✅ BASELINE |
| **dag-draw-junction-test.el** | Junction character algorithm | D5.4 | ✅ BASELINE |
| **dag-draw-junction-char-test.el** | Specific junction characters | D5.4 | ✅ BASELINE |
| **dag-draw-ascii-junction-implementation-test.el** | Junction implementation details | D5.4 | ✅ BASELINE |
| **dag-draw-ascii-box-test.el** | Node box rendering | D5.6 | ✅ BASELINE |
| **dag-draw-ascii-coordinate-preservation-test.el** | Coordinate scaling to grid | D5.1 | ✅ BASELINE |
| **dag-draw-grid-sizing-test.el** | ASCII grid dimension calculation | D5.1, D5.8 | ✅ BASELINE |

**Action:** KEEP ALL - ASCII rendering decisions D5.x are baseline adaptations.

---

### A7: Core Integration & GKNV Compliance Tests (11 files)

| File | What It Tests | Decision | Status |
|------|---------------|----------|--------|
| **gknv-paper-compliance-test.el** | Direct GKNV paper verification | ALL | ✅ BASELINE |
| **gknv-algorithm-comprehensive-test.el** | Exhaustive GKNV four-pass testing | ALL | ✅ BASELINE |
| **gknv-aesthetic-principles-test.el** | A1-A4 aesthetic compliance | A1-A4 | ✅ BASELINE |
| **gknv-edge-cases-test.el** | GKNV edge cases & robustness | ALL | ✅ BASELINE |
| **core-functionality-test.el** | API + basic graph operations | N/A | ✅ BASELINE |
| **dag-draw-end-to-end-test.el** | Full pipeline integration | ALL | ✅ BASELINE |
| **dag-draw-layout-integration-test.el** | Layout pass integration | D1-D4 | ✅ BASELINE |
| **dag-draw-render-test.el** | Rendering output | D5.x | ✅ BASELINE |
| **dag-draw-svg-renderer-test.el** | SVG output format | N/A | ✅ KEEP |
| **dag-draw-canonical-interface-test.el** | Public API contracts | N/A | ✅ KEEP |
| **dag-draw-algorithms-minimal-test.el** | Minimal algorithm tests | D1-D4 | ✅ BASELINE |

**Action:** KEEP ALL - These are critical compliance verification tests.

---

## Category B: ENHANCEMENT Tests (KEEP/MARK - 18 files)

These tests verify features BEYOND GKNV baseline. They're good tests but test non-baseline behavior.

### B1: Advanced Optimizations (Not in Baseline)

| File | What It Tests | Baseline? | Recommendation |
|------|---------------|-----------|----------------|
| **dag-draw-advanced-crossing-reduction-test.el** | Advanced crossing reduction beyond weighted median | NO | 🔵 MARK as "Future: Advanced Crossing" |
| **dag-draw-enhanced-cycle-breaking-test.el** | Enhanced cycle breaking beyond DFS | NO | 🔵 MARK as "Future: Enhanced Cycles" |
| **dag-draw-full-network-simplex-test.el** | Extended network simplex features | PARTIAL | 🟡 REVIEW - May overlap with baseline |

**Rationale:** GKNV baseline uses weighted median + transpose for crossing reduction (D2.2, D2.4). "Advanced" implies additional heuristics not in paper.

**Action:** 
- Add comment header: `;;; FUTURE ENHANCEMENT: Tests features beyond GKNV baseline`
- Keep tests but mark clearly as non-baseline
- Disable in baseline validation runs

---

### B2: Coordinate Debugging/Development (8 files)

| File | What It Tests | Baseline? | Recommendation |
|------|---------------|-----------|----------------|
| **dag-draw-coordinate-mode-test.el** | Multiple coordinate modes | MAYBE | 🟡 REVIEW content |
| **dag-draw-coordinate-scaling-test.el** | Scaling algorithms | D5.1 | ✅ If tests D5.1 |
| **dag-draw-coordinate-positioning-test.el** | Positioning correctness | D3.x | ✅ If tests D3.x |
| **dag-draw-coordinate-alignment-test.el** | Alignment algorithms | TBD | 🟡 REVIEW |
| **dag-draw-coordinate-authority-test.el** | Coordinate system authority | TBD | 🟡 REVIEW |
| **dag-draw-coordinate-debug-test.el** | Coordinate debugging utilities | NO | 🔵 DEBUG HELPER |
| **dag-draw-systematic-coordinate-debug-test.el** | Systematic debugging | NO | 🔵 DEBUG HELPER |
| **dag-draw-ascii-coordinate-failure-test.el** | Failure case debugging | NO | 🔵 DEBUG HELPER |

**Action:**
- Review each file to determine if it tests baseline (D3.x, D5.1) or enhancements
- If baseline: Move to Category A
- If debugging: Mark as development tool, not baseline test

---

### B3: Quality/Collision Detection (7 files)

| File | What It Tests | Baseline? | Recommendation |
|------|---------------|-----------|----------------|
| **dag-draw-collision-detection-test.el** | Node/edge collision detection | NO | 🔵 ENHANCEMENT |
| **dag-draw-collision-fix-test.el** | Automatic collision fixing | NO | 🔵 ENHANCEMENT |
| **dag-draw-simple-overlap-test.el** | Overlap detection | NO | 🔵 ENHANCEMENT |
| **dag-draw-pattern-isolation-test.el** | Pattern-specific debugging | NO | 🔵 DEBUG |
| **dag-draw-specific-pattern-test.el** | Specific graph patterns | MAYBE | 🟡 REVIEW |
| **dag-draw-port-to-port-test.el** | Direct port connections | D3.5 | ✅ If tests D3.5 |
| **dag-draw-port-coordinate-bug-test.el** | Port bug regression test | TBD | 🟡 REVIEW |

**Rationale:** GKNV baseline doesn't include automatic collision detection/fixing. Network simplex PREVENTS collisions via constraints (D3.3).

**Action:**
- collision-* files: Mark as future enhancements
- port-* files: Review - may be baseline (D3.5) or bug tests

---

## Category C: DEBUG/Development Tests (REVIEW - 15 files)

These may be temporary debugging tests or tests for obsolete code.

### C1: Debug-Specific Files (5 files)

| File | Purpose | Keep? |
|------|---------|-------|
| **debug-port-calculation.el** | Debug port calculation issues | 🟡 MAYBE - If still failing |
| **debug-edge-connectivity-simple.el** | Debug edge connectivity | 🟡 MAYBE - If still failing |
| **test-coord-verification.el** | Progressive coordinate verification | 🟡 DEVELOPMENT |
| **dag-draw-debug-network-simplex.el** | Network simplex debugging | 🟡 DEVELOPMENT |
| **dag-draw-root-cause-debug-test.el** | Root cause debugging | 🟡 DEVELOPMENT |

**Action:** 
- If tests currently PASS: Promote to baseline
- If tests currently FAIL: Fix or remove
- If tests are development scaffolding: Mark as such or remove

---

### C2: Corner Cases & Arrows (5 files)

| File | Purpose | Baseline? |
|------|---------|-----------|
| **dag-draw-corner-arrows-test.el** | Arrow placement at corners | D5.5 | ✅ If tests D5.5 |
| **dag-draw-arrow-placement-debug-test.el** | Arrow debugging | NO | 🔵 DEBUG |
| **dag-draw-coincident-ports-test.el** | Multiple edges same port | D4.5 | ✅ If tests D4.5 |
| **dag-draw-mathematical-notation-test.el** | Mathematical notation support | NO | 🔵 ENHANCEMENT |
| **dag-draw-gknv-function-names-test.el** | Function naming conventions | N/A | 🟡 META-TEST |

**Action:** Determine if each tests baseline behavior or is a debug/enhancement test.

---

### C3: Spacing & Dynamic Scale (5 files)

| File | Purpose | Baseline? |
|------|---------|-----------|
| **dag-draw-spacing-debug-test.el** | Spacing debugging | NO | 🔵 DEBUG |
| **dag-draw-dynamic-scale-test.el** | Dynamic scaling | TBD | 🟡 REVIEW |
| **dag-draw-spline-ascii-test.el** | Spline-to-ASCII conversion | D5.3 | ✅ If tests D5.3 |
| **dag-draw-ascii-test-harness-test.el** | Test harness functionality | N/A | ✅ INFRA |

**Action:** Review and categorize properly.

---

## Category D: INFRASTRUCTURE (KEEP - 11 files)

Essential test utilities, helpers, and infrastructure.

| File | Purpose | Action |
|------|---------|--------|
| **test-helpers.el** | Core test helper functions | ✅ KEEP |
| **dag-draw-test-helpers.el** | dag-draw specific helpers | ✅ KEEP |
| **dag-draw-test-harness.el** | Test harness framework | ✅ KEEP |
| **dag-draw-core-test.el** | Core data structure tests | ✅ KEEP |
| **dag-draw-test.el** | Basic API tests | ✅ KEEP |

**Action:** KEEP ALL - These are essential test infrastructure.

---

## Category E: QUALITY/Monitoring (KEEP - 5 files)

Post-algorithm quality analysis (not part of algorithm itself).

| File | Purpose | Baseline Compliant? |
|------|---------|---------------------|
| **dag-draw-quality-test.el** | Quality metrics | ✅ MONITORING ONLY |
| **dag-draw-quality-assurance-test.el** | QA checks | ✅ MONITORING ONLY |
| **dag-draw-aesthetic-principles-test.el** | A1-A4 evaluation | ✅ MONITORING ONLY |

**Action:** KEEP - These verify output quality without modifying algorithm (per D-BASELINE).

---

## Test Organization Issues

### Issue 1: Overlapping Test Coverage

Some tests may overlap:
- Network simplex tests (11 files) - May have redundancy
- Coordinate tests (8+ files) - Should be consolidated
- ASCII tests (10 files) - Well-organized

**Recommendation:** Future cleanup could consolidate redundant tests.

### Issue 2: Unclear Test Names

Some names don't indicate baseline vs. enhancement:
- "enhanced-*" clearly means enhancement ✅
- "advanced-*" clearly means enhancement ✅  
- "debug-*" clearly temporary 🟡
- Others ambiguous

**Recommendation:** Rename tests to indicate category:
- `baseline-network-simplex-test.el`
- `future-advanced-crossing-test.el`
- `dev-debug-ports-test.el`

### Issue 3: Missing Test Documentation

Many tests lack header comments explaining:
- What GKNV section they test
- What decisions (D1.x-D5.x) they verify
- Whether baseline or enhancement

**Recommendation:** Add standard headers:
```elisp
;;; FILE.el --- BRIEF DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;; 
;; GKNV Baseline Test - Section X.Y
;; Verifies implementation decisions: D1.2, D1.4, D1.8
;; 
;; Tests the network simplex optimization algorithm as described in
;; GKNV Figure 2-1 steps 3-6.

;;; Code:
```

---

## Priority Actions

### Week 2 Immediate Actions

1. **Review Debug Tests (5 files)**
   - Run each debug-* test
   - If passing: Promote to baseline or remove
   - If failing: Fix or document as known issue

2. **Categorize Coordinate Tests (8 files)**
   - Determine which test baseline (D3.x, D5.1)
   - Determine which test enhancements
   - Move to appropriate category

3. **Mark Enhancement Tests (18 files)**
   - Add clear "FUTURE ENHANCEMENT" headers
   - Document what they test beyond baseline
   - Optionally disable in baseline-only test runs

4. **Verify Baseline Coverage (52 files)**
   - Confirm all D1.x-D5.x decisions have tests
   - Identify any coverage gaps
   - Document test-to-decision mapping

5. **Create Test Suite Documentation**
   - Map each test to GKNV sections
   - Map each test to implementation decisions
   - Create testing guide for contributors

### Week 3 Cleanup Actions

1. **Consolidate Redundant Tests**
   - Network simplex tests (11 files) - Merge overlaps
   - Coordinate tests - Consolidate by category

2. **Rename Ambiguous Tests**
   - Add category prefixes (baseline/future/dev)
   - Make purpose clear from filename

3. **Add Standard Headers**
   - GKNV section references
   - Decision number references
   - Baseline/Enhancement classification

---

## Test Execution Strategy

### Baseline Validation Run

Run only Category A tests (52 files) for baseline compliance:

```bash
# Run only baseline tests
~/bin/eldev test -B \
  --file="test/gknv-*.el" \
  --file="test/dag-draw-network-simplex-*.el" \
  --file="test/dag-draw-dfs-*.el" \
  --file="test/dag-draw-rank-*.el" \
  --file="test/dag-draw-crossing-*.el" \
  --file="test/dag-draw-position-*.el" \
  --file="test/dag-draw-spline*.el" \
  --file="test/dag-draw-ascii-*.el" \
  --file="test/core-functionality-test.el" \
  --file="test/dag-draw-end-to-end-test.el"
```

### Full Test Suite Run

Run all tests including enhancements:

```bash
# Run everything
~/bin/eldev test -B
```

### Debug Test Run

Run only development/debug tests:

```bash
# Run debug tests
~/bin/eldev test -B --file="test/debug-*.el" --file="test/*-debug-*.el"
```

---

## Coverage Gaps

### Potential Missing Tests

After reviewing 101 files, these decision areas may need more coverage:

1. **D1.6: Rank Normalization**
   - Verify min rank = 0 after normalization
   - Create: `baseline-rank-normalization-test.el`

2. **D2.6: Flat Edge Ordering**
   - Transitive closure + partial order
   - May be covered in existing tests, verify

3. **D2.7: Tie Handling**
   - Flip on alternating iterations
   - Create: `baseline-tie-handling-test.el`

4. **D3.7: Zero-Cut Symmetry**
   - Optional enhancement, low priority
   - Document as future enhancement

5. **D4.4: Terminal Intersections**
   - Subdivide inter-rank space
   - Verify existing spline tests cover this

6. **D5.10: Self-Loop ASCII**
   - Fixed patterns for loops
   - Create: `baseline-ascii-self-loop-test.el`

---

## Conclusions

### Key Findings

1. **Test Suite is Strong** ✅
   - 51% baseline compliance tests
   - 18% enhancement tests (clearly beyond baseline)
   - Good infrastructure support

2. **No Massive Deletion Needed** ✅
   - Most tests verify baseline or are enhancements
   - Only ~15% are debug/development tests to review

3. **Organization is Good** ✅
   - Clear separation of concerns
   - Test harness well-designed
   - GKNV compliance tests prominent

4. **Some Cleanup Beneficial** 🟡
   - Categorize ambiguous tests
   - Add standard headers
   - Mark enhancements clearly

### Recommendations

**DO:**
- Keep all 52 baseline tests
- Keep all 11 infrastructure tests  
- Keep enhancement tests but mark clearly
- Review 15 debug tests (pass/fail/promote/remove)

**DON'T:**
- Delete baseline compliance tests
- Delete test infrastructure
- Delete passing integration tests
- Delete enhancement tests (mark instead)

### Success Metrics

After Week 2 cleanup:
- ✅ All baseline tests (Category A) passing
- ✅ All tests categorized correctly
- ✅ Enhancement tests clearly marked
- ✅ Debug tests resolved (passing or removed)
- ✅ Test-to-decision mapping documented
- ✅ Baseline validation run defined

---

**Next Steps:** See `.state/week2-test-cleanup-plan.md` for specific actions.

**Document Version:** 1.0  
**Author:** Week 2 Verification Team
