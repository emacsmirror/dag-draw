;;; dag-draw-boundary-violation-test.el --- Test for edge-through-node boundary violations -*- lexical-binding: t -*-

;;; Commentary:

;; GKNV Baseline Compliance Tests - Pass 4: Boundary Violation Detection
;;
;; This module tests GKNV spline boundary violation detection as specified in
;; "A Technique for Drawing Directed Graphs" (Gansner, Koutsofios, North, Vo).
;;
;; GKNV Reference: Section 5.2 (spline fitting within region boundaries)
;; Decision: D4.3 - Splines must respect region boundaries
;; Algorithm: Boundary Violation Detection and Correction
;;
;; Key Requirements Tested:
;; - Spline curve must stay within region boxes
;; - Violation detection: check if spline crosses box boundaries
;; - Correction strategies: straighten or subdivide
;; - Single-segment path: iteratively straighten spline
;; - Multi-segment path: refine curvature, then subdivide if needed
;; - Final spline guaranteed within boundaries (no overlaps)
;; - Violation detection enables correction loop
;;
;; Test Coverage:
;; - Boundary violation detection correct
;; - Splines that fit pass validation
;; - Splines that don't fit detected
;; - Correction strategies invoked appropriately
;; - Final splines within boundaries
;; - Various region shapes and spline curves
;; - Edge cases: tight regions, complex paths
;;
;; Baseline Status: ✅ Required for GKNV compliance
;;
;; See doc/implementation-decisions.md (D4.3) for full decision rationale.
;; See doc/algorithm-specification.md Pass 4 for implementation details.

;; Test to reproduce the specific boundary violation pattern │.*[─┼].*│

;;; Code:

(add-to-list 'load-path (expand-file-name "test/helpers" (locate-dominating-file default-directory "Eldev")))

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-render)
(require 'dag-draw-test-harness)

(describe "Edge Boundary Violation Tests"
  (it "should not draw edges through node interiors (reproduces │.*[─┼].*│ pattern)"
    ;; Create a complex graph that matches the failing case structure
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'research "Research")
      (dag-draw-add-node graph 'database "Database Design")
      (dag-draw-add-node graph 'api "API Design")
      (dag-draw-add-node graph 'infrastructure "Infrastructure Setup")
      (dag-draw-add-node graph 'backend "Backend Implementation")
      (dag-draw-add-node graph 'frontend "Frontend Implementation")
      (dag-draw-add-node graph 'integration "Integration Testing")
      (dag-draw-add-node graph 'deployment "Deployment")

      ;; Create the edge structure that triggers the boundary violation
      (dag-draw-add-edge graph 'research 'database)
      (dag-draw-add-edge graph 'research 'api)
      (dag-draw-add-edge graph 'research 'infrastructure)
      (dag-draw-add-edge graph 'database 'backend)
      (dag-draw-add-edge graph 'api 'backend)
      (dag-draw-add-edge graph 'api 'frontend)
      (dag-draw-add-edge graph 'backend 'integration)
      (dag-draw-add-edge graph 'frontend 'integration)
      (dag-draw-add-edge graph 'integration 'deployment)
      (dag-draw-layout-graph graph)
      
      (let ((ascii-output (dag-draw-render-ascii graph)))
        (message "=== BOUNDARY VIOLATION TEST OUTPUT ===")
        (message "%s" ascii-output)
        (message "======================================")
        
        ;; Use test harness for comprehensive validation
        (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
          (expect (plist-get node-validation :complete) :to-be t))
        (let ((boundary-validation (dag-draw-test--validate-node-boundaries ascii-output)))
          (expect (plist-get boundary-validation :valid) :to-be t))
        (let ((structure-validation (dag-draw-test--validate-graph-structure ascii-output graph)))
          (expect (plist-get structure-validation :topology-match) :to-be t))))))

(provide 'dag-draw-boundary-violation-test)

;;; dag-draw-boundary-violation-test.el ends here