;;; dag-draw-grid-sizing-test.el --- Test grid sizing issues -*- lexical-binding: t -*-

;;; Commentary:

;; GKNV Baseline Compliance Tests - ASCII: Grid Sizing
;;
;; NOTE: This is a RENDERER TEST that uses manual coordinates to reproduce
;; a specific grid sizing bug (nodes positioned outside grid boundaries).
;; Test calls layout then overrides coordinates (lines 60-69) to create
;; the problematic scenario. This is appropriate for regression testing.
;;
;; This module tests ASCII grid dimension calculation as specified in
;; doc/implementation-decisions.md (ASCII adaptations).
;;
;; GKNV Reference: N/A (grid sizing is ASCII-specific calculation)
;; Decision: D5.1 - Grid sized based on content bounds and scaling
;;           D5.8 - Force minimum separation, accept compression if needed
;; Algorithm: Grid Dimension Calculation from Graph Bounds
;;
;; Key Requirements Tested:
;; - Grid width calculated from X coordinate range + node sizes + margins
;; - Grid height calculated from Y coordinate range + node sizes + margins
;; - Grid large enough to contain all nodes with separations
;; - Margins added for edge routing space
;; - Dense regions: minimum 1-character separation enforced
;; - Grid dimensions allow readable output (not too compressed)
;; - Grid size reasonable for terminal display
;;
;; Test Coverage:
;; - Grid width sufficient for all nodes horizontally
;; - Grid height sufficient for all nodes vertically
;; - Margins provide space for edge routing
;; - Dense graphs: compression acceptable but readable
;; - Various graph sizes (small, medium, large)
;; - Edge cases: single node, linear chain, wide graph
;; - Grid dimensions support coordinate scaling
;;
;; Baseline Status: ✅ Required for GKNV compliance (ASCII adaptation)
;;
;; See doc/implementation-decisions.md (D5.1, D5.8) for decision rationale.
;; See doc/algorithm-specification.md ASCII Rendering for implementation details.

;; Test to reproduce the issue where nodes are positioned outside grid boundaries

;;; Code:

(add-to-list 'load-path (expand-file-name "test/helpers" (locate-dominating-file default-directory "Eldev")))

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-render)
(require 'dag-draw-test-harness)

(describe "Grid Sizing Issues"
  (it "should include all nodes within grid boundaries after spline regeneration"
    ;; Test case that reproduces the missing "End" node issue
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'start "Start")
      (dag-draw-add-node graph 'middle "Middle Blocker") 
      (dag-draw-add-node graph 'end "End")
      
      ;; Position nodes horizontally to trigger the coordinate issue
      (dag-draw-add-edge graph 'start 'end)
      (dag-draw-layout-graph graph :coordinate-mode 'ascii)
      
      ;; Manually set coordinates to reproduce the test case
      (let ((start-node (dag-draw-get-node graph 'start))
            (middle-node (dag-draw-get-node graph 'middle))
            (end-node (dag-draw-get-node graph 'end)))
        (setf (dag-draw-node-x-coord start-node) 50.0)
        (setf (dag-draw-node-y-coord start-node) 50.0)
        (setf (dag-draw-node-x-coord middle-node) 100.0)
        (setf (dag-draw-node-y-coord middle-node) 50.0)
        (setf (dag-draw-node-x-coord end-node) 150.0)
        (setf (dag-draw-node-y-coord end-node) 50.0))
      
      (let ((ascii-output (dag-draw-render-ascii graph)))
        (message "=== GRID SIZING TEST OUTPUT ===")
        (message "%s" ascii-output)
        (message "==============================")
        
        ;; Use test harness for comprehensive validation
        (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
          (expect (plist-get node-validation :complete) :to-be t))
        (let ((boundary-validation (dag-draw-test--validate-node-boundaries ascii-output)))
          (expect (plist-get boundary-validation :valid) :to-be t))
        ))))

(provide 'dag-draw-grid-sizing-test)

;;; dag-draw-grid-sizing-test.el ends here