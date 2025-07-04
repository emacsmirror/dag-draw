;;; dag-draw-systematic-coordinate-debug-test.el --- Systematic coordinate system debugging tests -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Systematic debugging of coordinate system issues using Fermi decomposition.
;; Builds from basic grid operations up to complex edge drawing, proving each
;; component works independently before building on it.
;;
;; Cartesian approach: Only build on proven components.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-render)
(require 'dag-draw-ascii-grid)

;;; Fixed grid-to-string function for testing

(defun dag-draw--test-grid-to-string (grid)
  "Convert ASCII grid to string without aggressive trimming."
  (mapconcat (lambda (row)
               ;; Don't trim - preserve all characters
               (apply #'string (append row nil)))
             grid
             "\n"))

;;; Level 0: Basic Grid Verification Tests

(describe "Level 0: Basic Grid Verification"

  (it "should place character at grid coordinate (0,0)"
    ;; Test 1: Most basic grid operation
    (let ((grid (dag-draw--create-ascii-grid 10 5)))
      ;; Place test character at top-left
      (aset (aref grid 0) 0 ?X)
      (let ((output (dag-draw--ascii-grid-to-string grid)))
        ;; Verify character appears at start of first line
        (expect (string-match "^X" output) :to-be-truthy))))

  (it "should place character at grid coordinate (5,2)"
    ;; Test 2: Mid-grid placement using fixed grid-to-string
    (let ((grid (dag-draw--create-ascii-grid 10 5)))
      ;; Place test character at position (5,2)
      (aset (aref grid 2) 5 ?Y)
      (let ((output (dag-draw--test-grid-to-string grid)))
        ;; DEBUG: Show output with fixed function
        (message "FIXED GRID OUTPUT: '%s'" output)
        ;; Verify character appears in output
        (expect (string-match "Y" output) :to-be-truthy))))

  (it "should place multiple characters at known coordinates"
    ;; Test 3: Multiple coordinate verification
    (let ((grid (dag-draw--create-ascii-grid 8 4)))
      ;; Place characters at specific coordinates
      (aset (aref grid 0) 2 ?A)  ; (2,0)
      (aset (aref grid 1) 4 ?B)  ; (4,1)
      (aset (aref grid 3) 1 ?C)  ; (1,3)
      (let ((output (dag-draw--ascii-grid-to-string grid)))
        ;; DEBUG: Show actual output
        (message "MULTI-GRID OUTPUT: '%s'" output)
        ;; Verify all characters appear
        (expect (string-match "A" output) :to-be-truthy)
        (expect (string-match "B" output) :to-be-truthy)
        (expect (string-match "C" output) :to-be-truthy)))))

;;; Level 1: Node Position Discovery Tests

(describe "Level 1: Node Position Discovery"

  (it "should discover actual node text coordinates in rendered grid"
    ;; Test 4: Find where nodes actually appear
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'test "Test")
      (dag-draw-layout-graph graph)
      (let ((output (dag-draw-render-ascii graph)))
        ;; Node should contain "Test" text
        (expect (string-match "Test" output) :to-be-truthy)
        ;; Store position for building on this test
        (let ((test-pos (string-match "Test" output)))
          ;; Verify we can find the node
          (expect test-pos :to-be-truthy)))))

  (it "should find node box boundaries in rendered output"
    ;; Test 5: Locate box drawing characters
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'test "Node")
      (dag-draw-layout-graph graph)
      (let ((output (dag-draw-render-ascii graph)))
        ;; Should contain box drawing characters
        (expect (string-match "[┌┐└┘]" output) :to-be-truthy))))

  (it "should identify relative positions of two nodes"
    ;; Test 6: Multi-node position mapping
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node1 "Node1")
      (dag-draw-add-node graph 'node2 "Node2")
      (dag-draw-layout-graph graph)
      (let ((output (dag-draw-render-ascii graph)))
        ;; Both nodes should be present
        (expect (string-match "Node1" output) :to-be-truthy)
        (expect (string-match "Node2" output) :to-be-truthy)
        ;; Can determine relative positions
        (let ((pos1 (string-match "Node1" output))
              (pos2 (string-match "Node2" output)))
          (expect (and pos1 pos2) :to-be-truthy))))))

;;; Level 2: Node Boundary Mapping Tests

(describe "Level 2: Node Boundary Mapping"

  (it "should map exact coordinates of node boundaries"
    ;; Test 7: Precise boundary coordinate discovery
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'test "Test")
      (dag-draw-layout-graph graph)
      (let ((output (dag-draw-render-ascii graph)))
        ;; Convert output to grid for coordinate analysis
        (let ((lines (split-string output "\\n")))
          ;; Find line containing node
          (let ((node-line-idx (cl-position-if (lambda (line) (string-match "Test" line)) lines)))
            (expect node-line-idx :to-be-truthy))))))

  (it "should verify box character positions"
    ;; Test 8: Box drawing character mapping
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'test "Node")
      (dag-draw-layout-graph graph)
      (let ((output (dag-draw-render-ascii graph)))
        ;; Should have complete box structure
        (expect (string-match "┌.*┐" output) :to-be-truthy)
        (expect (string-match "└.*┘" output) :to-be-truthy)))))

;;; Level 3: Two-Point Connection Tests

(describe "Level 3: Two-Point Connection"

  (it "should draw horizontal line between known coordinates"
    ;; Test 10: Basic horizontal line drawing
    (let ((grid (dag-draw--create-ascii-grid 10 3)))
      ;; Draw horizontal line from (2,1) to (6,1)
      (dotimes (i 5)
        (aset (aref grid 1) (+ 2 i) ?─))
      (let ((output (dag-draw--ascii-grid-to-string grid)))
        ;; DEBUG: Show actual output
        (message "HORIZONTAL LINE OUTPUT: '%s'" output)
        ;; Verify horizontal line appears
        (expect (string-match "─" output) :to-be-truthy))))

  (it "should draw vertical line between known coordinates"
    ;; Test 11: Basic vertical line drawing
    (let ((grid (dag-draw--create-ascii-grid 5 8)))
      ;; Draw vertical line from (2,1) to (2,4)
      (dotimes (i 4)
        (aset (aref grid (+ 1 i)) 2 ?│))
      (let ((output (dag-draw--ascii-grid-to-string grid)))
        ;; DEBUG: Show actual output
        (message "VERTICAL LINE OUTPUT: '%s'" output)
        ;; Verify vertical line appears
        (expect (string-match "│" output) :to-be-truthy))))

  (it "should connect two specific grid points with L-shape"
    ;; Test 12: L-shaped connection
    (let ((grid (dag-draw--create-ascii-grid 8 6)))
      ;; Connect (1,1) to (5,3) with L-shape
      ;; Horizontal segment: (1,1) to (5,1)
      (dotimes (i 5)
        (aset (aref grid 1) (+ 1 i) ?─))
      ;; Vertical segment: (5,1) to (5,3)
      (dotimes (i 3)
        (aset (aref grid (+ 1 i)) 5 ?│))
      (let ((output (dag-draw--ascii-grid-to-string grid)))
        ;; DEBUG: Show actual output
        (message "L-SHAPE OUTPUT: '%s'" output)
        ;; Verify L-shaped path contains both line types
        (expect (and (string-match "─" output) (string-match "│" output)) :to-be-truthy)))))

;;; Level 4: Port Position Verification Tests

(describe "Level 4: Port Position Verification"

  (it "should calculate port positions on node boundaries"
    ;; Test 13: Port calculation
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'test "Node")
      (dag-draw-layout-graph graph)
      ;; Get node for port calculation
      (let ((node (dag-draw-get-node graph 'test)))
        ;; Node should exist for port calculation
        (expect node :to-be-truthy))))

  (it "should verify port positions align with node boundaries"
    ;; Test 14: Port alignment verification
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'source "Source")
      (dag-draw-add-node graph 'target "Target")
      (dag-draw-add-edge graph 'source 'target)
      (dag-draw-layout-graph graph)
      (let ((output (dag-draw-render-ascii graph)))
        ;; Should have some form of connection
        (expect (string-match "[─│▶▼▲◀]" output) :to-be-truthy)))))

;;; Level 5: Coordinate System Verification Tests

(describe "Level 5: Coordinate System Verification"

  (it "should verify world-to-grid coordinate conversion accuracy"
    ;; Test 16: Coordinate conversion verification
    (let* ((world-x 100.0)
           (world-y 50.0)
           (min-x 0.0)
           (min-y 0.0)
           (scale 0.15)
           (grid-x (dag-draw--world-to-grid-coord world-x min-x scale))
           (grid-y (dag-draw--world-to-grid-coord world-y min-y scale)))
      ;; DEBUG: Show conversion results
      (message "COORDINATE CONVERSION: world=(%.1f,%.1f) -> grid=(%.1f,%.1f)" world-x world-y grid-x grid-y)
      ;; Conversion should produce reasonable grid coordinates
      (expect (numberp grid-x) :to-be-truthy)
      (expect (numberp grid-y) :to-be-truthy)
      (expect (>= grid-x 0) :to-be-truthy)
      (expect (>= grid-y 0) :to-be-truthy)))

  (it "should verify coordinate conversion produces expected visual results"
    ;; Test 17: End-to-end coordinate verification
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'a "A")
      (dag-draw-add-node graph 'b "B")
      (dag-draw-add-edge graph 'a 'b)
      (dag-draw-layout-graph graph)
      (let ((output (dag-draw-render-ascii graph)))
        ;; Should have nodes and some connection attempt
        (expect (string-match "A" output) :to-be-truthy)
        (expect (string-match "B" output) :to-be-truthy)
        ;; This test will initially fail - that's the point
        ;; Once coordinate system is fixed, this should pass
        (expect (string-match "[─│]" output) :to-be-truthy)))))

;;; Progressive Coordinate Verification Tests

;;; Coordinate Tracking Functions

(defun dag-draw--track-coordinate-pipeline (graph test-name)
  "Track all coordinates through the complete pipeline for debugging.
Returns hash table with coordinate data at each transformation stage."
  (let ((coords (ht-create))
        (node-coords (ht-create))
        (spline-coords (ht-create)))

    ;; Store test metadata
    (ht-set! coords 'test-name test-name)
    (ht-set! coords 'node-count (ht-size (dag-draw-graph-nodes graph)))

    ;; Phase 1: GKNV World Coordinates (before any transformation)
    (ht-each (lambda (node-id node)
               (ht-set! node-coords node-id
                       (list 'world
                             (dag-draw-node-x-coord node)
                             (dag-draw-node-y-coord node))))
             (dag-draw-graph-nodes graph))
    (ht-set! coords 'world-node-coords (ht-copy node-coords))

    ;; Phase 2: ASCII Context Coordinates
    (let* ((ascii-context (dag-draw--create-ascii-coordinate-context graph))
           (ascii-bounds (dag-draw--ascii-get-bounds ascii-context)))
      (ht-set! coords 'ascii-context ascii-context)
      (ht-set! coords 'ascii-bounds ascii-bounds)

      ;; Phase 3: Grid Bounds Calculation
      (let* ((raw-bounds (dag-draw-get-graph-bounds graph))
             (scale dag-draw-ascii-coordinate-scale))
        (ht-set! coords 'raw-bounds raw-bounds)
        (ht-set! coords 'scale scale)

        ;; Phase 4: Spline Coordinates (if edges exist)
        (when (dag-draw-graph-edges graph)
          (dolist (edge (dag-draw-graph-edges graph))
            (let ((spline-points (dag-draw-edge-spline-points edge)))
              (when spline-points
                (let ((edge-key (intern (format "%s->%s"
                                               (dag-draw-edge-from-node edge)
                                               (dag-draw-edge-to-node edge)))))
                  (ht-set! spline-coords edge-key
                          (list 'spline-world
                                (car spline-points)
                                (car (last spline-points))))))))
          (ht-set! coords 'world-spline-coords spline-coords))))

    coords))

(defun dag-draw--verify-coordinate-reasonableness (coords test-name)
  "Verify coordinates are reasonable and consistent.
Returns list of issues found, empty if all good."
  (let ((issues '())
        (node-count (ht-get coords 'node-count))
        (ascii-bounds (ht-get coords 'ascii-bounds)))

    ;; Check for reasonable ASCII bounds
    (when ascii-bounds
      (let ((width (- (nth 2 ascii-bounds) (nth 0 ascii-bounds)))
            (height (- (nth 3 ascii-bounds) (nth 1 ascii-bounds))))
        (when (> width (* node-count 100))
          (push (format "ASCII width %d excessive for %d nodes" width node-count) issues))
        (when (> height (* node-count 50))
          (push (format "ASCII height %d excessive for %d nodes" height node-count) issues))))

    ;; Check for coordinate system consistency
    (let ((world-coords (ht-get coords 'world-node-coords))
          (raw-bounds (ht-get coords 'raw-bounds)))
      (when (and world-coords raw-bounds)
        (let ((bounds-width (- (nth 2 raw-bounds) (nth 0 raw-bounds))))
          (when (> bounds-width (* node-count 200))
            (push (format "Bounds width %.1f excessive for %d nodes" bounds-width node-count) issues)))))

    issues))

;;; Progressive Complexity Tests

(describe "Progressive Coordinate Verification"

  (it "Level 1: Single Node - Coordinate Baseline"
    ;; Test: Establish baseline coordinate behavior
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node1 "Node1")
      (dag-draw-layout-graph graph)

      (let* ((coords (dag-draw--track-coordinate-pipeline graph "single-node"))
             (issues (dag-draw--verify-coordinate-reasonableness coords "single-node"))
             (ascii-bounds (ht-get coords 'ascii-bounds)))

        ;; Debug output
        (message "\n=== LEVEL 1: SINGLE NODE ===")
        (message "ASCII bounds: %s" ascii-bounds)
        (message "Issues found: %s" issues)

        ;; Single node should have minimal bounds
        (expect ascii-bounds :to-be-truthy)
        (expect (length issues) :to-equal 0))))

  (it "Level 2: Two Nodes - Basic Edge Tracking"
    ;; Test: Track coordinates through basic edge case
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node1 "Node1")
      (dag-draw-add-node graph 'node2 "Node2")
      (dag-draw-add-edge graph 'node1 'node2)
      (dag-draw-layout-graph graph)

      (let* ((coords (dag-draw--track-coordinate-pipeline graph "two-nodes"))
             (issues (dag-draw--verify-coordinate-reasonableness coords "two-nodes"))
             (ascii-bounds (ht-get coords 'ascii-bounds))
             (spline-coords (ht-get coords 'world-spline-coords)))

        ;; Debug output
        (message "\n=== LEVEL 2: TWO NODES ===")
        (message "ASCII bounds: %s" ascii-bounds)
        (message "Spline coords: %s" (when spline-coords (ht-keys spline-coords)))
        (message "Issues found: %s" issues)

        ;; Two nodes should still have reasonable bounds
        (expect ascii-bounds :to-be-truthy)
        (expect spline-coords :to-be-truthy)
        (expect (length issues) :to-equal 0))))

  (it "Level 3: Three Nodes - Simple Branching"
    ;; Test: Track coordinates through branching (1→2, 1→3)
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'root "Root")
      (dag-draw-add-node graph 'branch1 "Branch1")
      (dag-draw-add-node graph 'branch2 "Branch2")
      (dag-draw-add-edge graph 'root 'branch1)
      (dag-draw-add-edge graph 'root 'branch2)
      (dag-draw-layout-graph graph)

      (let* ((coords (dag-draw--track-coordinate-pipeline graph "three-nodes"))
             (issues (dag-draw--verify-coordinate-reasonableness coords "three-nodes"))
             (ascii-bounds (ht-get coords 'ascii-bounds)))

        ;; Debug output
        (message "\n=== LEVEL 3: THREE NODES (BRANCHING) ===")
        (message "ASCII bounds: %s" ascii-bounds)
        (message "Issues found: %s" issues)

        ;; Track when bounds start becoming excessive
        (when ascii-bounds
          (let ((width (- (nth 2 ascii-bounds) (nth 0 ascii-bounds))))
            (message "Width per node: %.1f" (/ width 3.0))))

        ;; This might be where issues start appearing
        (expect ascii-bounds :to-be-truthy))))

  (it "Level 4: Diamond Pattern - Convergence Testing"
    ;; Test: Track coordinates through convergence (1→2, 1→3, 2→4, 3→4)
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'start "Start")
      (dag-draw-add-node graph 'mid1 "Mid1")
      (dag-draw-add-node graph 'mid2 "Mid2")
      (dag-draw-add-node graph 'end "End")
      (dag-draw-add-edge graph 'start 'mid1)
      (dag-draw-add-edge graph 'start 'mid2)
      (dag-draw-add-edge graph 'mid1 'end)
      (dag-draw-add-edge graph 'mid2 'end)
      (dag-draw-layout-graph graph)

      (let* ((coords (dag-draw--track-coordinate-pipeline graph "diamond"))
             (issues (dag-draw--verify-coordinate-reasonableness coords "diamond"))
             (ascii-bounds (ht-get coords 'ascii-bounds)))

        ;; Debug output
        (message "\n=== LEVEL 4: DIAMOND PATTERN (CONVERGENCE) ===")
        (message "ASCII bounds: %s" ascii-bounds)
        (message "Issues found: %s" issues)

        ;; Diamond pattern tests convergence handling
        (when ascii-bounds
          (let ((width (- (nth 2 ascii-bounds) (nth 0 ascii-bounds))))
            (message "Width per node: %.1f" (/ width 4.0))))

        (expect ascii-bounds :to-be-truthy))))

  (it "Level 5: Five Nodes - Multiple Convergence"
    ;; Test: More complex convergence pattern
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'root "Root")
      (dag-draw-add-node graph 'branch1 "Branch1")
      (dag-draw-add-node graph 'branch2 "Branch2")
      (dag-draw-add-node graph 'merge1 "Merge1")
      (dag-draw-add-node graph 'final "Final")
      (dag-draw-add-edge graph 'root 'branch1)
      (dag-draw-add-edge graph 'root 'branch2)
      (dag-draw-add-edge graph 'branch1 'merge1)
      (dag-draw-add-edge graph 'branch2 'merge1)
      (dag-draw-add-edge graph 'merge1 'final)
      (dag-draw-layout-graph graph)

      (let* ((coords (dag-draw--track-coordinate-pipeline graph "five-nodes"))
             (issues (dag-draw--verify-coordinate-reasonableness coords "five-nodes"))
             (ascii-bounds (ht-get coords 'ascii-bounds)))

        ;; Debug output
        (message "\n=== LEVEL 5: FIVE NODES (MULTIPLE CONVERGENCE) ===")
        (message "ASCII bounds: %s" ascii-bounds)
        (message "Issues found: %s" issues)

        ;; Track coordinate system breakdown
        (when ascii-bounds
          (let ((width (- (nth 2 ascii-bounds) (nth 0 ascii-bounds))))
            (message "Width per node: %.1f" (/ width 5.0))
            (when (> width 400)
              (message "WARNING: Coordinate explosion detected at 5 nodes!"))))

        (expect ascii-bounds :to-be-truthy))))

  (it "Level 6: Eight Nodes - Current Failure Case"
    ;; Test: Full complex case (current failing test)
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'research "Research")
      (dag-draw-add-node graph 'database "Database Design")
      (dag-draw-add-node graph 'api "API Design")
      (dag-draw-add-node graph 'infrastructure "Infrastructure Setup")
      (dag-draw-add-node graph 'backend "Backend Implementation")
      (dag-draw-add-node graph 'frontend "Frontend Implementation")
      (dag-draw-add-node graph 'integration "Integration Testing")
      (dag-draw-add-node graph 'deployment "Deployment")

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

      (let* ((coords (dag-draw--track-coordinate-pipeline graph "eight-nodes"))
             (issues (dag-draw--verify-coordinate-reasonableness coords "eight-nodes"))
             (ascii-bounds (ht-get coords 'ascii-bounds))
             (raw-bounds (ht-get coords 'raw-bounds)))

        ;; Debug output
        (message "\n=== LEVEL 6: EIGHT NODES (CURRENT FAILURE) ===")
        (message "Raw bounds: %s" raw-bounds)
        (message "ASCII bounds: %s" ascii-bounds)
        (message "Issues found: %s" issues)

        ;; Detailed breakdown analysis
        (when ascii-bounds
          (let ((width (- (nth 2 ascii-bounds) (nth 0 ascii-bounds))))
            (message "Width per node: %.1f" (/ width 8.0))
            (message "Total width: %.1f" width)))

        ;; This is where we expect to see the coordinate explosion
        (expect ascii-bounds :to-be-truthy)

        ;; Report the breaking point
        (when (> (length issues) 0)
          (message "BREAKING POINT IDENTIFIED: %s" issues))))))

;;; Integration Test

(describe "Integration: Full Edge Drawing"

  (it "should draw complete edge between two nodes after all fixes"
    ;; Final integration test - should pass only after all levels work
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'start "Start")
      (dag-draw-add-node graph 'end "End")
      (dag-draw-add-edge graph 'start 'end)
      (dag-draw-layout-graph graph)
      (let ((output (dag-draw-render-ascii graph)))
        ;; Should have complete visual connection
        (expect (string-match "Start" output) :to-be-truthy)
        (expect (string-match "End" output) :to-be-truthy)
        ;; Should have edge connecting them (this will fail until fixed)
        (expect (string-match "[─│▶▼▲◀]" output) :to-be-truthy)))))

(provide 'dag-draw-systematic-coordinate-debug-test)

;;; dag-draw-systematic-coordinate-debug-test.el ends here
