;;; dag-draw-ascii-edge-routing-test.el --- Acceptance tests for ASCII edge routing -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Acceptance tests that demonstrate the difference between current (broken)
;; ASCII edge routing and expected (proper) ASCII edge routing following
;; GKNV algorithm principles.
;;
;; These tests define the visual quality standards we're aiming for:
;; - Edges connect to node boundaries, not centers
;; - Clean orthogonal routing with box-drawing characters
;; - Integration with existing spline system
;; - Professional appearance matching GKNV algorithm expectations

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-render)
(require 'dag-draw-test-harness)

(describe
 "ASCII Edge Routing Acceptance Tests"

 (describe
  "Current State (Broken) - Edge Routing Problems"

  (it "demonstrates current broken edge routing from node centers"
      (let ((graph (dag-draw-create-graph)))
        ;; Create simple two-node graph
        (dag-draw-add-node graph 'top "Top")
        (dag-draw-add-node graph 'bottom "Bottom")
        (dag-draw-add-edge graph 'top 'bottom)

        ;; Set explicit coordinates to make test deterministic
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'top)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'top)) 50)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'bottom)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'bottom)) 150)

        (let ((ascii-output (dag-draw-render-ascii graph)))
          ;; Current implementation should show these problems:
          ;; 1. Edge connects to node centers instead of boundaries
          ;; 2. Poor visual quality with lines overlapping boxes
          ;; 3. L-shaped routing instead of proper spline-based routing

          ;; For now, just verify we get output (will improve as we fix routing)
          (expect ascii-output :to-be-truthy)
          (expect (length ascii-output) :to-be-greater-than 10)

          ;; Document current problematic behavior
          (message "Current (broken) ASCII output:")
          (message "%s" ascii-output))))

  (it "shows complex graph edge routing chaos"
      (let ((graph (dag-draw-create-graph)))
        ;; Create diamond pattern that should show clean routing
        (dag-draw-add-node graph 'top "Top")
        (dag-draw-add-node graph 'left "Left")
        (dag-draw-add-node graph 'right "Right")
        (dag-draw-add-node graph 'bottom "Bottom")

        (dag-draw-add-edge graph 'top 'left)
        (dag-draw-add-edge graph 'top 'right)
        (dag-draw-add-edge graph 'left 'bottom)
        (dag-draw-add-edge graph 'right 'bottom)

        ;; Run full layout
        (dag-draw-layout-graph graph)

        (let ((ascii-output (dag-draw-render-ascii graph)))
          ;; Should demonstrate chaotic edge routing
          (expect ascii-output :to-be-truthy)

          ;; Document problems for comparison with fixed version
          (message "Complex graph - current broken routing:")
          (message "%s" ascii-output)))))

 (describe
  "Expected State (Target) - Proper Edge Routing"

  (it "should connect edges to node boundaries, not centers"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'source "Source")
        (dag-draw-add-node graph 'target "Target")
        (dag-draw-add-edge graph 'source 'target)

        ;; Position nodes vertically aligned
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'source)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'source)) 50)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'target)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'target)) 150)

        ;; Expected behavior:
        ;; 1. Edge should connect from bottom of source box to top of target box
        ;; 2. Should use clean vertical line with proper connection points
        ;; 3. Should NOT overlap with node interior

        (let ((ascii-output (dag-draw-render-ascii graph)))
          ;; Verify we get output
          (expect ascii-output :to-be-truthy)

          ;; Split into lines for analysis
          (let ((lines (split-string ascii-output "\n")))
            ;; Should have vertical line characters (│) connecting the boxes
            (expect (cl-some (lambda (line) (string-match-p "│" line)) lines) :to-be-truthy)

            ;; ALGORITHM STABILITY: Focus on structural correctness
            ;; Our conservative 0.08 box scale prioritizes algorithm stability over text display
            
            ;; Use test harness for comprehensive validation
            (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
              (expect (plist-get node-validation :complete) :to-be t))
            (let ((boundary-validation (dag-draw-test--validate-node-boundaries ascii-output)))
              (expect (plist-get boundary-validation :valid) :to-be t))
            (let ((connectivity-validation (dag-draw-test--validate-edge-connectivity ascii-output graph)))
              (expect (plist-get connectivity-validation :all-connected) :to-be t))
            
            ;; DEFER: Text-based node detection deferred until algorithm fully stable per CLAUDE.local.md
            ))))

  (it "should use orthogonal routing with box-drawing characters"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'left "Left")
        (dag-draw-add-node graph 'right "Right")
        (dag-draw-add-edge graph 'left 'right)

        ;; Position nodes horizontally
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'left)) 50)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'left)) 100)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'right)) 150)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'right)) 100)

        ;; Expected: horizontal line using ─ character from right edge of left box
        ;; to left edge of right box, with proper connection points

        (let ((ascii-output (dag-draw-render-ascii graph)))
          ;; Verify we get output
          (expect ascii-output :to-be-truthy)

          ;; Use test harness for comprehensive validation
          (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
            (expect (plist-get node-validation :complete) :to-be t))
          (let ((boundary-validation (dag-draw-test--validate-node-boundaries ascii-output)))
            (expect (plist-get boundary-validation :valid) :to-be t))
          (let ((connectivity-validation (dag-draw-test--validate-edge-connectivity ascii-output graph)))
            (expect (plist-get connectivity-validation :all-connected) :to-be t)))))

  (it "should integrate with spline system for curved routing"
      (let ((graph (dag-draw-create-graph)))
        ;; Create layout requiring routing around obstacles
        (dag-draw-add-node graph 'source "Source")
        (dag-draw-add-node graph 'obstacle "Obstacle")
        (dag-draw-add-node graph 'target "Target")

        (dag-draw-add-edge graph 'source 'target)

        ;; Position so edge must route around obstacle
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'source)) 50)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'source)) 50)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'obstacle)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'obstacle)) 75)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'target)) 150)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'target)) 100)

        ;; Expected: edge should route around obstacle using existing spline data
        ;; converted to ASCII orthogonal approximation

        ;; Run full layout to generate splines
        (dag-draw-layout-graph graph)

        (let ((ascii-output (dag-draw-render-ascii graph)))
          ;; ALGORITHM STABILITY: Test spline integration and obstacle avoidance
          ;; Our conservative 0.08 box scale prioritizes algorithm stability over text display
          
          ;; Should generate substantial output for 3-node obstacle avoidance scenario
          (expect ascii-output :to-be-truthy)
          (expect (length ascii-output) :to-be-greater-than 400)
          
          ;; Should have proper node boundaries for all three nodes
          (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
            (expect (plist-get node-validation :complete) :to-be t))
          
          ;; Should have routing lines demonstrating curved/obstacle avoidance
          (let ((boundary-validation (dag-draw-test--validate-node-boundaries ascii-output)))
            (expect (plist-get boundary-validation :valid) :to-be t))
          
          ;; Use test harness for comprehensive validation
          (let ((arrow-validation (dag-draw-test--validate-arrows ascii-output)))
            (expect (plist-get arrow-validation :valid-arrows) :to-be-greater-than 0))
          (let ((connectivity-validation (dag-draw-test--validate-edge-connectivity ascii-output graph)))
            (expect (plist-get connectivity-validation :all-connected) :to-be t))
          
          ;; DEFER: Text-based node matching deferred until algorithm fully stable per CLAUDE.local.md
          )))

  (it "should handle all three edge types properly"
      ;; Test inter-rank, flat, and self-edges as described in GKNV paper
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")

        ;; Inter-rank edge (most common)
        (dag-draw-add-edge graph 'a 'b)
        ;; Flat edge (same rank)
        (dag-draw-add-edge graph 'b 'c)
        ;; Self edge (loop)
        (dag-draw-add-edge graph 'c 'c)

        (dag-draw-layout-graph graph)

        ;; Expected: proper routing for each edge type
        ;; - Inter-rank: vertical with minimal bends
        ;; - Flat: horizontal routing
        ;; - Self: loop using curved ASCII approximation

        (let ((ascii-output (dag-draw-render-ascii graph)))
          ;; Verify we get output with all nodes
          (expect ascii-output :to-be-truthy)
          ;; Use test harness for node validation
          (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
            (expect (plist-get node-validation :complete) :to-be t))
          (let ((boundary-validation (dag-draw-test--validate-node-boundaries ascii-output)))
            (expect (plist-get boundary-validation :valid) :to-be t))

          ;; Verify the system handles different edge types without crashing
          ;; and produces clean routing for each type
          ;; Use test harness for comprehensive validation
          (let ((structure-validation (dag-draw-test--validate-graph-structure ascii-output graph)))
            (expect (plist-get structure-validation :topology-match) :to-be t)
            (expect (plist-get structure-validation :node-count-match) :to-be t)
            (expect (plist-get structure-validation :edge-count-match) :to-be t))
          (let ((connectivity-validation (dag-draw-test--validate-edge-connectivity ascii-output graph)))
            (expect (plist-get connectivity-validation :all-connected) :to-be t))))))

 (describe
  "Visual Quality Standards"

  (it "should produce professional-looking output"
      ;; This test defines our visual quality goals
      (let ((graph (dag-draw-create-graph)))
        ;; Create realistic software dependency graph
        (dag-draw-add-node graph 'ui "UI Layer")
        (dag-draw-add-node graph 'service "Service")
        (dag-draw-add-node graph 'data "Data Layer")

        (dag-draw-add-edge graph 'ui 'service)
        (dag-draw-add-edge graph 'service 'data)

        (dag-draw-layout-graph graph)

        ;; Expected output should look clean and professional:
        ;; ┌─────────┐
        ;; │UI Layer │
        ;; └─────────┘
        ;;      │
        ;;      ▼
        ;; ┌─────────┐
        ;; │Service  │
        ;; └─────────┘
        ;;      │
        ;;      ▼
        ;; ┌───────────┐
        ;; │Data Layer │
        ;; └───────────┘

        (let ((ascii-output (dag-draw-render-ascii graph)))
          ;; Basic output verification
          (expect ascii-output :to-be-truthy)

          ;; Use test harness for comprehensive validation
          (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
            (expect (plist-get node-validation :complete) :to-be t))
          (let ((boundary-validation (dag-draw-test--validate-node-boundaries ascii-output)))
            (expect (plist-get boundary-validation :valid) :to-be t))
          (let ((structure-validation (dag-draw-test--validate-graph-structure ascii-output graph)))
            (expect (plist-get structure-validation :topology-match) :to-be t)
            (expect (plist-get structure-validation :node-count-match) :to-be t))

          ;; Professional quality: clean connections between layers
          (let ((connectivity-validation (dag-draw-test--validate-edge-connectivity ascii-output graph)))
            (expect (plist-get connectivity-validation :all-connected) :to-be t))
          (let ((arrow-validation (dag-draw-test--validate-arrows ascii-output)))
            (expect (plist-get arrow-validation :valid-arrows) :to-be-greater-than 0)))))))

;;; dag-draw-ascii-edge-routing-test.el ends here
