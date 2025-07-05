;;; dag-draw-quality-test.el --- Tests for dynamic graph quality analysis -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Tests for dynamic rank separation calculation based on graph structure analysis.
;; These tests verify that spacing requirements are calculated dynamically based on
;; edge convergence patterns, node density, and routing complexity.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-quality)

;;; Basic Edge Analysis Tests

(describe "Edge convergence analysis"
  (it "should count edges between adjacent ranks"
    (let ((graph (dag-draw-create-graph)))
      ;; Create simple two-rank graph
      (dag-draw-add-node graph 'a "Node A")
      (dag-draw-add-node graph 'b "Node B") 
      (dag-draw-add-node graph 'c "Node C")
      (dag-draw-add-edge graph 'a 'b)
      (dag-draw-add-edge graph 'a 'c)
      
      ;; Set up ranks
      (dag-draw-rank-graph graph)
      
      (expect (dag-draw--count-edges-between-ranks graph 0 1) :to-equal 2)))

  (it "should detect maximum convergence on destination nodes"
    (let ((graph (dag-draw-create-graph)))
      ;; Create convergence pattern: 3 nodes → 1 node
      (dag-draw-add-node graph 'a "Node A")
      (dag-draw-add-node graph 'b "Node B")
      (dag-draw-add-node graph 'c "Node C")
      (dag-draw-add-node graph 'd "Node D")
      (dag-draw-add-edge graph 'a 'd)
      (dag-draw-add-edge graph 'b 'd)
      (dag-draw-add-edge graph 'c 'd)
      
      ;; Set up ranks
      (dag-draw-rank-graph graph)
      
      (expect (dag-draw--max-edges-to-same-destination graph 0 1) :to-equal 3)))

  (it "should calculate horizontal distances for edge routing"
    (let ((graph (dag-draw-create-graph)))
      ;; Create nodes with known positions
      (dag-draw-add-node graph 'left "Left Node")
      (dag-draw-add-node graph 'right "Right Node")
      (dag-draw-add-edge graph 'left 'right)
      
      ;; Set up ranks and positioning
      (dag-draw-rank-graph graph)
      (dag-draw-order-vertices graph)
      (dag-draw-position-nodes graph)
      
      (expect (dag-draw--max-horizontal-edge-distance graph 0 1) :to-be >= 0))))

;;; Dynamic Spacing Calculation Tests

(describe "Dynamic rank separation calculation"
  (it "should require minimal spacing for simple graphs"
    (let ((graph (dag-draw-create-graph)))
      ;; Single edge graph
      (dag-draw-add-node graph 'a "Node A")
      (dag-draw-add-node graph 'b "Node B")
      (dag-draw-add-edge graph 'a 'b)
      
      (dag-draw-rank-graph graph)
      
      (expect (dag-draw--calculate-dynamic-rank-separation graph 0 1) :to-equal 2)))

  (it "should require more spacing for high convergence graphs"
    (let ((graph (dag-draw-create-graph)))
      ;; High convergence: 4 nodes → 1 node
      (dag-draw-add-node graph 'a "Node A")
      (dag-draw-add-node graph 'b "Node B")
      (dag-draw-add-node graph 'c "Node C")
      (dag-draw-add-node graph 'd "Node D")
      (dag-draw-add-node graph 'target "Target")
      (dag-draw-add-edge graph 'a 'target)
      (dag-draw-add-edge graph 'b 'target)
      (dag-draw-add-edge graph 'c 'target)
      (dag-draw-add-edge graph 'd 'target)
      
      (dag-draw-rank-graph graph)
      
      (expect (dag-draw--calculate-dynamic-rank-separation graph 0 1) :to-be >= 4)))

  (it "should require more spacing for dense edge crossings"
    (let ((graph (dag-draw-create-graph)))
      ;; Many edges between ranks
      (dotimes (i 6)
        (dag-draw-add-node graph (intern (format "src%d" i)) (format "Source %d" i))
        (dag-draw-add-node graph (intern (format "dst%d" i)) (format "Dest %d" i))
        (dag-draw-add-edge graph (intern (format "src%d" i)) (intern (format "dst%d" i))))
      
      (dag-draw-rank-graph graph)
      
      (expect (dag-draw--calculate-dynamic-rank-separation graph 0 1) :to-be >= 3))))

;;; Integration Tests

(describe "Dynamic spacing integration"
  (it "should integrate with ASCII resolution preprocessing"
    (let ((graph (dag-draw-create-graph)))
      ;; Create complex convergence pattern
      (dag-draw-add-node graph 'research "Research")
      (dag-draw-add-node graph 'database "Database Design")
      (dag-draw-add-node graph 'api "API Design")
      (dag-draw-add-node graph 'backend "Backend Implementation")
      (dag-draw-add-edge graph 'research 'database)
      (dag-draw-add-edge graph 'research 'api)
      (dag-draw-add-edge graph 'database 'backend)
      (dag-draw-add-edge graph 'api 'backend)
      
      (dag-draw-rank-graph graph)
      
      ;; Test that dynamic calculation affects ASCII resolution
      (let ((spacing-info (dag-draw--calculate-min-ascii-routing-space graph)))
        (expect (plist-get spacing-info :min-vertical) :to-be >= 3))))

  (it "should optimize spacing for the complex dependency graph"
    (let ((graph (dag-draw-create-graph)))
      ;; Recreate the complex test case structure
      (dag-draw-add-node graph 'research "Research")
      (dag-draw-add-node graph 'database "Database Design")
      (dag-draw-add-node graph 'api "API Design")
      (dag-draw-add-node graph 'infrastructure "Infrastructure Setup")
      (dag-draw-add-node graph 'backend "Backend Implementation")
      (dag-draw-add-node graph 'frontend "Frontend Implementation")
      (dag-draw-add-node graph 'integration "Integration Testing")
      (dag-draw-add-node graph 'deployment "Deployment")
      
      ;; Add complex edge pattern
      (dag-draw-add-edge graph 'research 'database)
      (dag-draw-add-edge graph 'research 'api)
      (dag-draw-add-edge graph 'research 'infrastructure)
      (dag-draw-add-edge graph 'database 'backend)
      (dag-draw-add-edge graph 'api 'backend)
      (dag-draw-add-edge graph 'api 'frontend)
      (dag-draw-add-edge graph 'backend 'integration)
      (dag-draw-add-edge graph 'frontend 'integration)
      (dag-draw-add-edge graph 'integration 'deployment)
      
      (dag-draw-rank-graph graph)
      
      ;; Test that spacing is optimized for this complex pattern
      (let ((max-spacing (dag-draw--calculate-max-required-rank-separation graph)))
        (expect max-spacing :to-be >= 2)
        (expect max-spacing :to-be <= 6))))  ; Should be reasonable, not excessive

  (it "should produce hollow routing without excessive spacing"
    (let ((graph (dag-draw-create-graph)))
      ;; Simple case that should need minimal spacing
      (dag-draw-add-node graph 'a "Node A")
      (dag-draw-add-node graph 'b "Node B")
      (dag-draw-add-edge graph 'a 'b)
      
      ;; Full layout
      (dag-draw-layout-graph graph)
      
      ;; Render and check spacing is reasonable
      (let ((ascii-output (dag-draw-render-graph graph 'ascii)))
        ;; Should have some empty rows for hollow routing but not excessive
        (expect (length (split-string ascii-output "\n")) :to-be <= 15)))))

;;; Edge Case Tests

(describe "Edge case handling"
  (it "should handle empty graphs"
    (let ((graph (dag-draw-create-graph)))
      (expect (dag-draw--calculate-max-required-rank-separation graph) :to-equal 2)))

  (it "should handle single node graphs"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'single "Single Node")
      (expect (dag-draw--calculate-max-required-rank-separation graph) :to-equal 2)))

  (it "should handle graphs with no edges"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'a "Node A")
      (dag-draw-add-node graph 'b "Node B")
      (expect (dag-draw--calculate-max-required-rank-separation graph) :to-equal 2))))

(provide 'dag-draw-quality-test)

;;; dag-draw-quality-test.el ends here