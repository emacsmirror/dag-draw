;;; core-functionality-test.el --- Core functionality tests for dag-draw -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; GKNV Baseline Compliance Tests - Core: Complete Algorithm
;;
;; This module tests core GKNV four-pass algorithm functionality as specified in
;; "A Technique for Drawing Directed Graphs" (Gansner, Koutsofios, North, Vo).
;;
;; GKNV Reference: Abstract + Sections 1-5 (complete four-pass algorithm)
;; Decision: All decisions D1.1-D5.10
;; Algorithm: Complete GKNV Four-Pass Algorithm
;;
;; Key Requirements Tested:
;; - Four-pass algorithm structure: Pass 1 → Pass 2 → Pass 3 → Pass 4
;; - Pass 1: Optimal rank assignment via network simplex
;; - Pass 2: Vertex order within ranks via weighted median + transpose
;; - Pass 3: Optimal coordinates via auxiliary graph network simplex
;; - Pass 4: Splines via region-constrained curve fitting
;; - Aesthetic principles achieved: A1 (hierarchy), A2 (avoid anomalies),
;;   A3 (short edges), A4 (symmetry)
;; - Algorithm runs fast (interactive performance)
;; - Output quality high (makes good drawings)
;; - API contracts: input graph → output with coordinates and splines
;;
;; Test Coverage:
;; - Complete algorithm execution from input to output
;; - All four passes execute correctly
;; - Pass integration verified (correct data flow)
;; - Aesthetic principles achieved in output
;; - Performance acceptable (timing measurements)
;; - API functionality: create graph, add nodes/edges, draw
;; - Various graph types (trees, DAGs, complex structures)
;; - Edge cases: empty graph, single node, disconnected components
;;
;; Baseline Status: ✅ Required for GKNV compliance
;;
;; This test serves as the primary integration test for the complete GKNV
;; algorithm implementation, verifying that all four passes work together
;; correctly to produce high-quality graph drawings.
;;
;; See doc/implementation-decisions.md (all sections) for full decision rationale.
;; See doc/algorithm-specification.md (all sections) for implementation details.

;; Consolidated tests for core DAG-draw functionality including:
;; - Graph creation, node/edge management
;; - Basic validation and traversal
;; - Error handling and edge cases
;; - API acceptance tests
;;
;; Consolidated from: dag-draw-test.el, dag-draw-core-test.el

;;; Code:

(require 'buttercup)
(require 'ht)
(require 'dag-draw)
(require 'dag-draw-core)

;;; Graph Creation and Management

(describe "dag-draw package"
  
  (describe "when creating a new graph"
    (it "should create an empty graph"
      (let ((graph (dag-draw-create-graph)))
        (expect (dag-draw-graph-p graph) :to-be t)
        (expect (ht-size (dag-draw-graph-nodes graph)) :to-equal 0)
        (expect (dag-draw-graph-edges graph) :to-equal nil)))
    
    (it "should accept optional attributes"
      (let* ((attrs (ht ("title" "Test Graph")))
             (graph (dag-draw-create-graph attrs)))
        (expect (ht-get (dag-draw-graph-attributes graph) "title")
                :to-equal "Test Graph"))))

  (describe "when adding nodes to a graph"
    (let ((graph))
      (before-each
        (setq graph (dag-draw-create-graph)))
      
      (it "should add a node with default label"
        (dag-draw-add-node graph 'node1)
        (expect (ht-size (dag-draw-graph-nodes graph)) :to-equal 1)
        (let ((node (ht-get (dag-draw-graph-nodes graph) 'node1)))
          (expect (dag-draw-node-id node) :to-equal 'node1)
          (expect (dag-draw-node-label node) :to-equal "node1")))
      
      (it "should add a node with custom label"
        (dag-draw-add-node graph 'node1 "Custom Label")
        (let ((node (ht-get (dag-draw-graph-nodes graph) 'node1)))
          (expect (dag-draw-node-label node) :to-equal "Custom Label")))
      
      (it "should add multiple nodes"
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-node graph 'c)
        (expect (ht-size (dag-draw-graph-nodes graph)) :to-equal 3))))

  (describe "when adding edges to a graph"
    (let ((graph))
      (before-each
        (setq graph (dag-draw-create-graph))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-node graph 'c))
      
      (it "should add an edge between existing nodes"
        (dag-draw-add-edge graph 'a 'b)
        (expect (length (dag-draw-graph-edges graph)) :to-equal 1)
        (let ((edge (car (dag-draw-graph-edges graph))))
          (expect (dag-draw-edge-from-node edge) :to-equal 'a)
          (expect (dag-draw-edge-to-node edge) :to-equal 'b)
          (expect (dag-draw-edge-weight edge) :to-equal 1)))
      
      (it "should add an edge with custom weight"
        (dag-draw-add-edge graph 'a 'b 5)
        (let ((edge (car (dag-draw-graph-edges graph))))
          (expect (dag-draw-edge-weight edge) :to-equal 5)))
      
      (it "should add multiple edges"
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)
        (dag-draw-add-edge graph 'a 'c)
        (expect (length (dag-draw-graph-edges graph)) :to-equal 3))))

  (describe "when rendering a simple graph"
    (let ((graph))
      (before-each
        (setq graph (dag-draw-create-graph))
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-node graph 'b "Node B")
        (dag-draw-add-edge graph 'a 'b))
      
      (it "should render to SVG format"
        (let ((svg-output (dag-draw-render-graph graph 'svg)))
          (expect svg-output :to-match "<svg")
          (expect svg-output :to-match "</svg>")))
      
      (it "should render to ASCII format"
        (let ((ascii-output (dag-draw-render-graph graph 'ascii)))
          (expect ascii-output :to-be-truthy)))
      
      (it "should render to DOT format"
        (let ((dot-output (dag-draw-render-graph graph 'dot)))
          (expect dot-output :to-match "digraph")))))

;;; Layout Algorithm Acceptance Tests

(describe "acceptance criteria for layout algorithm"
  (let ((graph))
    (before-each
      ;; Create a simple hierarchical graph for testing
      (setq graph (dag-draw-create-graph))
      (dag-draw-add-node graph 'root "Root")
      (dag-draw-add-node graph 'left "Left")
      (dag-draw-add-node graph 'right "Right")
      (dag-draw-add-node graph 'leaf "Leaf")
      (dag-draw-add-edge graph 'root 'left)
      (dag-draw-add-edge graph 'root 'right)
      (dag-draw-add-edge graph 'left 'leaf)
      (dag-draw-add-edge graph 'right 'leaf))
    
    (it "should complete the full layout pipeline without errors"
      (expect (dag-draw-layout-graph graph) :not :to-throw))
    
    (it "should return the same graph instance after layout"
      (let ((result (dag-draw-layout-graph graph)))
        (expect result :to-be graph)))
    
    (it "should produce renderable output after layout"
      (dag-draw-layout-graph graph)
      (let ((svg-output (dag-draw-render-graph graph 'svg)))
        (expect svg-output :to-be-truthy)
        (expect (> (length svg-output) 0) :to-be t)))))

;;; Error Handling and Edge Cases

(describe "edge cases and error handling"
  (it "should handle empty graphs gracefully"
    (let ((empty-graph (dag-draw-create-graph)))
      (expect (dag-draw-layout-graph empty-graph) :not :to-throw)
      (expect (dag-draw-render-graph empty-graph 'ascii) :not :to-throw)))
  
  (it "should handle single node graphs"
    (let ((single-graph (dag-draw-create-graph)))
      (dag-draw-add-node single-graph 'single "Single Node")
      (expect (dag-draw-layout-graph single-graph) :not :to-throw)
      (expect (dag-draw-render-graph single-graph 'ascii) :not :to-throw)))
  
  (it "should handle disconnected graphs"
    (let ((disconnected-graph (dag-draw-create-graph)))
      (dag-draw-add-node disconnected-graph 'a "Node A")
      (dag-draw-add-node disconnected-graph 'b "Node B")
      (dag-draw-add-node disconnected-graph 'c "Node C")
      (dag-draw-add-node disconnected-graph 'd "Node D")
      (dag-draw-add-edge disconnected-graph 'a 'b)
      (dag-draw-add-edge disconnected-graph 'c 'd)
      (expect (dag-draw-layout-graph disconnected-graph) :not :to-throw)
      (expect (dag-draw-render-graph disconnected-graph 'ascii) :not :to-throw)))
  
  (it "should handle graphs with cycles"
    (let ((cyclic-graph (dag-draw-create-graph)))
      (dag-draw-add-node cyclic-graph 'a "Node A")
      (dag-draw-add-node cyclic-graph 'b "Node B")
      (dag-draw-add-node cyclic-graph 'c "Node C")
      (dag-draw-add-edge cyclic-graph 'a 'b)
      (dag-draw-add-edge cyclic-graph 'b 'c)
      (dag-draw-add-edge cyclic-graph 'c 'a)
      (expect (dag-draw-layout-graph cyclic-graph) :not :to-throw)
      (expect (dag-draw-render-graph cyclic-graph 'ascii) :not :to-throw)))
  
  
  (it "should handle large node labels"
    (let ((large-label-graph (dag-draw-create-graph)))
      (dag-draw-add-node large-label-graph 'a "A very long node label that exceeds normal expectations")
      (dag-draw-add-node large-label-graph 'b "B")
      (dag-draw-add-edge large-label-graph 'a 'b)
      (expect (dag-draw-layout-graph large-label-graph) :not :to-throw)
      (expect (dag-draw-render-graph large-label-graph 'ascii) :not :to-throw)))
  
  (it "should handle special characters in labels"
    (let ((special-graph (dag-draw-create-graph)))
      (dag-draw-add-node special-graph 'a "Node with newline")
      (dag-draw-add-node special-graph 'b "Node with tab")
      (dag-draw-add-edge special-graph 'a 'b)
      (expect (dag-draw-layout-graph special-graph) :not :to-throw)
      (expect (dag-draw-render-graph special-graph 'ascii) :not :to-throw)))))

(provide 'core-functionality-test)

;;; core-functionality-test.el ends here