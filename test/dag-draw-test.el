;;; dag-draw-test.el --- Tests for dag-draw.el -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Acceptance tests and integration tests for dag-draw.el following
;; the ATDD (Acceptance Test Driven Development) approach.

;;; Code:

(require 'buttercup)
(require 'dag-draw)

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

  (describe "edge cases and error handling"
    (it "should handle empty graphs gracefully"
      (let ((empty-graph (dag-draw-create-graph)))
        (expect (dag-draw-layout-graph empty-graph) :not :to-throw)
        (expect (dag-draw-render-graph empty-graph) :not :to-throw)))
    
    (it "should handle single node graphs"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'single)
        (expect (dag-draw-layout-graph graph) :not :to-throw)
        (expect (dag-draw-render-graph graph) :not :to-throw)))
    
    (it "should reject invalid output formats"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'test)
        (expect (dag-draw-render-graph graph 'invalid-format)
                :to-throw 'error)))))

;;; dag-draw-test.el ends here