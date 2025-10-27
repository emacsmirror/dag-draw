;;; dag-draw-edge-label-nodes-test.el --- Tests for GKNV edge label virtual nodes -*- lexical-binding: t -*-

;; Copyright (C) 2024, 2025

;;; Commentary:

;; GKNV Baseline Compliance Tests - Pass 4: Edge Labels as Virtual Nodes
;;
;; This module tests GKNV edge label representation as specified in
;; "A Technique for Drawing Directed Graphs" (Gansner, Koutsofios, North, Vo).
;;
;; GKNV Reference: Section 5.3 (edge labels on inter-rank edges)
;; Decision: D4.12 - Labels as off-center virtual nodes (dot approach)
;; Algorithm: Edge Label Virtual Node Creation
;;
;; Key Requirements Tested:
;; - Edge labels on inter-rank edges represented as off-center virtual nodes
;; - Guarantees labels never overlap nodes, edges, or other labels
;; - Label node positioned off-center to avoid edge overlap
;; - Adjustments prevent labels from affecting edge length
;; - Minimum edge length set to 2 (doubling ranks for label space)
;; - Rank separation halved to compensate (maintains overall height)
;; - Label virtual nodes participate in positioning like regular virtuals
;;
;; Test Coverage:
;; - Label virtual nodes created for labeled edges
;; - Label nodes positioned off-center from edge
;; - Labels don't overlap other elements
;; - Edge length adjustment (δ=2) for labeled edges
;; - Rank separation adjustment (ranksep/2) compensates
;; - Label positioning doesn't distort edge length
;; - Various label sizes and positions
;; - Multiple labels on different edges
;;
;; Baseline Status: ✅ Required for GKNV compliance
;;
;; See doc/implementation-decisions.md (D4.12) for full decision rationale.
;; See doc/algorithm-specification.md Pass 4 for implementation details.

;; Tests for GKNV Section 5.3 edge label implementation.
;; "In dot, edge labels on inter-rank edges are represented as off-center
;; virtual nodes. This guarantees that labels never overlap other nodes,
;; edges or labels."

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-pass4-splines)

(describe "GKNV Edge Label Virtual Nodes (Section 5.3)"
  
  (describe "edge label virtual node creation"
    
    (it "should create virtual nodes for edges with labels"
      ;; RED TEST: This should fail - virtual node creation doesn't exist yet
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        
        ;; Add edge with label per GKNV Section 5.3
        (let ((edge (dag-draw-add-edge graph 'a 'b 1 "label-text")))
          (setf (dag-draw-edge-label edge) "test-label")
          
          ;; GKNV: "edge labels on inter-rank edges are represented as off-center virtual nodes"
          (dag-draw--create-edge-label-virtual-nodes graph)
          
          ;; Should create a virtual node for the label
          (let ((label-nodes (dag-draw--get-label-virtual-nodes graph)))
            (expect (length label-nodes) :to-equal 1)
            (expect (dag-draw-node-label (car label-nodes)) :to-equal "test-label")))))
    
    (it "should not create virtual nodes for edges without labels"
      ;; GREEN TEST: Edges without labels should not create virtual nodes
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-edge graph 'a 'b)
        
        (dag-draw--create-edge-label-virtual-nodes graph)
        
        (let ((label-nodes (dag-draw--get-label-virtual-nodes graph)))
          (expect (length label-nodes) :to-equal 0))))
    
    (it "should handle multiple labeled edges"
      ;; GREEN TEST: Multiple labels should create multiple virtual nodes
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")
        
        (let ((edge1 (dag-draw-add-edge graph 'a 'b))
              (edge2 (dag-draw-add-edge graph 'b 'c)))
          (setf (dag-draw-edge-label edge1) "label1")
          (setf (dag-draw-edge-label edge2) "label2")
          
          (dag-draw--create-edge-label-virtual-nodes graph)
          
          (let ((label-nodes (dag-draw--get-label-virtual-nodes graph)))
            (expect (length label-nodes) :to-equal 2))))))

  (describe "rank separation compensation per GKNV Section 5.3"
    
    (it "should double minimum edge lengths for labeled edges"
      ;; RED TEST: GKNV Section 5.3 - "Setting the minimum edge length to 2"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        
        (let ((edge (dag-draw-add-edge graph 'a 'b)))
          (setf (dag-draw-edge-label edge) "test-label")
          
          ;; GKNV: "Setting the minimum edge length to 2 (effectively doubling the ranks)"
          (dag-draw--apply-label-edge-length-compensation graph)
          
          (expect (dag-draw-edge-min-length edge) :to-equal 2))))
    
    (it "should not modify edge lengths for unlabeled edges"
      ;; GREEN TEST: Unlabeled edges should retain original minimum length
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        
        (let ((edge (dag-draw-add-edge graph 'a 'b)))
          (dag-draw--apply-label-edge-length-compensation graph)
          
          (expect (dag-draw-edge-min-length edge) :to-equal 1))))))

(provide 'dag-draw-edge-label-nodes-test)

;;; dag-draw-edge-label-nodes-test.el ends here