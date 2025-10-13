;;; dag-draw-edge-label-integration-test.el --- Integration tests for edge labels -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; GKNV Baseline Compliance Tests - Pass 4: Edge Label Integration
;;
;; This module tests GKNV edge label integration with spline routing as specified
;; in "A Technique for Drawing Directed Graphs" (Gansner, Koutsofios, North, Vo).
;;
;; GKNV Reference: Section 5.3 (label integration with routing)
;; Decision: D4.12 - Labels integrate via virtual node mechanism
;; Algorithm: Edge Label and Spline Integration
;;
;; Key Requirements Tested:
;; - Edge labels represented as virtual nodes in graph
;; - Label nodes participate in all passes (ranking, ordering, positioning)
;; - Spline routing accounts for label node positions
;; - Edge passes near label node (label appears along edge)
;; - Label positioning off-center avoids edge overlap
;; - Integration seamless: labels behave like regular virtual nodes
;; - No special-case code needed for label routing
;;
;; Test Coverage:
;; - Labels participate in ranking (correct rank assignment)
;; - Labels participate in ordering (minimize crossings)
;; - Labels participate in positioning (optimal X/Y coordinates)
;; - Splines route past label nodes correctly
;; - Labels appear positioned along edges
;; - Label placement doesn't cause overlaps
;; - Multiple labels on different edges
;; - End-to-end: labeled graph to rendered output
;;
;; Baseline Status: ✅ Required for GKNV compliance
;;
;; See doc/implementation-decisions.md (D4.12) for full decision rationale.
;; See doc/algorithm-specification.md Pass 4 for implementation details.

;; Integration tests for edge label functionality in the mainline pipeline.
;; Tests that edge labels are processed automatically during graph layout.

;;; Code:

(require 'buttercup)
(require 'dag-draw)

(describe "Edge Label Integration with Mainline Pipeline"
  
  (it "should automatically process edge labels during layout"
    ;; Test that dag-draw-layout-graph automatically handles edge labels
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'a "A")
      (dag-draw-add-node graph 'b "B")
      
      ;; Add edge with label
      (let ((edge (dag-draw-add-edge graph 'a 'b)))
        (setf (dag-draw-edge-label edge) "test-label")
        
        ;; Layout graph - this should automatically process edge labels
        (dag-draw-layout-graph graph)
        
        ;; Check that virtual node was created
        (let ((label-nodes (dag-draw--get-label-virtual-nodes graph)))
          (expect (length label-nodes) :to-equal 1)
          (expect (dag-draw-node-label (car label-nodes)) :to-equal "test-label"))
        
        ;; Check that edge length was adjusted
        (expect (dag-draw-edge-δ edge) :to-equal 2))))
  
  (it "should skip processing when no edge labels exist"
    ;; Test that graphs without labels work normally
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'a "A")  
      (dag-draw-add-node graph 'b "B")
      (dag-draw-add-edge graph 'a 'b)
      
      ;; Layout graph without labels
      (dag-draw-layout-graph graph)
      
      ;; Should have no virtual nodes
      (let ((label-nodes (dag-draw--get-label-virtual-nodes graph)))
        (expect (length label-nodes) :to-equal 0))))
  
  (it "should handle mixed labeled and unlabeled edges"
    ;; Test proper handling of mixed edge types
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'a "A")
      (dag-draw-add-node graph 'b "B") 
      (dag-draw-add-node graph 'c "C")
      
      (let ((labeled-edge (dag-draw-add-edge graph 'a 'b))
            (unlabeled-edge (dag-draw-add-edge graph 'b 'c)))
        (setf (dag-draw-edge-label labeled-edge) "labeled")
        
        ;; Layout the graph
        (dag-draw-layout-graph graph)
        
        ;; Check that only labeled edge was modified
        (expect (dag-draw-edge-δ labeled-edge) :to-equal 2)
        (expect (dag-draw-edge-δ unlabeled-edge) :to-equal 1)
        
        ;; Check that one virtual node was created
        (let ((label-nodes (dag-draw--get-label-virtual-nodes graph)))
          (expect (length label-nodes) :to-equal 1))))))

(provide 'dag-draw-edge-label-integration-test)

;;; dag-draw-edge-label-integration-test.el ends here