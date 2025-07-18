;;; dag-draw-constraint-solver-test.el --- TDD tests for GKNV network simplex constraint solver -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; TDD tests for implementing GKNV Section 4.2 network simplex constraint solver.
;; Based on GKNV paper specification: "The method involves constructing an auxiliary 
;; graph... Every edge e=(u,v) in G is replaced by two edges (n_e,u) and (n_e,v) 
;; with δ=0 and ω=ω(e)×Ω(e). If v is the left neighbor of w, then G' has an edge 
;; f=(v,w) with δ(f)=ρ(v,w) and ω(f)=0."
;;
;; Reference: "A Technique for Drawing Directed Graphs" by Gansner, Koutsofios, North, Vo
;; Section 4.2: "Optimal Node Placement" with auxiliary graph construction

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-pass3-positioning)

;;; Auxiliary Graph Construction Tests

(describe "GKNV Section 4.2 auxiliary graph construction"
  (it "should create auxiliary node for each original edge"
    (let ((graph (dag-draw-create-graph)))
      ;; Create simple graph: A -> B -> C
      (dag-draw-add-node graph 'a "Node A")
      (dag-draw-add-node graph 'b "Node B") 
      (dag-draw-add-node graph 'c "Node C")
      (dag-draw-add-edge graph 'a 'b)
      (dag-draw-add-edge graph 'b 'c)
      
      ;; Build auxiliary graph for constraint solving
      (let ((aux-graph (dag-draw--build-constraint-auxiliary-graph graph)))
        ;; Should have original nodes plus auxiliary nodes for each edge
        (expect (dag-draw-node-count aux-graph) :to-equal 5)  ; 3 original + 2 auxiliary
        ;; Should have auxiliary nodes named after edges
        (expect (dag-draw-get-node aux-graph 'aux_edge_a_b) :not :to-be nil)
        (expect (dag-draw-get-node aux-graph 'aux_edge_b_c) :not :to-be nil))))

  (it "should create cost edges for each original edge per GKNV specification"
    (let ((graph (dag-draw-create-graph)))
      ;; Create graph with weighted edge
      (dag-draw-add-node graph 'u "Node U")
      (dag-draw-add-node graph 'v "Node V")
      (dag-draw-add-edge graph 'u 'v 5)  ; Weight = 5
      
      ;; Build auxiliary graph
      (let ((aux-graph (dag-draw--build-constraint-auxiliary-graph graph)))
        ;; Should have edges: (aux_edge_u_v, u) and (aux_edge_u_v, v)
        ;; Both with δ=0 and ω=weight×Ω_factor
        (let ((cost-edges (dag-draw--find-cost-edges aux-graph 'aux_edge_u_v)))
          (expect (length cost-edges) :to-equal 2)
          ;; Verify GKNV specification: δ=0, ω=ω(e)×Ω(e)
          (dolist (edge cost-edges)
            (expect (dag-draw-edge-min-length edge) :to-equal 0)  ; δ=0
            (expect (dag-draw-edge-weight edge) :to-be-greater-than 0)))))) ; ω>0

  (it "should create separation edges for adjacent nodes in same rank"
    (let ((graph (dag-draw-create-graph)))
      ;; Create nodes in same rank
      (dag-draw-add-node graph 'left "Left")
      (dag-draw-add-node graph 'right "Right")
      
      ;; Set same rank and positions to establish adjacency
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'left)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'right)) 0)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'left)) 100)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'right)) 200)
      
      ;; Build auxiliary graph
      (let ((aux-graph (dag-draw--build-constraint-auxiliary-graph graph)))
        ;; Should have separation edge: (left, right) with δ=ρ(left,right), ω=0
        (let ((sep-edge (dag-draw--find-separation-edge aux-graph 'left 'right)))
          (expect sep-edge :not :to-be nil)
          (expect (dag-draw-edge-weight sep-edge) :to-equal 0)  ; ω=0 per GKNV
          (expect (dag-draw-edge-min-length sep-edge) :to-be-greater-than 0))))) ; δ=ρ(v,w)>0

  (it "should handle multi-rank graphs with complex separation constraints"
    (let ((graph (dag-draw-create-graph)))
      ;; Create multi-rank graph
      (dag-draw-add-node graph 'top1 "Top 1")
      (dag-draw-add-node graph 'top2 "Top 2")
      (dag-draw-add-node graph 'bottom1 "Bottom 1") 
      (dag-draw-add-node graph 'bottom2 "Bottom 2")
      
      ;; Set up ranks
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'top1)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'top2)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'bottom1)) 1)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'bottom2)) 1)
      
      ;; Build auxiliary graph
      (let ((aux-graph (dag-draw--build-constraint-auxiliary-graph graph)))
        ;; Should have separation edges within each rank
        (let ((rank0-sep-edges (dag-draw--find-rank-separation-edges aux-graph 0))
              (rank1-sep-edges (dag-draw--find-rank-separation-edges aux-graph 1)))
          (expect (length rank0-sep-edges) :to-be-greater-than 0)
          (expect (length rank1-sep-edges) :to-be-greater-than 0))))))

;;; Network Simplex Constraint Solver Tests  

(describe "GKNV Section 4.2 network simplex constraint solver"
  (it "should solve simple constraint system with separation requirements"
    (let ((graph (dag-draw-create-graph)))
      ;; Create constrained positioning problem
      (dag-draw-add-node graph 'a "A")
      (dag-draw-add-node graph 'b "B")
      (dag-draw-add-edge graph 'a 'b)
      
      ;; Set up ranks for positioning
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'a)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'b)) 1)
      
      ;; Apply constraint solver
      (dag-draw--solve-positioning-constraints graph)
      
      ;; Verify solution respects constraints
      (let ((a-x (dag-draw-node-x-coord (dag-draw-get-node graph 'a)))
            (b-x (dag-draw-node-x-coord (dag-draw-get-node graph 'b))))
        (expect a-x :not :to-be nil)
        (expect b-x :not :to-be nil)
        ;; Nodes should have valid positions
        (expect a-x :to-be-greater-than -1)
        (expect b-x :to-be-greater-than -1))))

  (it "should minimize total edge cost while respecting separation constraints"
    (let ((graph (dag-draw-create-graph)))
      ;; Create graph with different edge weights
      (dag-draw-add-node graph 'center "Center")
      (dag-draw-add-node graph 'left "Left")
      (dag-draw-add-node graph 'right "Right")
      (dag-draw-add-edge graph 'center 'left 1)   ; Low cost edge
      (dag-draw-add-edge graph 'center 'right 10) ; High cost edge
      
      ;; Set up same rank to create positioning choice
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'center)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'left)) 1)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'right)) 1)
      
      ;; Apply constraint solver
      (dag-draw--solve-positioning-constraints graph)
      
      ;; Verify cost minimization: left should be closer to center than right
      (let ((center-x (dag-draw-node-x-coord (dag-draw-get-node graph 'center)))
            (left-x (dag-draw-node-x-coord (dag-draw-get-node graph 'left)))
            (right-x (dag-draw-node-x-coord (dag-draw-get-node graph 'right))))
        (let ((dist-to-left (abs (- center-x left-x)))
              (dist-to-right (abs (- center-x right-x))))
          ;; Lower cost edge should result in shorter distance
          (expect dist-to-left :to-be-less-than dist-to-right)))))

  (it "should handle complex constraint networks with multiple ranks"
    (let ((graph (dag-draw-create-graph)))
      ;; Create complex multi-rank constraint network
      (dag-draw-add-node graph 'source "Source")
      (dag-draw-add-node graph 'mid1 "Mid1")
      (dag-draw-add-node graph 'mid2 "Mid2") 
      (dag-draw-add-node graph 'target "Target")
      
      ;; Create constraint network with crossing dependencies
      (dag-draw-add-edge graph 'source 'mid1 2)
      (dag-draw-add-edge graph 'source 'mid2 3)
      (dag-draw-add-edge graph 'mid1 'target 1)
      (dag-draw-add-edge graph 'mid2 'target 4)
      
      ;; Set up ranks
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'source)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'mid1)) 1)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'mid2)) 1)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'target)) 2)
      
      ;; Apply constraint solver  
      (dag-draw--solve-positioning-constraints graph)
      
      ;; Verify all nodes have valid positions
      (let ((positions (mapcar (lambda (id) 
                                 (dag-draw-node-x-coord (dag-draw-get-node graph id)))
                               '(source mid1 mid2 target))))
        (expect (cl-every (lambda (x) (and x (>= x 0))) positions) :to-be-truthy))))

  (it "should produce better layouts than simple heuristic positioning"
    (let ((graph (dag-draw-create-graph)))
      ;; Create graph where optimal positioning differs from heuristic
      (dag-draw-add-node graph 'a "A")
      (dag-draw-add-node graph 'b "B")
      (dag-draw-add-node graph 'c "C")
      (dag-draw-add-edge graph 'a 'b 1)  ; Short preferred path
      (dag-draw-add-edge graph 'a 'c 10) ; Avoid long expensive edge
      
      ;; Set up positioning problem
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'a)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'b)) 1)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'c)) 1)
      
      ;; Apply simple heuristic positioning first  
      (dag-draw--apply-simple-heuristic-positioning graph)
      (let ((heuristic-cost (dag-draw--calculate-layout-cost graph)))
        ;; Apply constraint solver
        (dag-draw--solve-positioning-constraints graph)
        (let ((optimal-cost (dag-draw--calculate-layout-cost graph)))
          ;; Constraint solver should produce equal or better cost
          (expect optimal-cost :to-be-less-than (+ heuristic-cost 1)))))))

;;; Constraint Validation and Edge Cases

(describe "GKNV constraint solver validation and edge cases"
  (it "should handle empty graph gracefully"
    (let ((graph (dag-draw-create-graph)))
      ;; Should not error on empty graph
      (expect (dag-draw--solve-positioning-constraints graph) :not :to-throw)))

  (it "should handle single node graph"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'single "Single")
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'single)) 0)
      
      ;; Should position single node at valid coordinate
      (dag-draw--solve-positioning-constraints graph)
      (let ((x (dag-draw-node-x-coord (dag-draw-get-node graph 'single))))
        (expect x :not :to-be nil)
        (expect x :to-be-greater-than -1))))

  (it "should maintain GKNV separation constraints in solution"
    (let ((graph (dag-draw-create-graph)))
      ;; Create nodes requiring specific separation
      (dag-draw-add-node graph 'left "Left Node")
      (dag-draw-add-node graph 'right "Right Node")
      
      ;; Set up same rank positioning
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'left)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'right)) 0)
      
      ;; Apply constraint solver
      (dag-draw--solve-positioning-constraints graph)
      
      ;; Verify separation constraint: x_right - x_left >= ρ(left,right)
      (let* ((left-x (dag-draw-node-x-coord (dag-draw-get-node graph 'left)))
             (right-x (dag-draw-node-x-coord (dag-draw-get-node graph 'right)))
             (actual-separation (abs (- right-x left-x)))
             (min-separation (dag-draw--calculate-separation graph 'left 'right)))
        (expect actual-separation :to-be-greater-than (- min-separation 5))))) ; Small tolerance

  (it "should integrate with existing GKNV positioning workflow"
    (let ((graph (dag-draw-create-graph)))
      ;; Create graph that goes through full positioning  
      (dag-draw-add-node graph 'source "Source")
      (dag-draw-add-node graph 'target "Target")
      (dag-draw-add-edge graph 'source 'target)
      
      ;; Run positioning with constraint solver enabled
      (dag-draw-rank-graph graph)
      (dag-draw-order-vertices graph)
      (dag-draw--solve-positioning-constraints graph)
      
      ;; Should have valid final layout
      (let ((source-x (dag-draw-node-x-coord (dag-draw-get-node graph 'source)))
            (target-x (dag-draw-node-x-coord (dag-draw-get-node graph 'target))))
        (expect source-x :not :to-be nil)
        (expect target-x :not :to-be nil)))))

(provide 'dag-draw-constraint-solver-test)

;;; dag-draw-constraint-solver-test.el ends here