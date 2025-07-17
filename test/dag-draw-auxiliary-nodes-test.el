;;; dag-draw-auxiliary-nodes-test.el --- TDD for GKNV auxiliary node implementation -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; TDD tests to implement proper GKNV auxiliary node creation and management
;; during network simplex optimization.
;; 
;; GKNV Figure 2-2 specification (lines 416-420):
;; "for all nodes v with no in-edge, we make a temporary edge (S_min,v) with δ = 0, 
;;  and for all nodes v with no out-edge, we make a temporary edge (v,S_max) with δ = 0.
;;  Thus, λ(S_min) ≤ λ(v) ≤ λ(S_max) for all v."

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-pass1-ranking)

(describe "GKNV Auxiliary Node Implementation"
  
  (it "should create S_min and S_max auxiliary nodes during feasible tree construction"
    ;; RED: Test that auxiliary nodes are created per GKNV Figure 2-2
    (let ((graph (dag-draw-create-graph)))
      ;; Create simple graph with source and sink nodes
      (dag-draw-add-node graph 'a "A")
      (dag-draw-add-node graph 'b "B") 
      (dag-draw-add-node graph 'c "C")
      (dag-draw-add-edge graph 'a 'b)
      (dag-draw-add-edge graph 'b 'c)
      
      ;; Create feasible tree with auxiliary nodes (to be implemented)
      (let ((tree-result (dag-draw--construct-feasible-tree graph)))
        
        ;; Verify auxiliary nodes were created
        (let ((aux-nodes (dag-draw--find-auxiliary-nodes graph)))
          (expect (length aux-nodes) :to-equal 2)
          (expect (member 'dag-draw-s-min aux-nodes) :to-be-truthy)
          (expect (member 'dag-draw-s-max aux-nodes) :to-be-truthy))
        
        ;; Verify auxiliary edges were created per GKNV spec
        (let ((aux-edges (dag-draw--find-auxiliary-edges graph)))
          ;; Should have edge from S_min to source node 'a
          ;; Should have edge from sink node 'c to S_max
          (expect (length aux-edges) :to-equal 2)
          (expect (dag-draw--edge-exists-p graph 'dag-draw-s-min 'a) :to-be t)
          (expect (dag-draw--edge-exists-p graph 'c 'dag-draw-s-max) :to-be t)))))
  
  (it "should create auxiliary edges to all source nodes (nodes with no in-edges)"
    ;; RED: Test auxiliary edges creation per GKNV lines 416-417
    (let ((graph (dag-draw-create-graph)))
      ;; Create graph with multiple source nodes
      (dag-draw-add-node graph 'source1 "S1")
      (dag-draw-add-node graph 'source2 "S2")
      (dag-draw-add-node graph 'middle "M")
      (dag-draw-add-edge graph 'source1 'middle)
      (dag-draw-add-edge graph 'source2 'middle)
      
      ;; Construct feasible tree
      (dag-draw--construct-feasible-tree graph)
      
      ;; Verify S_min connects to all source nodes
      (expect (dag-draw--edge-exists-p graph 'dag-draw-s-min 'source1) :to-be t)
      (expect (dag-draw--edge-exists-p graph 'dag-draw-s-min 'source2) :to-be t)
      (expect (dag-draw--edge-exists-p graph 'dag-draw-s-min 'middle) :to-be nil) ; Not a source
      
      ;; Verify auxiliary edges have δ = 0 (minimum length)
      (let ((aux-edge1 (dag-draw--get-edge graph 'dag-draw-s-min 'source1))
            (aux-edge2 (dag-draw--get-edge graph 'dag-draw-s-min 'source2)))
        (expect (dag-draw-edge-min-length aux-edge1) :to-equal 0)
        (expect (dag-draw-edge-min-length aux-edge2) :to-equal 0))))
  
  (it "should create auxiliary edges from all sink nodes (nodes with no out-edges)"
    ;; RED: Test auxiliary edges creation per GKNV lines 417-418
    (let ((graph (dag-draw-create-graph)))
      ;; Create graph with multiple sink nodes
      (dag-draw-add-node graph 'middle "M")
      (dag-draw-add-node graph 'sink1 "T1")
      (dag-draw-add-node graph 'sink2 "T2")
      (dag-draw-add-edge graph 'middle 'sink1)
      (dag-draw-add-edge graph 'middle 'sink2)
      
      ;; Construct feasible tree
      (dag-draw--construct-feasible-tree graph)
      
      ;; Verify all sink nodes connect to S_max
      (expect (dag-draw--edge-exists-p graph 'sink1 'dag-draw-s-max) :to-be t)
      (expect (dag-draw--edge-exists-p graph 'sink2 'dag-draw-s-max) :to-be t)
      (expect (dag-draw--edge-exists-p graph 'middle 'dag-draw-s-max) :to-be nil) ; Not a sink
      
      ;; Verify auxiliary edges have δ = 0
      (let ((aux-edge1 (dag-draw--get-edge graph 'sink1 'dag-draw-s-max))
            (aux-edge2 (dag-draw--get-edge graph 'sink2 'dag-draw-s-max)))
        (expect (dag-draw-edge-min-length aux-edge1) :to-equal 0)
        (expect (dag-draw-edge-min-length aux-edge2) :to-equal 0))))
  
  (it "should maintain constraint λ(S_min) ≤ λ(v) ≤ λ(S_max) for all nodes"
    ;; RED: Test GKNV constraint from lines 418-420
    (let ((graph (dag-draw-create-graph)))
      ;; Create test graph
      (dag-draw-add-node graph 'a "A")
      (dag-draw-add-node graph 'b "B")
      (dag-draw-add-node graph 'c "C")
      (dag-draw-add-node graph 'd "D")
      (dag-draw-add-edge graph 'a 'b 2)
      (dag-draw-add-edge graph 'b 'c 1)
      (dag-draw-add-edge graph 'c 'd 3)
      
      ;; Create auxiliary nodes and assign ranks manually to test constraint
      (let ((tree-info (dag-draw--construct-feasible-tree graph)))
        (dag-draw--assign-basic-ranks-with-auxiliary graph tree-info)
        
        ;; Get ranks before cleanup
        (let ((s-min-rank (dag-draw-node-rank (dag-draw-get-node graph 'dag-draw-s-min)))
              (s-max-rank (dag-draw-node-rank (dag-draw-get-node graph 'dag-draw-s-max)))
              (a-rank (dag-draw-node-rank (dag-draw-get-node graph 'a)))
              (b-rank (dag-draw-node-rank (dag-draw-get-node graph 'b)))
              (c-rank (dag-draw-node-rank (dag-draw-get-node graph 'c)))
              (d-rank (dag-draw-node-rank (dag-draw-get-node graph 'd))))
          
          ;; Verify constraint: λ(S_min) ≤ λ(v) ≤ λ(S_max) for all v
          (expect s-min-rank :to-be-less-than (+ a-rank 1))
          (expect s-min-rank :to-be-less-than (+ b-rank 1))
          (expect s-min-rank :to-be-less-than (+ c-rank 1))
          (expect s-min-rank :to-be-less-than (+ d-rank 1))
          
          (expect a-rank :to-be-less-than (+ s-max-rank 1))
          (expect b-rank :to-be-less-than (+ s-max-rank 1))
          (expect c-rank :to-be-less-than (+ s-max-rank 1))
          (expect d-rank :to-be-less-than (+ s-max-rank 1))))))
  
  (it "should properly integrate auxiliary nodes with network simplex spanning tree"
    ;; RED: Test that auxiliary nodes are created during network simplex process
    (let ((graph (dag-draw-create-graph)))
      ;; Create graph requiring optimization
      (dag-draw-add-node graph 'x "X")
      (dag-draw-add-node graph 'y "Y")
      (dag-draw-add-node graph 'z "Z")
      (dag-draw-add-edge graph 'x 'y 2)
      (dag-draw-add-edge graph 'y 'z 1)
      
      ;; Spy on auxiliary cleanup to verify auxiliary nodes were created
      (let ((aux-nodes-found nil))
        (spy-on 'dag-draw--cleanup-auxiliary-elements 
                :and-call-fake (lambda (g)
                                 (let ((aux-nodes (dag-draw--find-auxiliary-nodes g)))
                                   (when aux-nodes
                                     (setq aux-nodes-found aux-nodes)))
                                 ;; Don't call original to avoid recursion - just do the cleanup manually
                                 (let ((aux-nodes (dag-draw--find-auxiliary-nodes g))
                                       (aux-edges (dag-draw--find-auxiliary-edges g)))
                                   (dolist (edge aux-edges)
                                     (dag-draw-remove-edge g 
                                                           (dag-draw-edge-from-node edge)
                                                           (dag-draw-edge-to-node edge)))
                                   (dolist (node-id aux-nodes)
                                     (when (dag-draw-get-node g node-id)
                                       (dag-draw-remove-node g node-id))))))
        
        ;; Run network simplex
        (dag-draw--assign-ranks-network-simplex graph)
        
        ;; Verify auxiliary nodes were created and then cleaned up
        (expect (member 'dag-draw-s-min aux-nodes-found) :to-be-truthy)
        (expect (member 'dag-draw-s-max aux-nodes-found) :to-be-truthy))))
  
  (it "should clean up auxiliary nodes after network simplex completion"
    ;; RED: Test complete auxiliary cleanup per existing cleanup function
    (let ((graph (dag-draw-create-graph)))
      ;; Create test graph
      (dag-draw-add-node graph 'p "P")
      (dag-draw-add-node graph 'q "Q")
      (dag-draw-add-edge graph 'p 'q)
      
      ;; Store original node count
      (let ((original-count (length (dag-draw-get-node-ids graph))))
        
        ;; Run complete network simplex process
        (dag-draw--assign-ranks-network-simplex graph)
        
        ;; Verify auxiliary nodes were removed after optimization
        (expect (length (dag-draw-get-node-ids graph)) :to-equal original-count)
        (expect (dag-draw-get-node graph 'dag-draw-s-min) :to-be nil)
        (expect (dag-draw-get-node graph 'dag-draw-s-max) :to-be nil)
        
        ;; Verify auxiliary edges were removed
        (expect (dag-draw--find-auxiliary-edges graph) :to-equal '())))))

(provide 'dag-draw-auxiliary-nodes-test)

;;; dag-draw-auxiliary-nodes-test.el ends here