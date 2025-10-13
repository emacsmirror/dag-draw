;;; dag-draw-network-simplex-core-test.el --- TDD for GKNV network simplex core algorithm -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; TDD tests to implement the core network simplex optimization algorithm
;; from GKNV Figure 2-1 steps 3-6.
;;
;; GKNV Figure 2-1 Network Simplex Algorithm:
;; 3. while (e = leave_edge()) ≠ nil do
;; 4. f = enter_edge(e);
;; 5. exchange(e,f);
;; 6. end
;;
;; This implements the iterative optimization that makes GKNV superior
;; to simple topological ordering by considering edge weights and costs.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-pass1-ranking)

(describe "GKNV Network Simplex Core Algorithm"

  (it "should detect when spanning tree is already optimal (no negative cut values)"
    ;; RED: Test termination condition from Figure 2-1 step 3
    (let ((graph (dag-draw-create-graph)))
      ;; Create simple graph where initial tree should be optimal
      (dag-draw-add-node graph 'a "A")
      (dag-draw-add-node graph 'b "B")
      (dag-draw-add-edge graph 'a 'b 1) ; Unit weight - should be optimal

      ;; Create feasible tree with auxiliary nodes
      (let ((tree-info (dag-draw--construct-feasible-tree graph)))
        ;; Test leave_edge() returns nil for optimal tree
        (expect (dag-draw--leave-edge tree-info graph) :to-be nil))))

  (it "should find leaving edge with negative cut value when tree is not optimal"
    ;; RED: Test leave_edge() finds optimization opportunity using proven GKNV pattern
    (let ((graph (dag-draw-create-graph)))
      ;; Use the pattern we proved works in comprehensive tests
      (dag-draw-add-node graph 'g "g")  
      (dag-draw-add-node graph 'h "h")  
      (dag-draw-add-node graph 'a "a")  
      (dag-draw-add-node graph 'e "e")  
      (dag-draw-add-node graph 'f "f")

      ;; Create edges that will produce negative cut value per GKNV Section 2.3
      (dag-draw-add-edge graph 'g 'h 1)   ; Tree edge: tail->head (+1)
      (dag-draw-add-edge graph 'a 'e 1)   ; Non-tree edge: head->tail (-1)  
      (dag-draw-add-edge graph 'a 'f 1)   ; Non-tree edge: head->tail (-1)
      (dag-draw-add-edge graph 'h 'a 1)   ; Tree edge: connects h to head component
      (dag-draw-add-edge graph 'g 'e 1)   ; Tree edge: connects g to tail component
      (dag-draw-add-edge graph 'g 'f 1)   ; Tree edge: connects g to tail component

      ;; Create tree structure where removing (g,h) creates negative cut value
      (let ((tree-info (ht-create)))
        (ht-set! tree-info 'tree-edges 
                 (list (dag-draw-find-edge graph 'g 'h)    ; Cut value = 1 - 1 - 1 = -1
                       (dag-draw-find-edge graph 'h 'a)    ; Connects h to head component  
                       (dag-draw-find-edge graph 'g 'e)    ; Connects g to tail component
                       (dag-draw-find-edge graph 'g 'f)))  ; Connects g to tail component
        (ht-set! tree-info 'non-tree-edges
                 (list (dag-draw-find-edge graph 'a 'e)    ; Head->tail crossing
                       (dag-draw-find-edge graph 'a 'f)))  ; Head->tail crossing

        ;; Should find the g->h edge with negative cut value
        (let ((leaving-edge (dag-draw--leave-edge tree-info graph)))
          (expect leaving-edge :not :to-be nil)
          ;; Should be one of the tree edges
          (expect (member leaving-edge (ht-get tree-info 'tree-edges)) :to-be-truthy))))))

(provide 'dag-draw-network-simplex-core-test)

;;; dag-draw-network-simplex-core-test.el ends here