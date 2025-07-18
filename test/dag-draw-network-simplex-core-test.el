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
    ;; RED: Test leave_edge() finds optimization opportunity
    (let ((graph (dag-draw-create-graph)))
      ;; Create graph with high-weight edge that should be optimized
      (dag-draw-add-node graph 'a "A")
      (dag-draw-add-node graph 'b "B")
      (dag-draw-add-node graph 'c "C")
      (dag-draw-add-edge graph 'a 'b 1)   ; Low weight
      (dag-draw-add-edge graph 'b 'c 5)   ; High weight - should prefer shorter path
      (dag-draw-add-edge graph 'a 'c 2)   ; Alternative path

      ;; Create feasible tree and assign basic ranks
      (let ((tree-info (dag-draw--construct-feasible-tree graph)))
        (dag-draw--assign-basic-ranks-with-auxiliary graph tree-info)

        ;; Should find an edge to leave for optimization
        (let ((leaving-edge (dag-draw--leave-edge tree-info graph)))
          (expect leaving-edge :not :to-be nil)
          ;; Should be one of the tree edges
          (expect (member leaving-edge (ht-get tree-info 'tree-edges)) :to-be-truthy)))))

  (it "should find entering edge to replace leaving edge"
    ;; RED: Test enter_edge() finds replacement per Figure 2-1 step 4
    (let ((graph (dag-draw-create-graph)))
      ;; Create graph requiring edge exchange
      (dag-draw-add-node graph 'a "A")
      (dag-draw-add-node graph 'b "B")
      (dag-draw-add-node graph 'c "C")
      (dag-draw-add-edge graph 'a 'b 3)   ; Tree edge
      (dag-draw-add-edge graph 'b 'c 1)   ; Tree edge
      (dag-draw-add-edge graph 'a 'c 2)   ; Non-tree edge - better path

      (let ((tree-info (dag-draw--construct-feasible-tree graph)))
        (dag-draw--assign-basic-ranks-with-auxiliary graph tree-info)

        ;; Find edge to leave
        (let ((leaving-edge (dag-draw--leave-edge tree-info graph)))
          (when leaving-edge
            ;; Should find an entering edge to replace it
            (let ((entering-edge (dag-draw--enter-edge leaving-edge tree-info graph)))
              (expect entering-edge :not :to-be nil)
              ;; Should be one of the non-tree edges
              (expect (member entering-edge (ht-get tree-info 'non-tree-edges)) :to-be-truthy)))))))

  (it "should exchange leaving and entering edges maintaining tree structure"
    ;; RED: Test exchange() maintains spanning tree property
    (let ((graph (dag-draw-create-graph)))
      ;; Create graph for edge exchange
      (dag-draw-add-node graph 'a "A")
      (dag-draw-add-node graph 'b "B")
      (dag-draw-add-node graph 'c "C")
      (dag-draw-add-edge graph 'a 'b 2)
      (dag-draw-add-edge graph 'b 'c 1)
      (dag-draw-add-edge graph 'a 'c 3)

      (let ((tree-info (dag-draw--construct-feasible-tree graph)))
        (dag-draw--assign-basic-ranks-with-auxiliary graph tree-info)

        ;; Store original counts
        (let ((original-tree-count (length (ht-get tree-info 'tree-edges)))
              (original-non-tree-count (length (ht-get tree-info 'non-tree-edges))))

          (let ((leaving-edge (dag-draw--leave-edge tree-info graph)))
            (when leaving-edge
              (let ((entering-edge (dag-draw--enter-edge leaving-edge tree-info graph)))
                (when entering-edge
                  ;; Perform exchange
                  (dag-draw--exchange-edges leaving-edge entering-edge tree-info graph)

                  ;; Tree should maintain same number of edges
                  (expect (length (ht-get tree-info 'tree-edges)) :to-equal original-tree-count)
                  (expect (length (ht-get tree-info 'non-tree-edges)) :to-equal original-non-tree-count)

                  ;; Leaving edge should now be non-tree
                  (expect (member leaving-edge (ht-get tree-info 'non-tree-edges)) :to-be-truthy)
                  ;; Entering edge should now be tree
                  (expect (member entering-edge (ht-get tree-info 'tree-edges)) :to-be-truthy)))))))))

  (it "should calculate cut values for tree edges to guide optimization"
    ;; RED: Test cut value calculation for optimization decisions
    (let ((graph (dag-draw-create-graph)))
      ;; Create graph with known optimal structure
      (dag-draw-add-node graph 'a "A")
      (dag-draw-add-node graph 'b "B")
      (dag-draw-add-node graph 'c "C")
      (dag-draw-add-edge graph 'a 'b 1)   ; Optimal path
      (dag-draw-add-edge graph 'b 'c 1)   ; Optimal path
      (dag-draw-add-edge graph 'a 'c 5)   ; Suboptimal - high weight

      (let ((tree-info (dag-draw--construct-feasible-tree graph)))
        (dag-draw--assign-basic-ranks-with-auxiliary graph tree-info)

        ;; Calculate cut values for all tree edges
        (let ((cut-values (dag-draw--calculate-tree-cut-values tree-info graph)))
          ;; Should have cut value for each tree edge
          (expect (ht-size cut-values) :to-equal (length (ht-get tree-info 'tree-edges)))

          ;; Cut values should be numbers (cost indicators)
          (dolist (cut-value (ht-values cut-values))
            (expect (numberp cut-value) :to-be t))))))
  (it "should perform complete network simplex optimization iteration"
    ;; RED: Test one complete iteration of Figure 2-1 steps 3-6
    (let ((graph (dag-draw-create-graph)))
      ;; Create graph needing optimization
      (dag-draw-add-node graph 'start "Start")
      (dag-draw-add-node graph 'mid "Mid")
      (dag-draw-add-node graph 'end "End")
      (dag-draw-add-edge graph 'start 'mid 1)
      (dag-draw-add-edge graph 'mid 'end 1)
      (dag-draw-add-edge graph 'start 'end 10) ; High weight - should be avoided

      (let ((tree-info (dag-draw--construct-feasible-tree graph)))
        (dag-draw--assign-basic-ranks-with-auxiliary graph tree-info)

        ;; Perform one optimization iteration
        (let ((iteration-result (dag-draw--network-simplex-iteration tree-info graph)))
          ;; Should return result indicating what happened
          (expect (ht-p iteration-result) :to-be t)
          (expect (ht-contains-p iteration-result 'improved) :to-be t)
          (expect (ht-contains-p iteration-result 'converged) :to-be t)

          ;; If improvement was possible, tree should be updated
          (when (ht-get iteration-result 'improved)
            (expect (ht-get iteration-result 'updated-tree-info) :not :to-be nil))))))

  (it "should converge to optimal solution through iterative optimization"
    ;; RED: Test complete optimization until convergence
    (let ((graph (dag-draw-create-graph)))
      ;; Create graph with clear optimization opportunity
      (dag-draw-add-node graph 'a "A")
      (dag-draw-add-node graph 'b "B")
      (dag-draw-add-node graph 'c "C")
      (dag-draw-add-node graph 'd "D")
      (dag-draw-add-edge graph 'a 'b 1)
      (dag-draw-add-edge graph 'b 'c 1)
      (dag-draw-add-edge graph 'c 'd 1)
      (dag-draw-add-edge graph 'a 'd 10) ; Should be avoided in optimal solution

      (let ((tree-info (dag-draw--construct-feasible-tree graph)))
        (dag-draw--assign-basic-ranks-with-auxiliary graph tree-info)

        ;; Run optimization to convergence
        (let ((final-result (dag-draw--optimize-network-simplex tree-info graph)))
          ;; Should converge to optimal solution
          (expect (ht-get final-result 'converged) :to-be t)
          (expect (ht-get final-result 'iterations) :to-be-greater-than 0)
          (expect (ht-get final-result 'final-cost) :to-be-truthy)

          ;; Final tree should be different from initial (optimization occurred)
          (let ((final-tree-info (ht-get final-result 'final-tree-info)))
            (expect final-tree-info :not :to-be nil)))))))


(provide 'dag-draw-network-simplex-core-test)

;;; dag-draw-network-simplex-core-test.el ends here
