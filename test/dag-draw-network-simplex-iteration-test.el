;;; dag-draw-network-simplex-iteration-test.el --- TDD tests for network simplex iteration -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; TDD Phase 1.3: Network simplex iteration for optimizing spanning tree.
;; This implements the iterative optimization process from GKNV paper section 2.3.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-pass1-ranking)

(describe "Network Simplex Iteration Process"
  (describe "entering and leaving edge selection"
    (it "should find entering edge for optimization"
        ;; RED phase: This test will fail because entering edge selection doesn't exist yet
        (let ((graph (dag-draw-create-graph)))
          ;; Create graph with suboptimal spanning tree
          (dag-draw-add-node graph 'a "A")
          (dag-draw-add-node graph 'b "B")
          (dag-draw-add-node graph 'c "C")
          (dag-draw-add-edge graph 'a 'b 1)  ; In spanning tree
          (dag-draw-add-edge graph 'a 'c 2)  ; Not in spanning tree, higher weight
          (dag-draw-add-edge graph 'b 'c 1)  ; Not in spanning tree

          (let* ((spanning-tree (dag-draw--create-feasible-spanning-tree graph))
                 (cut-values (dag-draw--calculate-cut-values graph spanning-tree))
                 (negative-edges (dag-draw--find-negative-cut-value-edges cut-values)))

            ;; Should find an entering edge when negative cut values exist
            (expect negative-edges :to-be-truthy) ; Ensure we have negative edges
            (let ((leaving-edge (caar negative-edges))) ; Get edge from first (edge . cut-value) pair
              (let ((entering-edge (dag-draw--find-entering-edge graph spanning-tree leaving-edge)))

                ;; Entering edge should be found
                (expect entering-edge :to-be-truthy)

                ;; Entering edge should not be in current spanning tree
                (expect (member entering-edge (dag-draw-spanning-tree-edges spanning-tree)) :to-be nil))))))

    (it "should perform edge exchange in spanning tree"
        ;; RED phase: This test will fail because edge exchange doesn't exist yet
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'x "X")
          (dag-draw-add-node graph 'y "Y")
          (dag-draw-add-node graph 'z "Z")
          (dag-draw-add-edge graph 'x 'y 1)
          (dag-draw-add-edge graph 'x 'z 2)
          (dag-draw-add-edge graph 'y 'z 1)

          (let* ((spanning-tree (dag-draw--create-feasible-spanning-tree graph))
                 (original-edges (length (dag-draw-spanning-tree-edges spanning-tree)))
                 (cut-values (dag-draw--calculate-cut-values graph spanning-tree))
                 (negative-edges (dag-draw--find-negative-cut-value-edges cut-values)))

            (when negative-edges
              (let* ((leaving-edge (car (mapcar 'car negative-edges)))
                     (entering-edge (dag-draw--find-entering-edge graph spanning-tree leaving-edge))
                     (new-tree (dag-draw--exchange-spanning-tree-edges spanning-tree leaving-edge entering-edge)))

                ;; Tree should still have same number of edges
                (expect (length (dag-draw-spanning-tree-edges new-tree)) :to-equal original-edges)

                ;; Leaving edge should be removed
                (expect (member leaving-edge (dag-draw-spanning-tree-edges new-tree)) :to-be nil)

                ;; Entering edge should be represented in the tree (if it was provided)
                (when entering-edge
                  (expect (cl-some (lambda (tree-edge)
                                     (and (eq (dag-draw-edge-from-node entering-edge)
                                              (dag-draw-tree-edge-from-node tree-edge))
                                          (eq (dag-draw-edge-to-node entering-edge)
                                              (dag-draw-tree-edge-to-node tree-edge))))
                                   (dag-draw-spanning-tree-edges new-tree)) :to-be-truthy))))))))

  (describe "iteration convergence and optimization"
    (it "should perform single iteration of network simplex"
        ;; RED phase: This test will fail because iteration step doesn't exist yet
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'p "P")
          (dag-draw-add-node graph 'q "Q")
          (dag-draw-add-node graph 'r "R")
          (dag-draw-add-edge graph 'p 'q 1)
          (dag-draw-add-edge graph 'p 'r 3)  ; Higher weight - should be optimized
          (dag-draw-add-edge graph 'q 'r 1)

          (let* ((spanning-tree (dag-draw--create-feasible-spanning-tree graph))
                 (iteration-result (dag-draw--perform-simplex-iteration graph spanning-tree)))

            ;; Iteration should return result information
            (expect (ht-get iteration-result 'success) :to-be-truthy)

            ;; Should indicate whether optimization occurred
            (expect (ht-contains-p iteration-result 'optimized) :to-be t)

            ;; Should return updated spanning tree
            (expect (ht-get iteration-result 'spanning-tree) :to-be-truthy))))

    (it "should detect convergence when no negative cut values exist"
        ;; RED phase: This test will fail because convergence detection doesn't exist yet
        (let ((graph (dag-draw-create-graph)))
          ;; Create simple optimal graph
          (dag-draw-add-node graph 'm "M")
          (dag-draw-add-node graph 'n "N")
          (dag-draw-add-edge graph 'm 'n 1)

          (let* ((spanning-tree (dag-draw--create-feasible-spanning-tree graph))
                 (is-optimal (dag-draw--is-spanning-tree-optimal graph spanning-tree)))

            ;; Should detect when spanning tree is optimal
            (expect is-optimal :to-be-truthy)))))

  (describe "complete network simplex optimization"
    (it "should optimize spanning tree to convergence"
        ;; RED phase: This test will fail because full optimization doesn't exist yet
        (let ((graph (dag-draw-create-graph)))
          ;; Create graph requiring multiple iterations
          (dag-draw-add-node graph 'start "Start")
          (dag-draw-add-node graph 'mid1 "Mid1")
          (dag-draw-add-node graph 'mid2 "Mid2")
          (dag-draw-add-node graph 'end "End")
          (dag-draw-add-edge graph 'start 'mid1 1)
          (dag-draw-add-edge graph 'start 'mid2 2)
          (dag-draw-add-edge graph 'mid1 'end 1)
          (dag-draw-add-edge graph 'mid2 'end 1)
          (dag-draw-add-edge graph 'mid1 'mid2 3)  ; Cross edge

          ;; Test end-to-end: call the actual public API that users would call
          (dag-draw-assign-ranks graph)

          ;; Verify that optimization converged and ranks are properly assigned
          (let ((start-rank (dag-draw-node-rank (dag-draw-get-node graph 'start)))
                (mid1-rank (dag-draw-node-rank (dag-draw-get-node graph 'mid1)))
                (mid2-rank (dag-draw-node-rank (dag-draw-get-node graph 'mid2)))
                (end-rank (dag-draw-node-rank (dag-draw-get-node graph 'end))))

            ;; All nodes should have ranks assigned (shows convergence)
            (expect start-rank :to-be-truthy)
            (expect mid1-rank :to-be-truthy)
            (expect mid2-rank :to-be-truthy)
            (expect end-rank :to-be-truthy)

            ;; Verify topological ordering is maintained
            (expect (< start-rank end-rank) :to-be t)
            (expect (< mid1-rank end-rank) :to-be t)
            (expect (< mid2-rank end-rank) :to-be t))))))

(provide 'dag-draw-network-simplex-iteration-test)

;;; dag-draw-network-simplex-iteration-test.el ends here
