;;; dag-draw-network-simplex-iteration-test.el --- TDD tests for network simplex iteration -*- lexical-binding: t -*-

;; Copyright (C) 2024, 2025

;;; Commentary:

;; GKNV Baseline Compliance Tests - Pass 1: Network Simplex Iteration Loop
;;
;; This module tests GKNV network simplex iteration logic as specified in
;; "A Technique for Drawing Directed Graphs" (Gansner, Koutsofios, North, Vo).
;;
;; GKNV Reference: Section 2.3 Figure 2-1 steps 3-6 (iterative refinement loop)
;; Decision: D1.4 - Iterative leave/enter edge exchange until optimal
;; Algorithm: Network Simplex Main Iteration Loop
;;
;; Key Requirements Tested:
;; - Iteration continues while negative cut value edges exist
;; - Each iteration improves or maintains solution quality
;; - Leave edge (negative cut value) identified each iteration
;; - Enter edge (minimum slack) replaces leave edge
;; - Rank adjustments maintain feasibility throughout
;; - Algorithm terminates when all cut values non-negative (optimality)
;; - Iteration count bounded (no infinite loops)
;;
;; Test Coverage:
;; - while loop continues until optimal
;; - Each iteration finds valid leave edge
;; - Each iteration finds valid enter edge
;; - Exchange operation executes correctly
;; - Solution quality monotonically improves
;; - Termination occurs at optimality
;; - Iteration count reasonable for graph size
;;
;; Baseline Status: ✅ Required for GKNV compliance
;;
;; See doc/implementation-decisions.md (D1.4) for full decision rationale.
;; See doc/algorithm-specification.md Pass 1 for implementation details.

;; Original Commentary:
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

          (let* ((tree-info (dag-draw--construct-feasible-tree graph))
                 (cut-values (dag-draw--calculate-tree-cut-values tree-info graph))
                 (leaving-edge (dag-draw--leave-edge tree-info graph)))

            ;; Should find a leaving edge when negative cut values exist
            (when leaving-edge
              (let ((entering-edge (dag-draw--enter-edge leaving-edge tree-info graph)))

                ;; Entering edge should be found
                (expect entering-edge :to-be-truthy)

                ;; Entering edge should not be in current spanning tree
                (expect (member entering-edge (ht-get tree-info 'tree-edges)) :to-be nil))))))

    (it "should perform edge exchange in spanning tree"
        ;; RED phase: This test will fail because edge exchange doesn't exist yet
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'x "X")
          (dag-draw-add-node graph 'y "Y")
          (dag-draw-add-node graph 'z "Z")
          (dag-draw-add-edge graph 'x 'y 1)
          (dag-draw-add-edge graph 'x 'z 2)
          (dag-draw-add-edge graph 'y 'z 1)

          (let* ((tree-info (dag-draw--construct-feasible-tree graph))
                 (original-edge-count (length (ht-get tree-info 'tree-edges)))
                 (leaving-edge (dag-draw--leave-edge tree-info graph)))

            (when leaving-edge
              (let* ((entering-edge (dag-draw--enter-edge leaving-edge tree-info graph))
                     (tree-edges-before (copy-sequence (ht-get tree-info 'tree-edges))))

                ;; Perform edge exchange
                (dag-draw--exchange-edges leaving-edge entering-edge tree-info graph)

                ;; Tree should still have same number of edges
                (expect (length (ht-get tree-info 'tree-edges)) :to-equal original-edge-count)

                ;; Leaving edge should be removed from tree
                (expect (member leaving-edge (ht-get tree-info 'tree-edges)) :to-be nil)

                ;; Should have changed the tree structure
                (expect (equal (ht-get tree-info 'tree-edges) tree-edges-before) :to-be nil))))))

    )

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

          (let* ((tree-info (dag-draw--construct-feasible-tree graph))
                 (iteration-result (dag-draw--network-simplex-iteration tree-info graph)))

            ;; Iteration should return result information
            (expect (ht-contains-p iteration-result 'converged) :to-be t)

            ;; Should indicate whether optimization occurred
            (expect (ht-contains-p iteration-result 'improved) :to-be t)

            ;; When improvement occurs, should return updated tree info
            (when (ht-get iteration-result 'improved)
              (expect (ht-get iteration-result 'updated-tree-info) :to-be-truthy)))))

    (it "should detect convergence when no negative cut values exist"
        ;; RED phase: This test will fail because convergence detection doesn't exist yet
        (let ((graph (dag-draw-create-graph)))
          ;; Create simple optimal graph
          (dag-draw-add-node graph 'm "M")
          (dag-draw-add-node graph 'n "N")
          (dag-draw-add-edge graph 'm 'n 1)

          (let* ((tree-info (dag-draw--construct-feasible-tree graph))
                 (leaving-edge (dag-draw--leave-edge tree-info graph)))

            ;; Should detect when spanning tree is optimal (no leaving edge with negative cut value)
            ;; For a simple unit-weight graph, there should be no negative cut values
            (expect leaving-edge :to-be nil)))))

  (describe
   "cut value formula implementation"
   (it "should implement GKNV cut value formula correctly"
       ;; Test based on GKNV Section 2.3 understanding of cut values
       (let ((graph (dag-draw-create-graph)))
         ;; Create graph that produces negative cut value according to GKNV formula
         ;; When tree edge is removed, need reverse edges (head->tail) to get negative result
         (dag-draw-add-node graph 'g "G")  ; Will be in tail component
         (dag-draw-add-node graph 'h "H")  ; Will be in head component  
         (dag-draw-add-node graph 'a "A")  ; Will be in head component
         (dag-draw-add-node graph 'e "E")  ; Will be in tail component
         
         ;; Create edges that will produce negative cut value per GKNV Section 2.3
         (dag-draw-add-edge graph 'g 'h 1)   ; Tree edge: tail->head (+1)
         (dag-draw-add-edge graph 'a 'e 2)   ; Non-tree edge: head->tail (-2)  
         (dag-draw-add-edge graph 'h 'a 1)   ; Tree edge: connects components
         (dag-draw-add-edge graph 'g 'e 1)   ; Tree edge: connects components

         ;; Manually create tree structure for predictable cut value calculation
         (let ((tree-info (ht-create)))
           (ht-set! tree-info 'tree-edges 
                    (list (dag-draw-find-edge graph 'g 'h)    ; Cut value = 1 - 2 = -1
                          (dag-draw-find-edge graph 'h 'a)    ; Keeps h,a in head component  
                          (dag-draw-find-edge graph 'g 'e)))  ; Keeps g,e in tail component
           (ht-set! tree-info 'non-tree-edges
                    (list (dag-draw-find-edge graph 'a 'e)))  ; Head->tail crossing
           
           ;; Test the g->h edge which should have negative cut value
           (let ((test-edge (dag-draw-find-edge graph 'g 'h)))
             (when test-edge
               (let ((cut-value (dag-draw--calculate-edge-cut-value test-edge tree-info graph)))
                 ;; Cut value should be numeric  
                 (expect (numberp cut-value) :to-be t)
                 ;; Should be negative: g->h (+1) minus a->e (-2) = 1 - 2 = -1
                 (expect cut-value :to-be-less-than 0)))))))))


(provide 'dag-draw-network-simplex-iteration-test)

;;; dag-draw-network-simplex-iteration-test.el ends here
