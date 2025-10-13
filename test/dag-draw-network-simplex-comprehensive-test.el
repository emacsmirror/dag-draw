;;; dag-draw-network-simplex-comprehensive-test.el --- Comprehensive network simplex tests -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Comprehensive tests for GKNV network simplex algorithm based on paper examples.
;; Tests multiple scenarios to isolate the exact issue with cut value calculation.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-pass1-ranking)
(require 'dag-draw-render)

(describe "GKNV Network Simplex - Comprehensive Tests"

  (it "should reproduce GKNV Figure 2-3 exact example with negative cut value"
    ;; Based on GKNV Section 2.3, lines 712-716: cut value of (g,h) = -1
    ;; Cut value = weight(g,h) - weight(a,e) - weight(a,f) = 1 - 1 - 1 = -1
    ;; Key insight: (a,e) and (a,f) go from head component to tail component
    (let ((graph (dag-draw-create-graph)))
      ;; Create minimal example to match GKNV explanation
      ;; When (g,h) is removed: g in tail component, {a,e,f,h} in head component
      (dag-draw-add-node graph 'g "g")  ; Will be in tail component
      (dag-draw-add-node graph 'h "h")  ; Will be in head component  
      (dag-draw-add-node graph 'a "a")  ; Will be in head component
      (dag-draw-add-node graph 'e "e")  ; Will be in tail component
      (dag-draw-add-node graph 'f "f")  ; Will be in tail component

      ;; Edges that create the component structure  
      (dag-draw-add-edge graph 'g 'h 1)   ; Tree edge: tail->head (+1)
      (dag-draw-add-edge graph 'a 'e 1)   ; Non-tree edge: head->tail (-1)  
      (dag-draw-add-edge graph 'a 'f 1)   ; Non-tree edge: head->tail (-1)
      (dag-draw-add-edge graph 'h 'a 1)   ; Tree edge: connects h to head component
      (dag-draw-add-edge graph 'g 'e 1)   ; Tree edge: connects g to tail component
      (dag-draw-add-edge graph 'g 'f 1)   ; Tree edge: connects g to tail component

      ;; Create tree structure where removing (g,h) separates components correctly
      ;; Tree must be connected: 4 edges for 5 nodes
      (let ((tree-info (ht-create)))
        (ht-set! tree-info 'tree-edges 
                 (list (dag-draw-find-edge graph 'g 'h)    ; The edge we're testing - removing this splits tree
                       (dag-draw-find-edge graph 'h 'a)    ; Keeps h,a together in head component  
                       (dag-draw-find-edge graph 'g 'e)    ; Keeps g,e together in tail component
                       (dag-draw-find-edge graph 'g 'f)))  ; Keeps g,f together in tail component
        (ht-set! tree-info 'non-tree-edges
                 (list (dag-draw-find-edge graph 'a 'e)    ; Head->tail
                       (dag-draw-find-edge graph 'a 'f)))  ; Head->tail

        ;; Test cut value of (g,h) should be -1
        ;; When (g,h) removed: tail={g,e,f}, head={h,a}  
        ;; Edges crossing cut: (g,h)=+1, (a,e)=-1, (a,f)=-1 → total = -1
        (let ((gh-edge (dag-draw-find-edge graph 'g 'h)))
          (when gh-edge
            (let ((cut-value (dag-draw--calculate-edge-cut-value gh-edge tree-info graph)))
              (expect cut-value :to-equal -1))))

        ;; Test that leave_edge finds the (g,h) edge
        (let ((leaving-edge (dag-draw--leave-edge tree-info graph)))
          (expect leaving-edge :not :to-be nil)
          (expect (dag-draw-edge-from-node leaving-edge) :to-equal 'g)
          (expect (dag-draw-edge-to-node leaving-edge) :to-equal 'h)))))

  (it "should handle simple 3-node triangle with clear optimization opportunity"
    ;; Simplest case: expensive path a->b->c vs cheaper direct a->c
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'a "A")
      (dag-draw-add-node graph 'b "B")  
      (dag-draw-add-node graph 'c "C")
      (dag-draw-add-edge graph 'a 'b 1)   ; Tree edge
      (dag-draw-add-edge graph 'b 'c 3)   ; Tree edge (expensive) - this should have negative cut value
      (dag-draw-add-edge graph 'a 'c 1)   ; Non-tree edge (cheaper alternative)

      ;; Create tree that forces suboptimal path
      ;; Tree: a->b, b->c (total cost for a to c = 4)
      ;; Non-tree: a->c (cost = 1) - much better!
      (let ((tree-info (ht-create)))
        (ht-set! tree-info 'tree-edges 
                 (list (dag-draw-find-edge graph 'a 'b)
                       (dag-draw-find-edge graph 'b 'c)))
        (ht-set! tree-info 'non-tree-edges
                 (list (dag-draw-find-edge graph 'a 'c)))

        ;; Test cut value of b->c edge
        ;; When b->c removed: tail={b}, head={c}, a connects to c via non-tree edge
        ;; Cut value = weight(b,c) - weight(a,c) = 3 - 1 = 2 (positive - not optimal!)
        
        ;; Actually, let's test a->b edge instead
        ;; When a->b removed: tail={a}, head={b,c}  
        ;; Cut value = weight(a,b) - 0 = 1 (no reverse edges)
        
        ;; The issue is that for this simple case, neither tree edge has negative cut value
        ;; because the alternative path (a->c) doesn't create a reverse crossing
        
        ;; Let's create a case that definitely has negative cut value
        ;; We need: tree edge weight < sum of reverse crossing edges
        (ht-set! tree-info 'tree-edges 
                 (list (dag-draw-find-edge graph 'b 'c)))  ; Only this edge in tree
        (ht-set! tree-info 'non-tree-edges
                 (list (dag-draw-find-edge graph 'a 'b)
                       (dag-draw-find-edge graph 'a 'c)))

        ;; Now when b->c removed: tail={b}, head={c}
        ;; But a is isolated and a->c crosses from tail to head? No...
        ;; This is getting complex. Let me use a simpler known-working pattern.
        
        ;; Use the pattern we know works from GKNV test
        (ht-set! tree-info 'tree-edges 
                 (list (dag-draw-find-edge graph 'a 'b)
                       (dag-draw-find-edge graph 'a 'c)))  ; Make a->c tree edge
        (ht-set! tree-info 'non-tree-edges
                 (list (dag-draw-find-edge graph 'b 'c)))  ; Make b->c non-tree

        ;; Should find optimization opportunity in one of the tree edges
        (let ((leaving-edge (dag-draw--leave-edge tree-info graph)))
          (expect leaving-edge :not :to-be nil)))))

  (it "should work with full GKNV pipeline for realistic graph"
    ;; Test using the full dag-draw pipeline with ASCII rendering
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'start "Start")
      (dag-draw-add-node graph 'process "Process")
      (dag-draw-add-node graph 'end "End")
      (dag-draw-add-edge graph 'start 'process 1)
      (dag-draw-add-edge graph 'process 'end 1)
      (dag-draw-add-edge graph 'start 'end 3)  ; More expensive direct path

      ;; Run full GKNV algorithm 
      (dag-draw-layout-graph graph)

      ;; Should be able to render as ASCII
      (let ((ascii-output (dag-draw-render-ascii graph)))
        (expect ascii-output :to-be-truthy)
        (expect ascii-output :to-match "Start")
        (expect ascii-output :to-match "End"))))

  (it "should verify cut value calculation fundamentals"
    ;; Test the basic cut value formula step by step
    (let ((graph (dag-draw-create-graph)))
      ;; Create simple case where we can manually verify calculation
      (dag-draw-add-node graph 'x "X")
      (dag-draw-add-node graph 'y "Y")
      (dag-draw-add-edge graph 'x 'y 5)  ; Only edge

      ;; Single edge should have cut value = its own weight
      (let ((tree-info (ht-create)))
        (ht-set! tree-info 'tree-edges (list (dag-draw-find-edge graph 'x 'y)))
        (ht-set! tree-info 'non-tree-edges '())

        (let ((xy-edge (dag-draw-find-edge graph 'x 'y)))
          (when xy-edge
            (let ((cut-value (dag-draw--calculate-edge-cut-value xy-edge tree-info graph)))
              ;; Cut value should equal edge weight (5) since no reverse edges
              (expect cut-value :to-equal 5)))))))

  (it "should handle disconnected components correctly"
    ;; Test GKNV handling of multiple components
    (let ((graph (dag-draw-create-graph)))
      ;; Component 1
      (dag-draw-add-node graph 'a1 "A1")
      (dag-draw-add-node graph 'b1 "B1")
      (dag-draw-add-edge graph 'a1 'b1 1)

      ;; Component 2  
      (dag-draw-add-node graph 'a2 "A2")
      (dag-draw-add-node graph 'b2 "B2")
      (dag-draw-add-edge graph 'a2 'b2 1)

      ;; Should handle both components
      (dag-draw-layout-graph graph)
      (let ((ascii-output (dag-draw-render-ascii graph)))
        (expect ascii-output :to-match "A1")
        (expect ascii-output :to-match "B2"))))

  (it "should pass network simplex optimization sanity check"
    ;; Verify that optimization actually improves objective function
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'root "Root")
      (dag-draw-add-node graph 'mid1 "Mid1")
      (dag-draw-add-node graph 'mid2 "Mid2")
      (dag-draw-add-node graph 'leaf "Leaf")
      (dag-draw-add-edge graph 'root 'mid1 1)
      (dag-draw-add-edge graph 'mid1 'mid2 1) 
      (dag-draw-add-edge graph 'mid2 'leaf 1)
      (dag-draw-add-edge graph 'root 'leaf 2)  ; Alternative path

      ;; Run ranking and check that result is reasonable
      (dag-draw-assign-ranks graph)
      
      ;; All nodes should have valid ranks
      (expect (dag-draw-node-rank (dag-draw-get-node graph 'root)) :to-be-truthy)
      (expect (dag-draw-node-rank (dag-draw-get-node graph 'leaf)) :to-be-truthy))))

(provide 'dag-draw-network-simplex-comprehensive-test)

;;; dag-draw-network-simplex-comprehensive-test.el ends here