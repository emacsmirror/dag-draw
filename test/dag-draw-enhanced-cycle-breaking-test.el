;;; dag-draw-enhanced-cycle-breaking-test.el --- TDD tests for enhanced cycle breaking -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; TDD Implementation of enhanced cycle breaking with virtual node management.
;; This implements intelligent cycle breaking that preserves graph structure quality.

;;; Code:

(require 'buttercup)
(require 'dag-draw-pass1-ranking)

(describe "Enhanced cycle breaking and virtual node management"
  (describe "intelligent cycle detection with edge priorities"
    (it "should detect cycles and prioritize edge removal based on edge weights"
      ;; RED phase: This test will fail because priority-based breaking doesn't exist yet
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")
        ;; Create cycle with different edge weights
        (dag-draw-add-edge graph 'a 'b 1)   ; Low weight - prefer to remove
        (dag-draw-add-edge graph 'b 'c 5)   ; High weight - preserve
        (dag-draw-add-edge graph 'c 'a 3)   ; Medium weight
        
        ;; Apply intelligent cycle breaking
        (let ((result (dag-draw--intelligent-cycle-breaking graph)))
          (expect (ht-get result 'cycles-broken) :to-be t)
          (expect (ht-get result 'edges-removed) :not :to-be nil)
          ;; Should prefer to remove low-weight edges
          (expect (dag-draw--graph-is-acyclic-p graph) :to-be t))))))
  
  (describe "virtual node creation for long edges"
    (it "should create virtual nodes to break long edges spanning multiple ranks"
      ;; RED phase: This test will fail because virtual node creation doesn't exist yet
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'start "Start")
        (dag-draw-add-node graph 'end "End")
        (dag-draw-add-edge graph 'start 'end)
        
        ;; Set up ranks that create a long edge (span > 1)
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'start)) 0)
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'end)) 3)  ; Spans ranks 1,2
        
        ;; Apply virtual node insertion
        (let ((result (dag-draw--insert-virtual-nodes-for-long-edges graph)))
          (expect (ht-get result 'virtual-nodes-created) :not :to-be nil)
          (expect (length (ht-get result 'virtual-nodes-created)) :to-be 2)  ; For ranks 1,2
          ;; Virtual nodes should have proper ranks
          (let ((virtual-nodes (ht-get result 'virtual-nodes-created)))
            (dolist (vnode virtual-nodes)
              (let ((vnode-obj (dag-draw-get-node graph vnode)))
                (expect (and (>= (dag-draw-node-rank vnode-obj) 1)
                             (<= (dag-draw-node-rank vnode-obj) 2)) :to-be t))))))))
  
  (describe "virtual node cleanup and optimization"
    (it "should remove unnecessary virtual nodes and optimize edge paths"
      ;; RED phase: This test will fail because cleanup doesn't exist yet
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'real1 "Real1")
        (dag-draw-add-node graph 'real2 "Real2")
        (dag-draw-add-node graph 'virtual1 "Virtual1")  ; Should be optimized away
        (dag-draw-add-edge graph 'real1 'virtual1)
        (dag-draw-add-edge graph 'virtual1 'real2)
        
        ;; Mark virtual node
        (setf (dag-draw-node-virtual-p (dag-draw-get-node graph 'virtual1)) t)
        
        ;; Apply cleanup
        (let ((result (dag-draw--cleanup-unnecessary-virtual-nodes graph)))
          (expect (ht-get result 'nodes-removed) :not :to-be nil)
          (expect (ht-get result 'edges-optimized) :not :to-be nil)
          ;; Direct edge should exist between real nodes
          (expect (dag-draw--edge-exists-p graph 'real1 'real2) :to-be t)))))

(provide 'dag-draw-enhanced-cycle-breaking-test)

;;; dag-draw-enhanced-cycle-breaking-test.el ends here