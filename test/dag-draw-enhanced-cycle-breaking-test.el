;;; dag-draw-enhanced-cycle-breaking-test.el --- TDD tests for enhanced cycle breaking -*- lexical-binding: t -*-

;; Copyright (C) 2024, 2025

;;; Commentary:

;; FUTURE ENHANCEMENT - Beyond GKNV Baseline
;;
;; Enhancement Category: Optimization
;; Baseline Status: ⏳ Deferred (Not required for baseline compliance)
;;
;; This test verifies:
;; - Enhanced cycle breaking with intelligent edge weight consideration
;; - Virtual node management for long edge spanning
;; - GKNV minpath virtual chain straightening optimization
;;
;; Related Baseline Decisions: D1.1 (DFS Cycle Breaking), D3.2 (Minpath)
;; Enhancement Source: Extended cycle breaking strategies beyond basic DFS
;;
;; These enhancements may be implemented in Phase 5 (Future Work).
;; See doc/test-suite-analysis.md (Category B1) for categorization rationale.
;;
;; [Original commentary: TDD Implementation of enhanced cycle breaking...]
;;
;; TDD Implementation of enhanced cycle breaking with virtual node management.
;; This implements intelligent cycle breaking that preserves graph structure quality.

;;; Code:

(require 'buttercup)
(require 'dag-draw-pass1-ranking)
(require 'dag-draw-cycle-breaking)
(require 'dag-draw-pass2-ordering)
(require 'dag-draw-pass3-positioning)
(require 'cl-lib)

(describe "Enhanced cycle breaking and virtual node management"
  (describe "GKNV cycle detection with DFS back-edge removal"
    (it "should detect cycles and break them using GKNV-recommended DFS approach"
      ;; Tests GKNV Section 2.1 - DFS-based cycle breaking algorithm
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")
        ;; Create cycle with different edge weights
        (dag-draw-add-edge graph 'a 'b 1)   ; Low weight - prefer to remove
        (dag-draw-add-edge graph 'b 'c 5)   ; High weight - preserve
        (dag-draw-add-edge graph 'c 'a 3)   ; Medium weight
        
        ;; Apply GKNV DFS edge classification cycle breaking (reverses back edges)
        (let* ((original-edge-count (length (dag-draw-graph-edges graph)))
               (has-cycles-before (dag-draw-simple-has-cycles graph)))
          ;; Should detect cycles in original graph
          (expect has-cycles-before :to-be t)
          
          ;; Apply GKNV cycle breaking (modifies graph in-place)
          (dag-draw--break-cycles-using-gknv-classification graph)
          
          (let ((final-edge-count (length (dag-draw-graph-edges graph)))
                (has-cycles-after (dag-draw-simple-has-cycles graph)))
            ;; GKNV preserves all edges (reverses back edges, doesn't remove)
            (expect final-edge-count :to-equal original-edge-count)
            ;; Should break cycles by reversing back edges
            (expect has-cycles-after :to-be nil)
            ;; Should produce acyclic graph
            (expect (dag-draw-simple-has-cycles graph) :to-be nil))))))
  
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
        
        ;; Apply virtual node insertion using functional interface
        (let* ((virtual-graph (dag-draw--create-virtual-nodes graph))
               ;; Extract virtual nodes (IDs starting with "virtual_")
               (virtual-nodes (cl-remove-if-not 
                              (lambda (node-id) 
                                (string-match "^virtual_" (symbol-name node-id)))
                              (ht-keys (dag-draw-graph-nodes virtual-graph)))))
          (expect virtual-nodes :not :to-be nil)
          (expect (length virtual-nodes) :to-be 2)  ; For ranks 1,2
          ;; Virtual nodes should have proper ranks
          (dolist (vnode virtual-nodes)
            (let ((vnode-obj (dag-draw-get-node virtual-graph vnode)))
              (expect (and (>= (dag-draw-node-rank vnode-obj) 1)
                           (<= (dag-draw-node-rank vnode-obj) 2)) :to-be t)))))))
  
  (describe "GKNV minpath virtual node chain straightening"
    (it "should align virtual nodes in chains for straight edge appearance"
      ;; Tests GKNV minpath algorithm - virtual nodes should be aligned, not removed
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'real1 "Real1")
        (dag-draw-add-node graph 'real2 "Real2") 
        (dag-draw-add-node graph 'virtual_1 "")  ; Virtual node for long edge
        (dag-draw-add-edge graph 'real1 'virtual_1)
        (dag-draw-add-edge graph 'virtual_1 'real2)
        
        ;; Set up ranks for vertical chain
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'real1)) 0)
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'virtual_1)) 1)
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'real2)) 2)
        
        ;; Set misaligned X coordinates
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'real1)) 100)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'virtual_1)) 150)  ; misaligned
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'real2)) 100)
        
        ;; Apply GKNV minpath straightening
        (dag-draw--minpath-straighten-virtual-chains graph)
        
        ;; Virtual node should be aligned between endpoints for straight edge
        (let ((virtual1-x (dag-draw-node-x-coord (dag-draw-get-node graph 'virtual_1))))
          (expect virtual1-x :to-be-close-to 100 10))))))

(provide 'dag-draw-enhanced-cycle-breaking-test)

;;; dag-draw-enhanced-cycle-breaking-test.el ends here