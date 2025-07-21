;;; dag-draw-canonical-interface-test.el --- Test the canonical GKNV interface -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Integration test demonstrating the canonical GKNV interface using
;; standard function names from Figure 1-1. This validates that users
;; can now use the paper's exact function names for algorithm steps.
;;
;; GKNV Reference: Figure 1-1 - Four-pass algorithm structure
;; Ubiquitous Language: Users should be able to call rank(), ordering(), position(), make_splines()

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)

(describe "GKNV Canonical Interface Integration"
  
  (it "should allow users to call the four-pass algorithm using standard GKNV names"
    ;; Integration test using canonical GKNV function names from Figure 1-1
    (let ((graph (dag-draw-create-graph)))
      ;; Create a simple three-node graph
      (dag-draw-add-node graph 'start "Start")
      (dag-draw-add-node graph 'middle "Middle") 
      (dag-draw-add-node graph 'end "End")
      (dag-draw-add-edge graph 'start 'middle)
      (dag-draw-add-edge graph 'middle 'end)
      
      ;; Call the four-pass algorithm using canonical GKNV names
      ;; This mirrors exactly what's shown in Figure 1-1
      
      ;; Pass 1: rank(G) - assign nodes to discrete ranks
      (dag-draw-rank graph)
      
      ;; Verify ranks were assigned
      (let* ((start-node (dag-draw-get-node graph 'start))
             (middle-node (dag-draw-get-node graph 'middle))
             (end-node (dag-draw-get-node graph 'end)))
        (expect (dag-draw-node-λ start-node) :not :to-be nil)
        (expect (dag-draw-node-λ middle-node) :not :to-be nil) 
        (expect (dag-draw-node-λ end-node) :not :to-be nil)
        (expect (dag-draw-node-λ end-node) :to-be-greater-than (dag-draw-node-λ start-node)))
      
      ;; Pass 2: ordering(G) - determine vertex order within ranks
      (dag-draw-ordering graph)
      
      ;; Verify orders were assigned
      (let* ((start-node (dag-draw-get-node graph 'start))
             (middle-node (dag-draw-get-node graph 'middle))
             (end-node (dag-draw-get-node graph 'end)))
        (expect (dag-draw-node-order start-node) :not :to-be nil)
        (expect (dag-draw-node-order middle-node) :not :to-be nil)
        (expect (dag-draw-node-order end-node) :not :to-be nil))
      
      ;; Pass 3: position(G) - assign coordinates to nodes
      (dag-draw-position graph)
      
      ;; Verify coordinates were assigned
      (let* ((start-node (dag-draw-get-node graph 'start))
             (middle-node (dag-draw-get-node graph 'middle))
             (end-node (dag-draw-get-node graph 'end)))
        (expect (dag-draw-node-x-coord start-node) :not :to-be nil)
        (expect (dag-draw-node-y-coord start-node) :not :to-be nil)
        (expect (dag-draw-node-x-coord middle-node) :not :to-be nil)
        (expect (dag-draw-node-y-coord middle-node) :not :to-be nil)
        (expect (dag-draw-node-x-coord end-node) :not :to-be nil)
        (expect (dag-draw-node-y-coord end-node) :not :to-be nil))
      
      ;; Pass 4: make_splines(G) - generate spline curves for edges
      (dag-draw-make-splines graph)
      
      ;; Verify splines were generated
      (let ((edges (dag-draw-graph-edges graph)))
        (expect (length edges) :to-equal 2)
        (dolist (edge edges)
          (expect (dag-draw-edge-spline-points edge) :not :to-be nil)))))
  
  (it "should allow access to individual GKNV canonical functions"
    ;; Test individual canonical function access
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'a "A")
      (dag-draw-add-node graph 'b "B")
      (dag-draw-add-edge graph 'a 'b)
      
      ;; Test init_rank() from Figure 2-2
      (dag-draw-init-rank graph)
      (expect (dag-draw-node-λ (dag-draw-get-node graph 'a)) :not :to-be nil)
      
      ;; Test feasible_tree() from Figure 2-2  
      (let ((tree (dag-draw-feasible-tree graph)))
        (expect tree :not :to-be nil))
      
      ;; Complete layout for spline test
      (dag-draw-ordering graph)
      (dag-draw-position graph)
      
      ;; Test generate_spline() from Section 5.2
      ;; Note: generate_spline works on individual edges, but requires full layout first
      (dag-draw-make-splines graph)  ; Generate all splines first
      (let ((edge (car (dag-draw-graph-edges graph))))
        (expect (dag-draw-edge-spline-points edge) :not :to-be nil)))))

(provide 'dag-draw-canonical-interface-test)

;;; dag-draw-canonical-interface-test.el ends here