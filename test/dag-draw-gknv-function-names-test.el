;;; dag-draw-gknv-function-names-test.el --- Tests for standard GKNV function names -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Tests to ensure all standard GKNV function names from Figure 1-1 are implemented
;; with proper dag-draw- prefix. This validates the canonical interface compliance
;; per "A Technique for Drawing Directed Graphs" paper specification.
;;
;; GKNV Reference: Figure 1-1 - Main algorithm entry points
;; Ubiquitous Language: Standard function names must match paper specification

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)

(describe "GKNV Standard Function Names Compliance"
  
  (describe "Pass 1: Rank Assignment Functions"
    
    (it "should have dag-draw-rank entry point"
      ;; RED TEST: This should fail - dag-draw-rank doesn't exist yet
      ;; GKNV Figure 1-1: rank(G) as main entry point for Pass 1
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'source "Source")
        (dag-draw-add-node graph 'target "Target")
        (dag-draw-add-edge graph 'source 'target)
        
        ;; Should have canonical GKNV rank() function
        (expect (fboundp 'dag-draw-rank) :to-be t)
        
        ;; Should assign ranks when called
        (dag-draw-rank graph)
        (let* ((source-node (dag-draw-get-node graph 'source))
               (target-node (dag-draw-get-node graph 'target))
               (source-rank (dag-draw-node-λ source-node))
               (target-rank (dag-draw-node-λ target-node)))
          (expect source-rank :not :to-be nil)
          (expect target-rank :not :to-be nil)
          (expect target-rank :to-be-greater-than source-rank))))
    
    (it "should have dag-draw-init-rank from Figure 2-2"
      ;; RED TEST: init_rank() function from GKNV Figure 2-2
      (expect (fboundp 'dag-draw-init-rank) :to-be t))
    
    (it "should have dag-draw-feasible-tree from Figure 2-2"
      ;; RED TEST: feasible_tree() function from GKNV Figure 2-2  
      (expect (fboundp 'dag-draw-feasible-tree) :to-be t)))
  
  (describe "Pass 2: Vertex Ordering Functions"
    
    (it "should have dag-draw-ordering entry point"
      ;; RED TEST: ordering(G) as main entry point for Pass 2
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-edge graph 'a 'b)
        
        ;; Should have canonical GKNV ordering() function
        (expect (fboundp 'dag-draw-ordering) :to-be t)
        
        ;; Should set vertex orders when called (after ranking)
        (dag-draw-rank graph)
        (dag-draw-ordering graph)
        (let* ((node-a (dag-draw-get-node graph 'a))
               (node-b (dag-draw-get-node graph 'b)))
          (expect (dag-draw-node-order node-a) :not :to-be nil)
          (expect (dag-draw-node-order node-b) :not :to-be nil)))))
  
  (describe "Pass 3: Coordinate Assignment Functions"
    
    (it "should have dag-draw-position entry point"
      ;; RED TEST: position(G) as main entry point for Pass 3
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'node "Node")
        
        ;; Should have canonical GKNV position() function
        (expect (fboundp 'dag-draw-position) :to-be t)
        
        ;; Should assign coordinates when called (after rank/order)
        (dag-draw-rank graph)
        (dag-draw-ordering graph) 
        (dag-draw-position graph)
        (let* ((node (dag-draw-get-node graph 'node))
               (x-coord (dag-draw-node-x-coord node))
               (y-coord (dag-draw-node-y-coord node)))
          (expect x-coord :not :to-be nil)
          (expect y-coord :not :to-be nil)))))
  
  (describe "Pass 4: Spline Generation Functions"
    
    (it "should have dag-draw-make-splines entry point"
      ;; RED TEST: make_splines(G) as main entry point for Pass 4
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'from "From")
        (dag-draw-add-node graph 'to "To")
        (dag-draw-add-edge graph 'from 'to)
        
        ;; Should have canonical GKNV make_splines() function
        (expect (fboundp 'dag-draw-make-splines) :to-be t)
        
        ;; Should generate splines when called (after full layout)
        (dag-draw-rank graph)
        (dag-draw-ordering graph)
        (dag-draw-position graph)
        (dag-draw-make-splines graph)
        (let ((edge (car (dag-draw-graph-edges graph))))
          (expect (dag-draw-edge-spline-points edge) :not :to-be nil))))
    
    (it "should have dag-draw-generate-spline from Section 5.2"
      ;; RED TEST: generate_spline() function from GKNV Section 5.2
      (expect (fboundp 'dag-draw-generate-spline) :to-be t))))

(provide 'dag-draw-gknv-function-names-test)

;;; dag-draw-gknv-function-names-test.el ends here