;;; dag-draw-coordinate-mode-test.el --- Tests for coordinate-mode parameter -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Tests for coordinate-mode parameter functionality.
;; Implements Phase 2 of ASCII-native GKNV implementation.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)

(describe "Coordinate-mode parameter"
  
  (describe "dag-draw-layout-graph with coordinate-mode"
    (it "should accept coordinate-mode parameter"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-node graph 'b "Node B")
        (dag-draw-add-edge graph 'a 'b)
        
        ;; This should not fail - coordinate-mode parameter should be accepted
        (expect (dag-draw-layout-graph graph :coordinate-mode 'ascii)
                :not :to-be nil)))
    
    (it "should operate in ASCII-native mode when coordinate-mode is 'ascii"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-node graph 'b "Node B")
        (dag-draw-add-edge graph 'a 'b)
        
        ;; Layout with ASCII coordinate mode
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)
        
        ;; In ASCII mode, coordinates should be integer-friendly for grid positioning
        (let ((node-a (dag-draw-get-node graph 'a))
              (node-b (dag-draw-get-node graph 'b)))
          ;; ASCII mode should produce coordinates that work well with grid conversion
          ;; (exact values will depend on implementation, but should be reasonable integers)
          (expect (dag-draw-node-x-coord node-a) :to-be-greater-than 0)
          (expect (dag-draw-node-y-coord node-a) :to-be-greater-than 0)
          (expect (dag-draw-node-x-coord node-b) :to-be-greater-than 0) 
          (expect (dag-draw-node-y-coord node-b) :to-be-greater-than 0))))
    
    (it "should operate in high-resolution mode by default"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-node graph 'b "Node B")
        (dag-draw-add-edge graph 'a 'b)
        
        ;; Layout without coordinate-mode (should default to high-res)
        (dag-draw-layout-graph graph)
        
        ;; Should work as before - coordinates may be floating point
        (let ((node-a (dag-draw-get-node graph 'a))
              (node-b (dag-draw-get-node graph 'b)))
          (expect (dag-draw-node-x-coord node-a) :not :to-be nil)
          (expect (dag-draw-node-y-coord node-a) :not :to-be nil)
          (expect (dag-draw-node-x-coord node-b) :not :to-be nil) 
          (expect (dag-draw-node-y-coord node-b) :not :to-be nil))))
    
    (it "should produce different results for ASCII vs high-res modes"
      (let ((graph1 (dag-draw-create-graph))
            (graph2 (dag-draw-create-graph)))
        ;; Create identical graphs
        (dag-draw-add-node graph1 'a "Node A")
        (dag-draw-add-node graph1 'b "Node B")
        (dag-draw-add-edge graph1 'a 'b)
        
        (dag-draw-add-node graph2 'a "Node A")
        (dag-draw-add-node graph2 'b "Node B")
        (dag-draw-add-edge graph2 'a 'b)
        
        ;; Layout with different modes
        (dag-draw-layout-graph graph1 :coordinate-mode 'ascii)
        (dag-draw-layout-graph graph2) ; default high-res mode
        
        ;; ASCII mode should produce coordinates optimized for ASCII grid
        ;; while high-res mode uses the traditional GKNV coordinate system
        (let ((ascii-node-a (dag-draw-get-node graph1 'a))
              (highres-node-a (dag-draw-get-node graph2 'a)))
          ;; The coordinate systems should be different enough to be distinguishable
          ;; (This test may need adjustment based on actual implementation)
          (expect (abs (- (dag-draw-node-x-coord ascii-node-a) 
                         (dag-draw-node-x-coord highres-node-a)))
                  :to-be-greater-than 0.1))))))

;;; dag-draw-coordinate-mode-test.el ends here