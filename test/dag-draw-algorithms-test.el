;;; dag-draw-algorithms-test.el --- Tests for dag-draw-algorithms.el -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Tests for graph algorithms including DFS, cycle detection, and topological sorting.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-algorithms)

(describe "dag-draw-algorithms"
  
  (describe "depth-first search"
    (it "should perform DFS on acyclic graph"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-node graph 'c)
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'a 'c)
        
        (let ((result (dag-draw-dfs graph)))
          (expect (plist-get result :visited) :to-be-truthy)
          (expect (plist-get result :pre-order) :to-be-truthy)
          (expect (plist-get result :post-order) :to-be-truthy)
          (expect (plist-get result :edge-classification) :to-be-truthy))))
    
    (it "should classify edges correctly"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-node graph 'c)
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'a 'c)
        (dag-draw-add-edge graph 'b 'c)  ; This should be forward or cross edge
        
        (let* ((result (dag-draw-dfs graph))
               (classifications (plist-get result :edge-classification)))
          (expect (length classifications) :to-equal 3)
          ;; Should have at least some tree edges
          (expect (--some (eq (cadr it) 'tree) classifications) :to-be t)))))

  (describe "cycle detection"
    (it "should detect no cycles in acyclic graph"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-node graph 'c)
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)
        
        (expect (dag-draw-detect-cycles graph) :to-equal nil)))
    
    (it "should detect cycles in cyclic graph"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-node graph 'c)
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)
        (dag-draw-add-edge graph 'c 'a)  ; Creates cycle
        
        (let ((back-edges (dag-draw-detect-cycles graph)))
          (expect (length back-edges) :to-be-greater-than 0))))
    
    
    (it "should handle disconnected components"
      (let ((graph (dag-draw-create-graph)))
        ;; Component 1: acyclic
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-edge graph 'a 'b)
        
        ;; Component 2: cyclic
        (dag-draw-add-node graph 'x)
        (dag-draw-add-node graph 'y)
        (dag-draw-add-edge graph 'x 'y)
        (dag-draw-add-edge graph 'y 'x)
        
        (let ((back-edges (dag-draw-detect-cycles graph)))
          (expect (length back-edges) :to-be-greater-than 0)))))





)

;;; dag-draw-algorithms-test.el ends here