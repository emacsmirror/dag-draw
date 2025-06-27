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
        
        (expect (dag-draw-has-cycles graph) :to-be nil)
        (expect (dag-draw-detect-cycles graph) :to-equal nil)))
    
    (it "should detect cycles in cyclic graph"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-node graph 'c)
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)
        (dag-draw-add-edge graph 'c 'a)  ; Creates cycle
        
        (expect (dag-draw-has-cycles graph) :to-be t)
        (let ((back-edges (dag-draw-detect-cycles graph)))
          (expect (length back-edges) :to-be-greater-than 0))))
    
    (it "should detect self-loops"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-edge graph 'a 'a)  ; Self-loop
        
        (expect (dag-draw-has-cycles graph) :to-be t)))
    
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
        
        (expect (dag-draw-has-cycles graph) :to-be t))))

  (describe "cycle breaking"
    (it "should break cycles by reversing edges"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-node graph 'c)
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)
        (dag-draw-add-edge graph 'c 'a)  ; Creates cycle
        
        (let ((acyclic (dag-draw-break-cycles graph)))
          (expect (dag-draw-has-cycles acyclic) :to-be nil)
          ;; Original graph should be unchanged
          (expect (dag-draw-has-cycles graph) :to-be t)
          ;; Should have same number of edges
          (expect (dag-draw-edge-count acyclic) :to-equal (dag-draw-edge-count graph)))))
    
    (it "should preserve edge attributes when reversing"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-edge graph 'a 'b 3 "test label")
        (dag-draw-add-edge graph 'b 'a 2)  ; Creates cycle
        
        (let ((acyclic (dag-draw-break-cycles graph)))
          (expect (dag-draw-has-cycles acyclic) :to-be nil)
          ;; Check that some edge was marked as reversed
          (let ((has-reversed (--some (ht-get (dag-draw-edge-attributes it) :reversed)
                                      (dag-draw-graph-edges acyclic))))
            (expect has-reversed :to-be t))))))

  (describe "topological sorting"
    (it "should sort acyclic graph topologically"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-node graph 'c)
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'a 'c)
        (dag-draw-add-edge graph 'b 'c)
        
        (let ((sorted (dag-draw-topological-sort graph)))
          (expect (length sorted) :to-equal 3)
          ;; 'a' should come before 'b' and 'c'
          (expect (cl-position 'a sorted) :to-be-less-than (cl-position 'b sorted))
          (expect (cl-position 'a sorted) :to-be-less-than (cl-position 'c sorted))
          ;; 'b' should come before 'c'
          (expect (cl-position 'b sorted) :to-be-less-than (cl-position 'c sorted)))))
    
    (it "should reject cyclic graphs"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'a)  ; Creates cycle
        
        (expect (dag-draw-topological-sort graph) :to-throw 'error)))
    
    (it "should handle isolated nodes"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'isolated)
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-edge graph 'a 'b)
        
        (let ((sorted (dag-draw-topological-sort graph)))
          (expect (length sorted) :to-equal 3)
          (expect sorted :to-contain 'isolated)))))

  (describe "graph transpose"
    (it "should reverse all edges"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-node graph 'c)
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)
        
        (let ((transpose (dag-draw-transpose-graph graph)))
          ;; Should have same number of nodes and edges
          (expect (dag-draw-node-count transpose) :to-equal (dag-draw-node-count graph))
          (expect (dag-draw-edge-count transpose) :to-equal (dag-draw-edge-count graph))
          
          ;; Edges should be reversed
          (expect (dag-draw-has-edge transpose 'b 'a) :to-be t)
          (expect (dag-draw-has-edge transpose 'c 'b) :to-be t)
          (expect (dag-draw-has-edge transpose 'a 'b) :to-be nil)
          (expect (dag-draw-has-edge transpose 'b 'c) :to-be nil)))))

  (describe "strongly connected components"
    (it "should find SCCs in simple graph"
      (let ((graph (dag-draw-create-graph)))
        ;; Create a graph with known SCC structure
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-node graph 'c)
        (dag-draw-add-node graph 'd)
        
        ;; SCC 1: {a, b}
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'a)
        
        ;; SCC 2: {c}
        (dag-draw-add-edge graph 'a 'c)
        
        ;; SCC 3: {d}
        (dag-draw-add-edge graph 'c 'd)
        
        (let ((sccs (dag-draw-strongly-connected-components graph)))
          (expect (length sccs) :to-equal 3)
          ;; Should find the {a, b} component
          (expect (--some (and (member 'a it) (member 'b it)) sccs) :to-be t)))))

  (describe "integration with cycle breaking workflow"
    (it "should create acyclic graph suitable for ranking"
      (let ((graph (dag-draw-create-graph)))
        ;; Create complex graph with multiple cycles
        (dag-draw-add-node graph 'root)
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-node graph 'c)
        (dag-draw-add-node graph 'sink)
        
        ;; Add edges including cycles
        (dag-draw-add-edge graph 'root 'a)
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)
        (dag-draw-add-edge graph 'c 'a)    ; Cycle 1
        (dag-draw-add-edge graph 'b 'root) ; Cycle 2
        (dag-draw-add-edge graph 'c 'sink)
        
        ;; Break cycles and verify result is suitable for ranking
        (let ((acyclic (dag-draw-break-cycles graph)))
          (expect (dag-draw-has-cycles acyclic) :to-be nil)
          
          ;; Should be able to topologically sort
          (let ((sorted (dag-draw-topological-sort acyclic)))
            (expect (length sorted) :to-equal 5)
            (expect sorted :to-contain 'root)
            (expect sorted :to-contain 'sink))))))

;;; dag-draw-algorithms-test.el ends here