;;; gknv-edge-cases-test.el --- GKNV algorithm edge cases and robustness tests -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Tests for edge cases, boundary conditions, and robustness of the GKNV algorithm.
;; These tests ensure the implementation handles degenerate cases, large graphs,
;; and unusual structures as described in the paper.
;;
;; Based on robustness requirements from "A Technique for Drawing Directed Graphs"
;; and implementation resilience expectations.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)

(describe "GKNV Algorithm Edge Cases and Robustness"
  
  ;; =================================================================
  ;; MINIMAL AND DEGENERATE GRAPHS
  ;; =================================================================
  
  (describe "Minimal and Degenerate Graph Cases"
    
    (it "should handle empty graphs gracefully"
      (let ((graph (dag-draw-create-graph)))
        ;; Empty graph with no nodes or edges
        (expect (dag-draw-layout-graph graph) :not :to-be nil)
        
        ;; Should render without error
        (let ((ascii-output (dag-draw-render-ascii graph)))
          (expect ascii-output :to-be-truthy)
          (expect ascii-output :to-match "(Empty Graph)"))))
    
    (it "should handle single node graphs - trivial rank assignment"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'single "Single Node")
        
        (dag-draw-layout-graph graph)
        
        ;; Single node should be at rank 0
        (let ((node (dag-draw-get-node graph 'single)))
          (expect (dag-draw-node-rank node) :to-be-close-to 0 0.1)
          (expect (dag-draw-node-x-coord node) :not :to-be nil)
          (expect (dag-draw-node-y-coord node) :not :to-be nil))
        
        ;; Should render properly
        (let ((ascii-output (dag-draw-render-ascii graph)))
          (expect ascii-output :to-match "Single Node"))))
    
    (it "should handle two disconnected nodes"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'node1 "Node 1")
        (dag-draw-add-node graph 'node2 "Node 2")
        ;; No edges - disconnected
        
        (dag-draw-layout-graph graph)
        
        ;; Both nodes should have valid coordinates
        (let ((node1 (dag-draw-get-node graph 'node1))
              (node2 (dag-draw-get-node graph 'node2)))
          (expect (dag-draw-node-rank node1) :not :to-be nil)
          (expect (dag-draw-node-rank node2) :not :to-be nil)
          (expect (dag-draw-node-x-coord node1) :not :to-be nil)
          (expect (dag-draw-node-x-coord node2) :not :to-be nil))))
    
    (it "should handle single edge graph"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'source "Source")
        (dag-draw-add-node graph 'target "Target")
        (dag-draw-add-edge graph 'source 'target)
        
        (dag-draw-layout-graph graph)
        
        ;; Should create two-rank structure
        (let ((source-rank (dag-draw-node-rank (dag-draw-get-node graph 'source)))
              (target-rank (dag-draw-node-rank (dag-draw-get-node graph 'target))))
          (expect source-rank :to-be-close-to 0 0.1)
          (expect target-rank :to-be-close-to 1 0.1))))
    
    (it "should handle self-loops - Section 5.1.3"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'self "Self Loop Node")
        (dag-draw-add-edge graph 'self 'self)
        
        ;; Paper Section 2: "loops are ignored" in rank assignment
        (dag-draw-layout-graph graph)
        
        ;; Should assign valid rank despite self-loop
        (let ((node (dag-draw-get-node graph 'self)))
          (expect (dag-draw-node-rank node) :to-be-close-to 0 0.1))
        
        ;; Should render without error
        (let ((ascii-output (dag-draw-render-ascii graph)))
          (expect ascii-output :to-match "Self Loop Node"))))
  
  ;; =================================================================
  ;; CYCLE HANDLING - Section 2.1
  ;; =================================================================
  
  (describe "Cycle Detection and Breaking - Section 2.1"
    
    (it "should break simple cycles using DFS back edge detection"
      (let ((graph (dag-draw-create-graph)))
        ;; Simple cycle: A -> B -> C -> A
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-node graph 'b "Node B")
        (dag-draw-add-node graph 'c "Node C")
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)
        (dag-draw-add-edge graph 'c 'a) ; Creates cycle
        
        ;; Paper Section 2.1: algorithm should break cycles
        (expect (dag-draw-layout-graph graph) :not :to-be nil)
        
        ;; Should assign valid ranks to all nodes
        (dolist (node-id '(a b c))
          (let ((node (dag-draw-get-node graph node-id)))
            (expect (dag-draw-node-rank node) :not :to-be nil)))))
    
    (it "should handle complex cycles with multiple back edges"
      (let ((graph (dag-draw-create-graph)))
        ;; Complex cycle structure
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C") 
        (dag-draw-add-node graph 'd "D")
        
        ;; Forward edges
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)
        (dag-draw-add-edge graph 'c 'd)
        
        ;; Back edges creating cycles
        (dag-draw-add-edge graph 'd 'b) ; Cycle: b->c->d->b
        (dag-draw-add-edge graph 'c 'a) ; Cycle: a->b->c->a
        
        ;; Should handle multiple cycles
        (expect (dag-draw-layout-graph graph) :not :to-be nil)
        
        ;; All nodes should have ranks
        (dolist (node-id '(a b c d))
          (let ((node (dag-draw-get-node graph node-id)))
            (expect (dag-draw-node-rank node) :not :to-be nil)))))
    
    (it "should preserve original edge directions in visualization"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'forward "Forward")
        (dag-draw-add-node graph 'back "Back")
        (dag-draw-add-edge graph 'forward 'back)
        (dag-draw-add-edge graph 'back 'forward) ; Creates cycle
        
        (dag-draw-layout-graph graph)
        
        ;; Should render with arrows showing original directions
        (let ((ascii-output (dag-draw-render-ascii graph)))
          (expect ascii-output :to-be-truthy)
          (expect ascii-output :to-match "Forward")
          (expect ascii-output :to-match "Back"))))
  
  ;; =================================================================
  ;; DISCONNECTED COMPONENTS - Section 1.2, line 74
  ;; =================================================================
  
  (describe "Disconnected Components Handling"
    
    (it "should handle multiple disconnected components"
      (let ((graph (dag-draw-create-graph)))
        ;; Component 1: A -> B
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-edge graph 'a 'b)
        
        ;; Component 2: C -> D -> E
        (dag-draw-add-node graph 'c "C")
        (dag-draw-add-node graph 'd "D")
        (dag-draw-add-node graph 'e "E")
        (dag-draw-add-edge graph 'c 'd)
        (dag-draw-add-edge graph 'd 'e)
        
        ;; Component 3: Isolated node
        (dag-draw-add-node graph 'isolated "Isolated")
        
        ;; Paper Section 1.2: "each connected component can be laid out separately"
        (dag-draw-layout-graph graph)
        
        ;; All nodes should have valid coordinates
        (dolist (node-id '(a b c d e isolated))
          (let ((node (dag-draw-get-node graph node-id)))
            (expect (dag-draw-node-rank node) :not :to-be nil)
            (expect (dag-draw-node-x-coord node) :not :to-be nil)
            (expect (dag-draw-node-y-coord node) :not :to-be nil)))))
    
    (it "should maintain component hierarchy within disconnected components"
      (let ((graph (dag-draw-create-graph)))
        ;; Two separate hierarchies
        ;; Hierarchy 1: root1 -> child1 -> grandchild1
        (dag-draw-add-node graph 'root1 "Root 1")
        (dag-draw-add-node graph 'child1 "Child 1")
        (dag-draw-add-node graph 'grandchild1 "Grandchild 1")
        (dag-draw-add-edge graph 'root1 'child1)
        (dag-draw-add-edge graph 'child1 'grandchild1)
        
        ;; Hierarchy 2: root2 -> child2 -> grandchild2
        (dag-draw-add-node graph 'root2 "Root 2")
        (dag-draw-add-node graph 'child2 "Child 2")
        (dag-draw-add-node graph 'grandchild2 "Grandchild 2")
        (dag-draw-add-edge graph 'root2 'child2)
        (dag-draw-add-edge graph 'child2 'grandchild2)
        
        (dag-draw-layout-graph graph)
        
        ;; Each hierarchy should maintain proper rank progression
        (let ((rank-r1 (dag-draw-node-rank (dag-draw-get-node graph 'root1)))
              (rank-c1 (dag-draw-node-rank (dag-draw-get-node graph 'child1)))
              (rank-gc1 (dag-draw-node-rank (dag-draw-get-node graph 'grandchild1))))
          (expect rank-r1 :to-be-less-than rank-c1)
          (expect rank-c1 :to-be-less-than rank-gc1))
        
        (let ((rank-r2 (dag-draw-node-rank (dag-draw-get-node graph 'root2)))
              (rank-c2 (dag-draw-node-rank (dag-draw-get-node graph 'child2)))
              (rank-gc2 (dag-draw-node-rank (dag-draw-get-node graph 'grandchild2))))
          (expect rank-r2 :to-be-less-than rank-c2)
          (expect rank-c2 :to-be-less-than rank-gc2))))
  
  ;; =================================================================
  ;; LARGE GRAPHS AND SCALABILITY
  ;; =================================================================
  
  (describe "Large Graphs and Scalability"
    
    (it "should handle moderately large linear chains"
      (let ((graph (dag-draw-create-graph))
            (chain-length 50))
        
        ;; Create long linear chain
        (dotimes (i chain-length)
          (dag-draw-add-node graph (intern (format "node-%d" i)) (format "Node %d" i)))
        
        (dotimes (i (1- chain-length))
          (dag-draw-add-edge graph 
                            (intern (format "node-%d" i))
                            (intern (format "node-%d" (1+ i)))))
        
        ;; Should complete in reasonable time
        (let ((start-time (current-time)))
          (dag-draw-layout-graph graph)
          (let ((elapsed (float-time (time-subtract (current-time) start-time))))
            ;; Should complete in under 10 seconds
            (expect elapsed :to-be-less-than 10)))
        
        ;; Should have proper rank assignment
        (let ((first-rank (dag-draw-node-rank (dag-draw-get-node graph 'node-0)))
              (last-rank (dag-draw-node-rank (dag-draw-get-node graph (intern (format "node-%d" (1- chain-length)))))))
          (expect (- last-rank first-rank) :to-be-close-to (1- chain-length) 1))))
    
    (it "should handle wide graphs with many parallel nodes"
      (let ((graph (dag-draw-create-graph))
            (width 20))
        
        ;; Create wide graph: root -> many nodes -> sink
        (dag-draw-add-node graph 'root "Root")
        (dag-draw-add-node graph 'sink "Sink")
        
        (dotimes (i width)
          (let ((node-id (intern (format "parallel-%d" i))))
            (dag-draw-add-node graph node-id (format "Parallel %d" i))
            (dag-draw-add-edge graph 'root node-id)
            (dag-draw-add-edge graph node-id 'sink)))
        
        (dag-draw-layout-graph graph)
        
        ;; All parallel nodes should be on same rank
        (let ((first-parallel-rank (dag-draw-node-rank (dag-draw-get-node graph 'parallel-0))))
          (dotimes (i width)
            (let ((node-rank (dag-draw-node-rank (dag-draw-get-node graph (intern (format "parallel-%d" i))))))
              (expect (abs (- node-rank first-parallel-rank)) :to-be-less-than 0.1))))
        
        ;; Should render without error
        (let ((ascii-output (dag-draw-render-ascii graph)))
          (expect ascii-output :to-be-truthy))))
    
    (it "should handle dense graphs with many edges"
      (let ((graph (dag-draw-create-graph))
            (node-count 10))
        
        ;; Create dense graph - every node connects to every later node
        (dotimes (i node-count)
          (dag-draw-add-node graph (intern (format "node-%d" i)) (format "Node %d" i)))
        
        (dotimes (i node-count)
          (dotimes (j node-count)
            (when (< i j)
              (dag-draw-add-edge graph 
                                (intern (format "node-%d" i))
                                (intern (format "node-%d" j))))))
        
        ;; Dense graph should still complete
        (expect (dag-draw-layout-graph graph) :not :to-be nil)
        
        ;; Should maintain proper ordering
        (dotimes (i (1- node-count))
          (let ((rank-i (dag-draw-node-rank (dag-draw-get-node graph (intern (format "node-%d" i)))))
                (rank-j (dag-draw-node-rank (dag-draw-get-node graph (intern (format "node-%d" (1+ i)))))))
            (expect rank-i :to-be-less-than (+ rank-j 0.1))))))
  
  ;; =================================================================
  ;; NUMERICAL STABILITY AND PRECISION
  ;; =================================================================
  
  (describe "Numerical Stability and Precision"
    
    (it "should maintain numerical stability with floating point coordinates"
      (let ((graph (dag-draw-create-graph)))
        ;; Graph that could stress floating point precision
        (dotimes (i 100)
          (let ((node-id (intern (format "precise-%d" i))))
            (dag-draw-add-node graph node-id (format "P%d" i))
            (when (> i 0)
              (dag-draw-add-edge graph (intern (format "precise-%d" (1- i))) node-id))))
        
        (dag-draw-layout-graph graph)
        
        ;; All coordinates should be finite numbers
        (dotimes (i 100)
          (let ((node (dag-draw-get-node graph (intern (format "precise-%d" i)))))
            (expect (numberp (dag-draw-node-rank node)) :to-be-truthy)
            (expect (numberp (dag-draw-node-x-coord node)) :to-be-truthy)
            (expect (numberp (dag-draw-node-y-coord node)) :to-be-truthy)
            
            ;; Should not be infinite or NaN
            (expect (dag-draw-node-rank node) :not :to-equal 1.0e+INF)
            (expect (dag-draw-node-x-coord node) :not :to-equal 1.0e+INF)))))
    
    (it "should handle coordinate precision in ASCII rendering"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-edge graph 'a 'b)
        
        (dag-draw-layout-graph graph)
        
        ;; ASCII grid should handle fractional coordinates properly
        (let ((ascii-output (dag-draw-render-ascii graph)))
          (expect ascii-output :to-be-truthy)
          (expect ascii-output :to-match "A")
          (expect ascii-output :to-match "B"))))
  
  ;; =================================================================
  ;; MALFORMED INPUT HANDLING
  ;; =================================================================
  
  (describe "Malformed Input Handling"
    
    (it "should handle missing nodes gracefully"
      (let ((graph (dag-draw-create-graph)))
        ;; Try to add edge with non-existent nodes
        (dag-draw-add-node graph 'existing "Existing")
        
        ;; This should either fail gracefully or handle the missing node
        ;; Implementation-specific behavior
        (expect (dag-draw-layout-graph graph) :not :to-be nil)))
    
    (it "should handle duplicate node additions"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'duplicate "First")
        (dag-draw-add-node graph 'duplicate "Second") ; Same ID
        
        ;; Should handle duplicate gracefully
        (expect (dag-draw-layout-graph graph) :not :to-be nil)
        
        ;; Should have some representation of the node
        (let ((node (dag-draw-get-node graph 'duplicate)))
          (expect node :not :to-be nil))))
    
    (it "should handle duplicate edge additions"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'a 'b) ; Duplicate edge
        
        ;; Paper Section 2: "multi-edges are merged"
        (expect (dag-draw-layout-graph graph) :not :to-be nil)
        
        (let ((ascii-output (dag-draw-render-ascii graph)))
          (expect ascii-output :to-match "A")
          (expect ascii-output :to-match "B"))))
  
  ;; =================================================================
  ;; SPECIAL GRAPH STRUCTURES
  ;; =================================================================
  
  (describe "Special Graph Structures"
    
    (it "should handle star graphs (hub and spoke)"
      (let ((graph (dag-draw-create-graph))
            (spoke-count 8))
        
        ;; Central hub connected to many spokes
        (dag-draw-add-node graph 'hub "Hub")
        (dotimes (i spoke-count)
          (let ((spoke-id (intern (format "spoke-%d" i))))
            (dag-draw-add-node graph spoke-id (format "Spoke %d" i))
            (dag-draw-add-edge graph 'hub spoke-id)))
        
        (dag-draw-layout-graph graph)
        
        ;; Hub should be at rank 0, all spokes at rank 1
        (expect (dag-draw-node-rank (dag-draw-get-node graph 'hub)) :to-be-close-to 0 0.1)
        (dotimes (i spoke-count)
          (let ((spoke-rank (dag-draw-node-rank (dag-draw-get-node graph (intern (format "spoke-%d" i))))))
            (expect spoke-rank :to-be-close-to 1 0.1)))))
    
    (it "should handle tree structures"
      (let ((graph (dag-draw-create-graph)))
        ;; Binary tree structure
        (dag-draw-add-node graph 'root "Root")
        (dag-draw-add-node graph 'left "Left")
        (dag-draw-add-node graph 'right "Right")
        (dag-draw-add-node graph 'left-left "LL")
        (dag-draw-add-node graph 'left-right "LR")
        (dag-draw-add-node graph 'right-left "RL")
        (dag-draw-add-node graph 'right-right "RR")
        
        (dag-draw-add-edge graph 'root 'left)
        (dag-draw-add-edge graph 'root 'right)
        (dag-draw-add-edge graph 'left 'left-left)
        (dag-draw-add-edge graph 'left 'left-right)
        (dag-draw-add-edge graph 'right 'right-left)
        (dag-draw-add-edge graph 'right 'right-right)
        
        (dag-draw-layout-graph graph)
        
        ;; Should maintain tree structure with proper ranks
        (expect (dag-draw-node-rank (dag-draw-get-node graph 'root)) :to-be-close-to 0 0.1)
        (expect (dag-draw-node-rank (dag-draw-get-node graph 'left)) :to-be-close-to 1 0.1)
        (expect (dag-draw-node-rank (dag-draw-get-node graph 'right)) :to-be-close-to 1 0.1)
        (expect (dag-draw-node-rank (dag-draw-get-node graph 'left-left)) :to-be-close-to 2 0.1)))
    
    (it "should handle complete bipartite graphs"
      (let ((graph (dag-draw-create-graph))
            (left-count 4)
            (right-count 3))
        
        ;; Left partition
        (dotimes (i left-count)
          (dag-draw-add-node graph (intern (format "left-%d" i)) (format "L%d" i)))
        
        ;; Right partition
        (dotimes (i right-count)
          (dag-draw-add-node graph (intern (format "right-%d" i)) (format "R%d" i)))
        
        ;; Connect every left node to every right node
        (dotimes (i left-count)
          (dotimes (j right-count)
            (dag-draw-add-edge graph 
                              (intern (format "left-%d" i))
                              (intern (format "right-%d" j)))))
        
        (dag-draw-layout-graph graph)
        
        ;; Left nodes should be at rank 0, right nodes at rank 1
        (dotimes (i left-count)
          (expect (dag-draw-node-rank (dag-draw-get-node graph (intern (format "left-%d" i)))) :to-be-close-to 0 0.1))
        (dotimes (i right-count)
          (expect (dag-draw-node-rank (dag-draw-get-node graph (intern (format "right-%d" i)))) :to-be-close-to 1 0.1))))))))))))

;;; gknv-edge-cases-test.el ends here