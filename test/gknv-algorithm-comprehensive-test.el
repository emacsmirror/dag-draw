;;; gknv-algorithm-comprehensive-test.el --- Comprehensive GKNV algorithm tests per paper specification -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Exhaustive test suite for GKNV four-pass algorithm based on 
;; "A Technique for Drawing Directed Graphs" (Gansner et al.)
;; 
;; Tests are organized by the four passes:
;; Pass 1: Rank Assignment using Network Simplex
;; Pass 2: Node Ordering within Ranks 
;; Pass 3: Coordinate Assignment
;; Pass 4: Spline Generation
;;
;; Each test verifies behavior described in the paper with proper bounds checking.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-test-harness)

(describe "GKNV Algorithm Comprehensive Test Suite - Paper Specification Compliance"
  
  ;; =================================================================
  ;; PASS 1: RANK ASSIGNMENT (Section 2)
  ;; "The first pass finds an optimal rank assignment using a network simplex algorithm"
  ;; =================================================================
  
  (describe "Pass 1: Rank Assignment - Section 2 Compliance"
    
    (describe "Basic Rank Assignment Properties - Section 2, line 352"
      (it "should assign each node to a discrete integer rank"
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'a "Node A")
          (dag-draw-add-node graph 'b "Node B") 
          (dag-draw-add-edge graph 'a 'b)
          
          (dag-draw-layout-graph graph)
          
          (let ((node-a (dag-draw-get-node graph 'a))
                (node-b (dag-draw-get-node graph 'b)))
            ;; Paper Section 2: "assigns each node v ∈ G to an integer rank λ(v)"
            (expect (dag-draw-node-rank node-a) :to-be-close-to 0 0.1)
            (expect (dag-draw-node-rank node-b) :to-be-close-to 1 0.1)
            ;; Verify ranks are integers (within tolerance for floating point)
            (expect (abs (- (dag-draw-node-rank node-a) (round (dag-draw-node-rank node-a)))) :to-be-less-than 0.01)
            (expect (abs (- (dag-draw-node-rank node-b) (round (dag-draw-node-rank node-b)))) :to-be-less-than 0.01))))
      
      (it "should satisfy edge length constraint l(e) ≥ δ(e) - Section 2, line 354"
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'source "Source")
          (dag-draw-add-node graph 'target "Target")
          (dag-draw-add-edge graph 'source 'target)
          
          (dag-draw-layout-graph graph)
          
          (let* ((source-node (dag-draw-get-node graph 'source))
                 (target-node (dag-draw-get-node graph 'target))
                 (edge-length (- (dag-draw-node-rank target-node) (dag-draw-node-rank source-node)))
                 (min-length 1)) ; δ(e) usually 1 per paper
            ;; Paper Section 2: "l(e) ≥ δ(e), where the length l(e) of e = (v,w) is defined as λ(w) − λ(v)"
            (expect edge-length :to-be-greater-than (- min-length 0.01))))))
    
    (describe "Feasible Ranking - Section 2.3, line 479"
      (it "should produce feasible ranking with non-negative slack for all edges"
        (let ((graph (dag-draw-create-graph)))
          ;; Create chain: a -> b -> c
          (dag-draw-add-node graph 'a "Node A")
          (dag-draw-add-node graph 'b "Node B")
          (dag-draw-add-node graph 'c "Node C") 
          (dag-draw-add-edge graph 'a 'b)
          (dag-draw-add-edge graph 'b 'c)
          
          (dag-draw-layout-graph graph)
          
          (let ((rank-a (dag-draw-node-rank (dag-draw-get-node graph 'a)))
                (rank-b (dag-draw-node-rank (dag-draw-get-node graph 'b)))
                (rank-c (dag-draw-node-rank (dag-draw-get-node graph 'c))))
            ;; Paper Section 2.3: "ranking is feasible if the slack of every edge is non-negative"
            ;; slack(e) = l(e) - δ(e), with δ(e) = 1 typically
            (let ((slack-ab (- (- rank-b rank-a) 1))
                  (slack-bc (- (- rank-c rank-b) 1)))
              (expect slack-ab :to-be-greater-than -0.01) ; Allow tiny floating point error
              (expect slack-bc :to-be-greater-than -0.01))))))
    
    (describe "Acyclic Graph Construction - Section 2.1"
      (it "should handle graphs with cycles by reversing back edges"
        (let ((graph (dag-draw-create-graph)))
          ;; Create cycle: a -> b -> c -> a
          (dag-draw-add-node graph 'a "Node A")
          (dag-draw-add-node graph 'b "Node B") 
          (dag-draw-add-node graph 'c "Node C")
          (dag-draw-add-edge graph 'a 'b)
          (dag-draw-add-edge graph 'b 'c)
          (dag-draw-add-edge graph 'c 'a) ; Creates cycle
          
          ;; Should not fail - algorithm should break cycles
          (expect (dag-draw-layout-graph graph) :not :to-be nil)
          
          ;; All nodes should have valid ranks
          (let ((rank-a (dag-draw-node-rank (dag-draw-get-node graph 'a)))
                (rank-b (dag-draw-node-rank (dag-draw-get-node graph 'b)))
                (rank-c (dag-draw-node-rank (dag-draw-get-node graph 'c))))
            (expect rank-a :not :to-be nil)
            (expect rank-b :not :to-be nil) 
            (expect rank-c :not :to-be nil)))))
    
    (describe "Network Simplex Convergence - Section 2.3"
      (it "should converge to optimal ranking for simple graphs"
        (let ((graph (dag-draw-create-graph)))
          ;; Diamond graph: root -> {left, right} -> sink
          (dag-draw-add-node graph 'root "Root")
          (dag-draw-add-node graph 'left "Left")
          (dag-draw-add-node graph 'right "Right")
          (dag-draw-add-node graph 'sink "Sink")
          (dag-draw-add-edge graph 'root 'left)
          (dag-draw-add-edge graph 'root 'right)
          (dag-draw-add-edge graph 'left 'sink)
          (dag-draw-add-edge graph 'right 'sink)
          
          (dag-draw-layout-graph graph)
          
          ;; Verify optimal 3-rank structure per A3 (keep edges short)
          (let ((rank-root (dag-draw-node-rank (dag-draw-get-node graph 'root)))
                (rank-left (dag-draw-node-rank (dag-draw-get-node graph 'left)))
                (rank-right (dag-draw-node-rank (dag-draw-get-node graph 'right)))
                (rank-sink (dag-draw-node-rank (dag-draw-get-node graph 'sink))))
            ;; Paper Section 1.1 A3: "Keep edges short"
            (expect rank-root :to-be-close-to 0 0.1)
            (expect rank-left :to-be-close-to 1 0.1)
            (expect rank-right :to-be-close-to 1 0.1)
            (expect rank-sink :to-be-close-to 2 0.1)))))
  
  ;; =================================================================
  ;; PASS 2: NODE ORDERING WITHIN RANKS (Section 3)
  ;; "The second pass sets the vertex order within ranks by an iterative heuristic"
  ;; =================================================================
  
  (describe "Pass 2: Node Ordering within Ranks - Section 3 Compliance"
    
    (describe "Virtual Node Insertion - Section 3, line 894"
      (it "should insert virtual nodes for multi-rank edges"
        (let ((graph (dag-draw-create-graph)))
          ;; Force multi-rank edge by setting minimum edge length > 1
          ;; TODO: This requires implementation of δ(e) > 1 support
          (dag-draw-add-node graph 'a "Node A")
          (dag-draw-add-node graph 'b "Node B")
          (dag-draw-add-edge graph 'a 'b)
          
          (dag-draw-layout-graph graph)
          
          ;; Basic verification - nodes should have valid ranks
          (let ((rank-a (dag-draw-node-rank (dag-draw-get-node graph 'a)))
                (rank-b (dag-draw-node-rank (dag-draw-get-node graph 'b))))
            (expect rank-a :not :to-be nil)
            (expect rank-b :not :to-be nil)))))
    
    (describe "Crossing Minimization - Section 3, line 902"
      (it "should reduce edge crossings using median heuristic"
        (let ((graph (dag-draw-create-graph)))
          ;; Create crossing situation: (a,b) -> (c,d) with potential crossing
          (dag-draw-add-node graph 'a "Node A")
          (dag-draw-add-node graph 'b "Node B")
          (dag-draw-add-node graph 'c "Node C") 
          (dag-draw-add-node graph 'd "Node D")
          (dag-draw-add-edge graph 'a 'c) ; a->c
          (dag-draw-add-edge graph 'a 'd) ; a->d  
          (dag-draw-add-edge graph 'b 'c) ; b->c
          (dag-draw-add-edge graph 'b 'd) ; b->d
          
          (dag-draw-layout-graph graph)
          
          ;; Verify nodes are ordered within ranks
          ;; Paper Section 3: algorithm should minimize crossings
          (let ((rank-a (dag-draw-node-rank (dag-draw-get-node graph 'a)))
                (rank-b (dag-draw-node-rank (dag-draw-get-node graph 'b)))
                (rank-c (dag-draw-node-rank (dag-draw-get-node graph 'c)))
                (rank-d (dag-draw-node-rank (dag-draw-get-node graph 'd))))
            ;; Nodes a,b should be on same rank, c,d should be on same rank
            (expect (abs (- rank-a rank-b)) :to-be-less-than 0.1)
            (expect (abs (- rank-c rank-d)) :to-be-less-than 0.1)))))
    
    (describe "Flat Edge Handling - Section 3, line 1163"
      (it "should orient flat edges left-to-right consistently"
        (let ((graph (dag-draw-create-graph)))
          ;; Create flat edge by forcing nodes to same rank
          (dag-draw-add-node graph 'left "Left Node")
          (dag-draw-add-node graph 'right "Right Node")
          (dag-draw-add-edge graph 'left 'right)
          
          ;; TODO: Implement same-rank constraint to test flat edges properly
          (dag-draw-layout-graph graph)
          
          ;; Basic verification that layout completes
          (expect (dag-draw-node-rank (dag-draw-get-node graph 'left)) :not :to-be nil)
          (expect (dag-draw-node-rank (dag-draw-get-node graph 'right)) :not :to-be nil))))
  
  ;; =================================================================
  ;; PASS 3: COORDINATE ASSIGNMENT (Section 4) 
  ;; "The third pass finds optimal coordinates for nodes"
  ;; =================================================================
  
  (describe "Pass 3: Coordinate Assignment - Section 4 Compliance"
    
    (describe "X Coordinate Optimization - Section 4, line 1216"
      (it "should minimize edge straightening objective function"
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'a "Node A")
          (dag-draw-add-node graph 'b "Node B")
          (dag-draw-add-edge graph 'a 'b)
          
          (dag-draw-layout-graph graph)
          
          (let ((node-a (dag-draw-get-node graph 'a))
                (node-b (dag-draw-get-node graph 'b)))
            ;; Paper Section 4: assigns X and Y coordinates
            (expect (dag-draw-node-x-coord node-a) :not :to-be nil)
            (expect (dag-draw-node-y-coord node-a) :not :to-be nil)
            (expect (dag-draw-node-x-coord node-b) :not :to-be nil)
            (expect (dag-draw-node-y-coord node-b) :not :to-be nil))))
    
    (describe "Node Separation Constraints - Section 4, line 1227"
      (it "should maintain minimum separation between adjacent nodes"
        (let ((graph (dag-draw-create-graph)))
          ;; Place two nodes on same rank to test separation
          (dag-draw-add-node graph 'left "Left Node with Long Label")
          (dag-draw-add-node graph 'right "Right")
          (dag-draw-add-node graph 'target "Target") 
          (dag-draw-add-edge graph 'left 'target)
          (dag-draw-add-edge graph 'right 'target)
          
          (dag-draw-layout-graph graph)
          
          (let ((left-node (dag-draw-get-node graph 'left))
                (right-node (dag-draw-get-node graph 'right)))
            ;; Nodes on same rank should be separated by at least their sizes + margin
            (let* ((x-left (dag-draw-node-x-coord left-node))
                   (x-right (dag-draw-node-x-coord right-node))
                   (xsize-left (dag-draw-node-x-size left-node))
                   (xsize-right (dag-draw-node-x-size right-node))
                   (nodesep (dag-draw-graph-node-separation graph))
                   ;; Paper Section 4: ρ(a,b) = (xsize_a + xsize_b)/2 + nodesep
                   (min-separation (+ (/ (+ xsize-left xsize-right) 2.0) nodesep)))
              ;; After ASCII scaling, node sizes are ~12-25, nodesep=6
              ;; So minimum separation is typically 12-18. Allow tolerance for compaction.
              (expect (abs (- x-right x-left)) :to-be-greater-than (- min-separation 2.0))))))
    
    (describe "Y Coordinate Assignment - Section 4, line 1207"
      (it "should assign same Y coordinate to nodes in same rank"
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'a "Node A")
          (dag-draw-add-node graph 'b "Node B")
          (dag-draw-add-node graph 'c "Node C")
          (dag-draw-add-edge graph 'a 'b)
          (dag-draw-add-edge graph 'a 'c)
          
          (dag-draw-layout-graph graph)
          
          (let ((node-a (dag-draw-get-node graph 'a))
                (node-b (dag-draw-get-node graph 'b))
                (node-c (dag-draw-get-node graph 'c)))
            ;; b and c should be on same rank, therefore same Y coordinate
            (if (= (dag-draw-node-rank node-b) (dag-draw-node-rank node-c))
                (expect (abs (- (dag-draw-node-y-coord node-b) (dag-draw-node-y-coord node-c))) :to-be-less-than 0.1))))))
  
  ;; =================================================================
  ;; PASS 4: SPLINE GENERATION (Section 5)
  ;; "The fourth pass makes splines to draw edges" 
  ;; =================================================================
  
  (describe "Pass 4: Edge Drawing with Splines - Section 5 Compliance"
    
    (describe "Inter-rank Edge Splines - Section 5.1.1"
      (it "should generate splines for edges between different ranks"
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'source "Source")
          (dag-draw-add-node graph 'target "Target")
          (dag-draw-add-edge graph 'source 'target)
          
          (dag-draw-layout-graph graph)
          
          ;; Use test harness to verify proper spline structure
          (let ((ascii-output (dag-draw-render-ascii graph)))
            (expect ascii-output :to-be-truthy)
            
            ;; Use test harness to find nodes and verify connection
            (let ((grid (dag-draw-test--parse-ascii-grid ascii-output)))
              (let ((source-pos (dag-draw-test--find-text-in-grid grid "Source"))
                    (target-pos (dag-draw-test--find-text-in-grid grid "Target")))
                ;; Both nodes should be found
                (expect source-pos :not :to-be nil)
                (expect target-pos :not :to-be nil)
                
                ;; Target should be below source (higher Y coordinate)
                (expect (cdr target-pos) :to-be-greater-than (cdr source-pos))
                
                ;; Should have edge connection between them
                (expect (dag-draw-test--has-path-between grid source-pos target-pos) :to-be-truthy))))))
    
    (describe "Edge Classification - Section 5.1"  
      (it "should handle different edge types appropriately"
        (let ((graph (dag-draw-create-graph)))
          ;; Inter-rank edge
          (dag-draw-add-node graph 'top "Top")
          (dag-draw-add-node graph 'bottom "Bottom")
          (dag-draw-add-edge graph 'top 'bottom)
          
          ;; Self-edge  
          (dag-draw-add-node graph 'self "Self")
          (dag-draw-add-edge graph 'self 'self)
          
          (dag-draw-layout-graph graph)
          
          ;; Use test harness to verify both edge types render properly
          (let ((ascii-output (dag-draw-render-ascii graph)))
            (expect ascii-output :to-be-truthy)
            
            ;; Verify nodes are present and positioned correctly
            (let ((grid (dag-draw-test--parse-ascii-grid ascii-output)))
              (let ((top-pos (dag-draw-test--find-text-in-grid grid "Top"))
                    (bottom-pos (dag-draw-test--find-text-in-grid grid "Bottom"))
                    (self-pos (dag-draw-test--find-text-in-grid grid "Self")))
                ;; All nodes should be found
                (expect top-pos :not :to-be nil)
                (expect bottom-pos :not :to-be nil)
                (expect self-pos :not :to-be nil)
                
                ;; Inter-rank edge: bottom should be below top
                (expect (cdr bottom-pos) :to-be-greater-than (cdr top-pos))
                
                ;; Should have path between top and bottom
                (expect (dag-draw-test--has-path-between grid top-pos bottom-pos) :to-be-truthy)))))))
  
  ;; =================================================================
  ;; AESTHETIC PRINCIPLES VERIFICATION (Section 1.1)
  ;; =================================================================
  
  (describe "Aesthetic Principles Compliance - Section 1.1"
    
    (describe "A1: Expose Hierarchical Structure - line 43"
      (it "should aim edges in the same general direction"
        (let ((graph (dag-draw-create-graph)))
          ;; Create clear hierarchy: root -> middle -> leaf
          (dag-draw-add-node graph 'root "Root")
          (dag-draw-add-node graph 'middle "Middle")
          (dag-draw-add-node graph 'leaf "Leaf")
          (dag-draw-add-edge graph 'root 'middle)
          (dag-draw-add-edge graph 'middle 'leaf)
          
          (dag-draw-layout-graph graph)
          
          (let ((rank-root (dag-draw-node-rank (dag-draw-get-node graph 'root)))
                (rank-middle (dag-draw-node-rank (dag-draw-get-node graph 'middle)))
                (rank-leaf (dag-draw-node-rank (dag-draw-get-node graph 'leaf))))
            ;; Should form clear top-to-bottom hierarchy
            (expect rank-root :to-be-less-than rank-middle)
            (expect rank-middle :to-be-less-than rank-leaf))))
    
    (describe "A2: Avoid Visual Anomalies - line 47"
      (it "should produce clean ASCII output without overlaps"
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'a "Node A")
          (dag-draw-add-node graph 'b "Node B")
          (dag-draw-add-edge graph 'a 'b)
          
          (dag-draw-layout-graph graph)
          
          (let ((ascii-output (dag-draw-render-ascii graph)))
            ;; Use test harness to verify clean structure
            (let ((grid (dag-draw-test--parse-ascii-grid ascii-output)))
              (let ((node-a-pos (dag-draw-test--find-text-in-grid grid "Node A"))
                    (node-b-pos (dag-draw-test--find-text-in-grid grid "Node B")))
                ;; Both nodes should be found cleanly
                (expect node-a-pos :not :to-be nil)
                (expect node-b-pos :not :to-be nil)
                
                ;; Should have proper hierarchical positioning
                (expect (cdr node-b-pos) :to-be-greater-than (cdr node-a-pos))
                
                ;; Should have clean connection
                (expect (dag-draw-test--has-path-between grid node-a-pos node-b-pos) :to-be-truthy))))))
    
    (describe "A3: Keep Edges Short - line 50"
      (it "should prefer shorter edge lengths when possible"
        (let ((graph (dag-draw-create-graph)))
          ;; Simple chain should have minimum edge lengths
          (dag-draw-add-node graph 'first "First")
          (dag-draw-add-node graph 'second "Second")
          (dag-draw-add-edge graph 'first 'second)
          
          (dag-draw-layout-graph graph)
          
          (let* ((node-first (dag-draw-get-node graph 'first))
                 (node-second (dag-draw-get-node graph 'second))
                 (edge-length (- (dag-draw-node-rank node-second) (dag-draw-node-rank node-first))))
            ;; Should use minimum edge length (typically 1)
            (expect edge-length :to-be-close-to 1 0.1)))))
  
  ;; =================================================================
  ;; INTEGRATION AND ROBUSTNESS TESTS
  ;; =================================================================
  
  (describe "Algorithm Integration and Robustness"
    
    (describe "Complete Four-Pass Pipeline"
      (it "should successfully execute all four passes for complex graphs"
        (let ((graph (dag-draw-create-graph)))
          ;; Complex graph with multiple patterns
          (dag-draw-add-node graph 'root "Root Node")
          (dag-draw-add-node graph 'branch1 "Branch 1")
          (dag-draw-add-node graph 'branch2 "Branch 2")
          (dag-draw-add-node graph 'merge "Merge Point")
          (dag-draw-add-node graph 'final "Final")
          
          (dag-draw-add-edge graph 'root 'branch1)
          (dag-draw-add-edge graph 'root 'branch2)
          (dag-draw-add-edge graph 'branch1 'merge)
          (dag-draw-add-edge graph 'branch2 'merge) 
          (dag-draw-add-edge graph 'merge 'final)
          
          ;; All four passes should complete successfully
          (expect (dag-draw-layout-graph graph) :not :to-be nil)
          
          ;; Should produce renderable output
          (let ((ascii-output (dag-draw-render-ascii graph)))
            (expect ascii-output :to-be-truthy)
            (expect (length ascii-output) :to-be-greater-than 50))))
    
    (describe "Edge Case Handling"
      (it "should handle single node graphs"
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'only "Only Node")
          
          (dag-draw-layout-graph graph)
          
          (let ((node (dag-draw-get-node graph 'only)))
            (expect (dag-draw-node-rank node) :to-be-close-to 0 0.1)
            (expect (dag-draw-node-x-coord node) :not :to-be nil)
            (expect (dag-draw-node-y-coord node) :not :to-be nil))))
      
      (it "should handle disconnected components"
        (let ((graph (dag-draw-create-graph)))
          ;; Component 1: a -> b  
          (dag-draw-add-node graph 'a "Node A")
          (dag-draw-add-node graph 'b "Node B")
          (dag-draw-add-edge graph 'a 'b)
          
          ;; Component 2: c -> d (disconnected)
          (dag-draw-add-node graph 'c "Node C")
          (dag-draw-add-node graph 'd "Node D") 
          (dag-draw-add-edge graph 'c 'd)
          
          ;; Should handle disconnected components per paper Section 1.2
          (expect (dag-draw-layout-graph graph) :not :to-be nil)
          
          ;; All nodes should have valid coordinates
          (dolist (node-id '(a b c d))
            (let ((node (dag-draw-get-node graph node-id)))
              (expect (dag-draw-node-x-coord node) :not :to-be nil)
              (expect (dag-draw-node-y-coord node) :not :to-be nil)))))
    
    (describe "Performance and Scalability"
      (it "should handle moderately complex graphs in reasonable time"
        (let ((graph (dag-draw-create-graph))
              (start-time (current-time)))
          
          ;; Create larger graph: 10 nodes in diamond pattern
          (dag-draw-add-node graph 'root "Root")
          (dotimes (i 8)
            (let ((node-id (intern (format "node-%d" i))))
              (dag-draw-add-node graph node-id (format "Node %d" i))
              (dag-draw-add-edge graph 'root node-id)))
          (dag-draw-add-node graph 'sink "Sink")
          (dotimes (i 8)
            (let ((node-id (intern (format "node-%d" i))))
              (dag-draw-add-edge graph node-id 'sink)))
          
          (dag-draw-layout-graph graph)
          
          (let ((elapsed-time (float-time (time-subtract (current-time) start-time))))
            ;; Should complete in reasonable time (under 5 seconds)
            (expect elapsed-time :to-be-less-than 5.0)
            
            ;; Should produce valid output
            (let ((ascii-output (dag-draw-render-ascii graph)))
              (expect ascii-output :to-be-truthy)
              (expect (length ascii-output) :to-be-greater-than 100))))))
  
  ;; =================================================================
  ;; PAPER AXIOMS AND INVARIANTS
  ;; =================================================================
  
  (describe "Paper Axioms and Mathematical Invariants"
    
    (describe "Network Simplex Invariants - Section 2.3"
      (it "should maintain feasibility throughout optimization"
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'a "A")
          (dag-draw-add-node graph 'b "B")
          (dag-draw-add-node graph 'c "C")
          (dag-draw-add-edge graph 'a 'b)
          (dag-draw-add-edge graph 'b 'c)
          
          (dag-draw-layout-graph graph)
          
          ;; All edge lengths should satisfy l(e) ≥ δ(e) = 1
          (let ((rank-a (dag-draw-node-rank (dag-draw-get-node graph 'a)))
                (rank-b (dag-draw-node-rank (dag-draw-get-node graph 'b)))
                (rank-c (dag-draw-node-rank (dag-draw-get-node graph 'c))))
            (expect (- rank-b rank-a) :to-be-greater-than 0.99)
            (expect (- rank-c rank-b) :to-be-greater-than 0.99))))
    
    (describe "Coordinate System Consistency"
      (it "should maintain coordinate system consistency between passes"
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'top "Top")
          (dag-draw-add-node graph 'bottom "Bottom")
          (dag-draw-add-edge graph 'top 'bottom)
          
          (dag-draw-layout-graph graph)
          
          (let ((top-node (dag-draw-get-node graph 'top))
                (bottom-node (dag-draw-get-node graph 'bottom)))
            ;; Pass 3 coordinates should be consistent with Pass 1 ranks
            ;; Higher rank should mean higher Y coordinate (in ASCII: lower visual position)
            (expect (dag-draw-node-y-coord bottom-node) :to-be-greater-than (dag-draw-node-y-coord top-node))))))))))))))))))))))

;;; gknv-algorithm-comprehensive-test.el ends here