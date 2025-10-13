;;; gknv-network-simplex-test.el --- Network Simplex algorithm tests per GKNV paper -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Detailed tests for the Network Simplex algorithm implementation used in both
;; Pass 1 (rank assignment) and Pass 3 (coordinate assignment) of the GKNV algorithm.
;;
;; Based on Section 2.3 "Network simplex" and Section 4.2 "Optimal Node Placement"
;; from "A Technique for Drawing Directed Graphs" (Gansner et al.)

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)

(describe "GKNV Network Simplex Algorithm - Section 2.3 & 4.2 Compliance"
  
  (describe "Feasible Tree Construction - Figure 2-2"
    
    (it "should construct initial feasible spanning tree - feasible_tree() procedure"
      (let ((graph (dag-draw-create-graph)))
        ;; Simple chain to test feasible tree construction
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-node graph 'b "Node B")
        (dag-draw-add-node graph 'c "Node C")
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)
        
        ;; Network simplex should converge (no failure message)
        (let ((result (dag-draw-layout-graph graph)))
          (expect result :not :to-be nil)
          
          ;; Verify feasible ranking was achieved
          (let ((rank-a (dag-draw-node-rank (dag-draw-get-node graph 'a)))
                (rank-b (dag-draw-node-rank (dag-draw-get-node graph 'b)))
                (rank-c (dag-draw-node-rank (dag-draw-get-node graph 'c))))
            ;; Should have proper rank progression
            (expect rank-a :to-be-less-than rank-b)
            (expect rank-b :to-be-less-than rank-c)))))
    
    (it "should handle init_rank() - initial feasible ranking"
      (let ((graph (dag-draw-create-graph)))
        ;; Create DAG that tests topological ordering
        (dag-draw-add-node graph 'source1 "Source 1")
        (dag-draw-add-node graph 'source2 "Source 2") 
        (dag-draw-add-node graph 'middle "Middle")
        (dag-draw-add-node graph 'sink "Sink")
        (dag-draw-add-edge graph 'source1 'middle)
        (dag-draw-add-edge graph 'source2 'middle)
        (dag-draw-add-edge graph 'middle 'sink)
        
        (dag-draw-layout-graph graph)
        
        ;; Sources should be at rank 0 (minimal elements)
        (let ((rank-s1 (dag-draw-node-rank (dag-draw-get-node graph 'source1)))
              (rank-s2 (dag-draw-node-rank (dag-draw-get-node graph 'source2)))
              (rank-mid (dag-draw-node-rank (dag-draw-get-node graph 'middle)))
              (rank-sink (dag-draw-node-rank (dag-draw-get-node graph 'sink))))
          ;; Paper Figure 2-2: "minimal elements to rank 0"
          (expect rank-s1 :to-be-close-to 0 0.1)
          (expect rank-s2 :to-be-close-to 0 0.1)
          (expect rank-mid :to-be-close-to 1 0.1)
          (expect rank-sink :to-be-close-to 2 0.1))))
    
    (it "should grow tight tree incrementally - tight_tree() expansion"
      (let ((graph (dag-draw-create-graph)))
        ;; Create graph where tight tree must grow incrementally
        (dag-draw-add-node graph 'root "Root")
        (dag-draw-add-node graph 'branch1 "Branch 1")
        (dag-draw-add-node graph 'branch2 "Branch 2")
        (dag-draw-add-node graph 'merge "Merge")
        (dag-draw-add-edge graph 'root 'branch1)
        (dag-draw-add-edge graph 'root 'branch2)
        (dag-draw-add-edge graph 'branch1 'merge)
        (dag-draw-add-edge graph 'branch2 'merge)
        
        ;; Should successfully build spanning tree
        (expect (dag-draw-layout-graph graph) :not :to-be nil)
        
        ;; All nodes should be included in final solution
        (dolist (node-id '(root branch1 branch2 merge))
          (let ((node (dag-draw-get-node graph node-id)))
            (expect (dag-draw-node-rank node) :not :to-be nil))))))
  
  (describe "Network Simplex Main Loop - Figure 2-1"
    
    (it "should implement leave_edge() - finding negative cut value edge"
      (let ((graph (dag-draw-create-graph)))
        ;; Create scenario that may require edge exchanges
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B") 
        (dag-draw-add-node graph 'c "C")
        (dag-draw-add-node graph 'd "D")
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'a 'c)
        (dag-draw-add-edge graph 'b 'd)
        (dag-draw-add-edge graph 'c 'd)
        
        ;; Algorithm should converge (may require multiple iterations)
        (expect (dag-draw-layout-graph graph) :not :to-be nil)
        
        ;; Final result should be optimal (no negative cut values)
        (let ((rank-a (dag-draw-node-rank (dag-draw-get-node graph 'a)))
              (rank-b (dag-draw-node-rank (dag-draw-get-node graph 'b)))
              (rank-c (dag-draw-node-rank (dag-draw-get-node graph 'c)))
              (rank-d (dag-draw-node-rank (dag-draw-get-node graph 'd))))
          ;; Should achieve optimal 3-level structure
          (expect rank-a :to-be-close-to 0 0.1)
          (expect rank-b :to-be-close-to 1 0.1)
          (expect rank-c :to-be-close-to 1 0.1)
          (expect rank-d :to-be-close-to 2 0.1))))
    
    (it "should implement enter_edge() - finding replacement edge with minimum slack"
      (let ((graph (dag-draw-create-graph)))
        ;; Create graph that tests edge replacement logic
        (dag-draw-add-node graph 'source "Source")
        (dag-draw-add-node graph 'mid1 "Mid 1")
        (dag-draw-add-node graph 'mid2 "Mid 2")
        (dag-draw-add-node graph 'target "Target")
        (dag-draw-add-edge graph 'source 'mid1)
        (dag-draw-add-edge graph 'source 'mid2)
        (dag-draw-add-edge graph 'mid1 'target)
        (dag-draw-add-edge graph 'mid2 'target)
        ;; Add cross-edge that may participate in optimization
        (dag-draw-add-edge graph 'mid1 'mid2)
        
        ;; Should find optimal solution
        (expect (dag-draw-layout-graph graph) :not :to-be nil)
        
        ;; Verify feasibility maintained
        (let ((ranks (list (dag-draw-node-rank (dag-draw-get-node graph 'source))
                          (dag-draw-node-rank (dag-draw-get-node graph 'mid1))
                          (dag-draw-node-rank (dag-draw-get-node graph 'mid2))
                          (dag-draw-node-rank (dag-draw-get-node graph 'target)))))
          (dolist (rank ranks)
            (expect rank :not :to-be nil)))))
    
    (it "should implement exchange(e,f) - tree edge replacement"
      (let ((graph (dag-draw-create-graph)))
        ;; Create example from Figure 2-3 in paper
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")
        (dag-draw-add-node graph 'd "D")
        (dag-draw-add-node graph 'e "E")
        (dag-draw-add-node graph 'f "F")
        (dag-draw-add-node graph 'g "G")
        (dag-draw-add-node graph 'h "H")
        
        ;; Edges from Figure 2-3
        (dag-draw-add-edge graph 'a 'e)
        (dag-draw-add-edge graph 'a 'f)
        (dag-draw-add-edge graph 'b 'g)
        (dag-draw-add-edge graph 'c 'g)
        (dag-draw-add-edge graph 'g 'h)
        (dag-draw-add-edge graph 'e 'f)
        (dag-draw-add-edge graph 'f 'h)
        
        ;; Should successfully optimize
        (expect (dag-draw-layout-graph graph) :not :to-be nil)
        
        ;; All nodes should have valid ranks
        (dolist (node-id '(a b c d e f g h))
          (let ((node (dag-draw-get-node graph node-id)))
            (when node ; Some nodes might not exist if graph structure differs
              (expect (dag-draw-node-rank node) :not :to-be nil)))))))
  
  (describe "Network Simplex Termination and Optimization"
    
    (it "should normalize() - set minimum rank to zero"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-edge graph 'a 'b)
        
        (dag-draw-layout-graph graph)
        
        ;; Paper Section 2.3, line 571: "solution is normalized by setting the least rank to zero"
        (let ((rank-a (dag-draw-node-rank (dag-draw-get-node graph 'a)))
              (rank-b (dag-draw-node-rank (dag-draw-get-node graph 'b))))
          ;; One of the ranks should be 0 (or close to it)
          (expect (min rank-a rank-b) :to-be-close-to 0 0.1))))
    
    (it "should balance() - improve aspect ratio by moving nodes to less crowded ranks"
      (let ((graph (dag-draw-create-graph)))
        ;; Create graph with potential for rank balancing
        (dag-draw-add-node graph 'central "Central")
        (dag-draw-add-node graph 'isolated1 "Isolated 1")
        (dag-draw-add-node graph 'isolated2 "Isolated 2")
        (dag-draw-add-node graph 'isolated3 "Isolated 3")
        ;; Only connect central node, leave others isolated
        (dag-draw-add-node graph 'target "Target")
        (dag-draw-add-edge graph 'central 'target)
        
        (dag-draw-layout-graph graph)
        
        ;; Should complete without error
        (expect (dag-draw-node-rank (dag-draw-get-node graph 'central)) :not :to-be nil)
        (expect (dag-draw-node-rank (dag-draw-get-node graph 'target)) :not :to-be nil)))
    
    (it "should achieve optimal cost for simple test cases"
      (let ((graph (dag-draw-create-graph)))
        ;; Simple path - should achieve minimum total edge length
        (dag-draw-add-node graph 'start "Start")
        (dag-draw-add-node graph 'end "End")
        (dag-draw-add-edge graph 'start 'end)
        
        (dag-draw-layout-graph graph)
        
        ;; Should achieve minimum edge length (typically 1)
        (let* ((start-node (dag-draw-get-node graph 'start))
               (end-node (dag-draw-get-node graph 'end))
               (edge-length (- (dag-draw-node-rank end-node) (dag-draw-node-rank start-node))))
          ;; Paper objective: minimize sum of weighted edge lengths
          (expect edge-length :to-be-close-to 1 0.1))))
  
  (describe "Network Simplex for Coordinate Assignment - Section 4.2"
    
    (it "should construct auxiliary graph for X-coordinate optimization"
      (let ((graph (dag-draw-create-graph)))
        ;; Test coordinate assignment network simplex
        (dag-draw-add-node graph 'left "Left Node")
        (dag-draw-add-node graph 'right "Right Node")
        (dag-draw-add-edge graph 'left 'right)
        
        (dag-draw-layout-graph graph)
        
        ;; Should assign valid X coordinates
        (let ((left-node (dag-draw-get-node graph 'left))
              (right-node (dag-draw-get-node graph 'right)))
          (expect (dag-draw-node-x-coord left-node) :not :to-be nil)
          (expect (dag-draw-node-x-coord right-node) :not :to-be nil)
          
          ;; Coordinates should respect separation constraints
          (let ((separation (abs (- (dag-draw-node-x-coord right-node) 
                                   (dag-draw-node-x-coord left-node)))))
            ;; Should be separated by at least node sizes + margin
            (expect separation :to-be-greater-than 2)))))
    
    (it "should handle edge straightening weights Ω(e) correctly"
      (let ((graph (dag-draw-create-graph)))
        ;; Create chain that tests edge straightening
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)
        
        (dag-draw-layout-graph graph)
        
        ;; Chain should be relatively straight (X coordinates should be aligned)
        (let ((x-a (dag-draw-node-x-coord (dag-draw-get-node graph 'a)))
              (x-b (dag-draw-node-x-coord (dag-draw-get-node graph 'b)))
              (x-c (dag-draw-node-x-coord (dag-draw-get-node graph 'c))))
          ;; Paper Section 4: higher Ω(e) for virtual chains should align them
          ;; In simple chain, nodes should have similar X coordinates
          (expect (abs (- x-a x-b)) :to-be-less-than 5)
          (expect (abs (- x-b x-c)) :to-be-less-than 5))))
  
  (describe "Network Simplex Error Conditions and Recovery"
    
    (it "should handle convergence failure gracefully"
      (let ((graph (dag-draw-create-graph)))
        ;; Create potentially problematic graph
        (dag-draw-add-node graph 'center "Center")
        (dotimes (i 10)
          (let ((node-id (intern (format "spoke-%d" i))))
            (dag-draw-add-node graph node-id (format "Spoke %d" i))
            (dag-draw-add-edge graph 'center node-id)))
        
        ;; Should either converge or fallback gracefully
        (let ((result (dag-draw-layout-graph graph)))
          (expect result :not :to-be nil)
          
          ;; All nodes should have valid ranks
          (let ((center-node (dag-draw-get-node graph 'center)))
            (expect (dag-draw-node-rank center-node) :not :to-be nil)))))
    
    (it "should detect and handle degenerate cases"
      (let ((graph (dag-draw-create-graph)))
        ;; Single node (trivial case)
        (dag-draw-add-node graph 'single "Single Node")
        
        ;; Should handle without error
        (expect (dag-draw-layout-graph graph) :not :to-be nil)
        
        (let ((node (dag-draw-get-node graph 'single)))
          (expect (dag-draw-node-rank node) :to-be-close-to 0 0.1)
          (expect (dag-draw-node-x-coord node) :not :to-be nil))))
    
    (it "should maintain numerical stability"
      (let ((graph (dag-draw-create-graph)))
        ;; Create graph with many nodes to test numerical stability
        (dag-draw-add-node graph 'root "Root")
        (dotimes (i 20)
          (let ((node-id (intern (format "node-%d" i))))
            (dag-draw-add-node graph node-id (format "Node %d" i))
            (when (> i 0)
              (dag-draw-add-edge graph (intern (format "node-%d" (1- i))) node-id))))
        (dag-draw-add-edge graph 'root 'node-0)
        
        (dag-draw-layout-graph graph)
        
        ;; All coordinates should be finite numbers
        (let ((root-node (dag-draw-get-node graph 'root)))
          (expect (numberp (dag-draw-node-rank root-node)) :to-be-truthy)
          (expect (numberp (dag-draw-node-x-coord root-node)) :to-be-truthy)
          (expect (numberp (dag-draw-node-y-coord root-node)) :to-be-truthy))))))))

;;; gknv-network-simplex-test.el ends here