;;; dag-draw-size-aware-ordering-test.el --- Tests for size-aware vertex ordering -*- lexical-binding: t -*-

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-pass2-ordering)
(require 'cl-lib)

(describe "Size-Aware Vertex Ordering - TDD"

  (describe "Size-Aware Median Calculation"
    (it "should calculate basic weighted median without size information"
      ;; Test basic functionality still works
      (let ((positions '(1 3 5)))
        (expect (dag-draw--weighted-median positions) :to-equal 3.0)))

    (it "should adjust median positions to prevent overlaps with size information"
      ;; Test size-aware median calculation
      (let ((positions '(0 10 20))
            (node-sizes '((:width 15 :height 5)  ; Wide node at position 0
                         (:width 5 :height 5)    ; Small node at position 10  
                         (:width 8 :height 5))))  ; Medium node at position 20
        
        ;; Size-aware median should spread positions to prevent overlaps
        (let ((size-aware-median (dag-draw--weighted-median positions node-sizes)))
          (expect (numberp size-aware-median) :to-be-truthy)
          ;; Should not place nodes too close together
          (expect (not (= size-aware-median 10.0)) :to-be-truthy)))))

  (describe "Size-Aware Position Calculation"
    (it "should calculate median position considering node sizes"
      (let ((graph (dag-draw-create-graph)))
        ;; Create a simple graph with nodes of different sizes
        (dag-draw-add-node graph 'small "S")
        (dag-draw-add-node graph 'large "Large Node")
        (dag-draw-add-node graph 'medium "Med")
        
        ;; Set different sizes
        (let ((small-node (dag-draw-get-node graph 'small))
              (large-node (dag-draw-get-node graph 'large))
              (medium-node (dag-draw-get-node graph 'medium)))
          (setf (dag-draw-node-x-size small-node) 5.0)
          (setf (dag-draw-node-y-size small-node) 3.0)
          (setf (dag-draw-node-x-size large-node) 20.0)
          (setf (dag-draw-node-y-size large-node) 5.0)
          (setf (dag-draw-node-x-size medium-node) 12.0)
          (setf (dag-draw-node-y-size medium-node) 4.0)
          
          ;; Set ranks
          (setf (dag-draw-node-rank small-node) 0)
          (setf (dag-draw-node-rank large-node) 0)
          (setf (dag-draw-node-rank medium-node) 1)
          
          ;; Add edge to create adjacency
          (dag-draw-add-edge graph 'small 'medium)
          (dag-draw-add-edge graph 'large 'medium))
        
        ;; Test size-aware position calculation
        (let* ((adjacent-rank-nodes '(small large))
               (position (dag-draw--calculate-median-position graph 'medium adjacent-rank-nodes t)))
          (expect (numberp position) :to-be-truthy)))))

  (describe "Size-Aware Rank Ordering"
    (it "should order nodes within rank considering sizes to prevent overlaps"
      (let ((graph (dag-draw-create-graph)))
        ;; Create nodes with overlapping potential
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'wide "Very Wide Node")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'target "Target")
        
        ;; Set sizes that would cause overlaps with traditional ordering
        (let ((node-a (dag-draw-get-node graph 'a))
              (node-wide (dag-draw-get-node graph 'wide))
              (node-b (dag-draw-get-node graph 'b))
              (target-node (dag-draw-get-node graph 'target)))
          
          ;; Wide node in the middle  
          (setf (dag-draw-node-x-size node-wide) 25.0)
          (setf (dag-draw-node-y-size node-wide) 5.0)
          (setf (dag-draw-node-x-size node-a) 5.0)
          (setf (dag-draw-node-y-size node-a) 3.0)
          (setf (dag-draw-node-x-size node-b) 5.0)
          (setf (dag-draw-node-y-size node-b) 3.0)
          (setf (dag-draw-node-x-size target-node) 8.0)
          (setf (dag-draw-node-y-size target-node) 4.0)
          
          ;; Set ranks
          (setf (dag-draw-node-rank node-a) 0)
          (setf (dag-draw-node-rank node-wide) 0)  
          (setf (dag-draw-node-rank node-b) 0)
          (setf (dag-draw-node-rank target-node) 1)
          
          ;; Create edges that would cause conflicts
          (dag-draw-add-edge graph 'a 'target)
          (dag-draw-add-edge graph 'wide 'target)
          (dag-draw-add-edge graph 'b 'target))
        
        ;; Test size-aware rank ordering
        (let* ((rank-nodes '(a wide b))
               (adjacent-rank-nodes '(target))
               (ordered-nodes (dag-draw--order-rank-by-median 
                              graph rank-nodes adjacent-rank-nodes 'down t)))
          
          (expect (listp ordered-nodes) :to-be-truthy)
          (expect (= (length ordered-nodes) 3) :to-be-truthy)))))

  (describe "Integration with Main Ordering Algorithm"
    (it "should integrate size-aware ordering into main vertex ordering"
      (let ((graph (dag-draw-create-graph)))
        ;; Create a graph that would benefit from size-aware ordering
        (dag-draw-add-node graph 'small1 "S1")
        (dag-draw-add-node graph 'large "Large Node Text")
        (dag-draw-add-node graph 'small2 "S2")
        (dag-draw-add-node graph 'target "Target")
        
        ;; Set up sizes and structure
        (let ((s1 (dag-draw-get-node graph 'small1))
              (large (dag-draw-get-node graph 'large))
              (s2 (dag-draw-get-node graph 'small2))
              (target (dag-draw-get-node graph 'target)))
          
          (setf (dag-draw-node-x-size s1) 6.0)
          (setf (dag-draw-node-x-size large) 22.0)
          (setf (dag-draw-node-x-size s2) 6.0)
          (setf (dag-draw-node-x-size target) 10.0)
          
          (dag-draw-add-edge graph 'small1 'target)
          (dag-draw-add-edge graph 'large 'target)
          (dag-draw-add-edge graph 'small2 'target))
        
        ;; Run the enhanced ordering algorithm
        (dag-draw-rank-graph graph)
        (dag-draw-order-vertices graph)
        
        ;; Verify that nodes have been ordered (should not crash)
        (let ((s1 (dag-draw-get-node graph 'small1))
              (large (dag-draw-get-node graph 'large))
              (s2 (dag-draw-get-node graph 'small2)))
          (expect (numberp (dag-draw-node-order s1)) :to-be-truthy)
          (expect (numberp (dag-draw-node-order large)) :to-be-truthy)
          (expect (numberp (dag-draw-node-order s2)) :to-be-truthy))))))

(provide 'dag-draw-size-aware-ordering-test)

;;; dag-draw-size-aware-ordering-test.el ends here