;;; dag-draw-quality-test.el --- Tests for layout quality assessment -*- lexical-binding: t -*-

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-quality)

(describe "Layout Quality Assessment"

  (describe "Node Overlap Detection"
    (it "should detect overlapping nodes correctly"
      (let ((graph (dag-draw-create-graph)))
        ;; Create two overlapping nodes
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-node graph 'b "Node B")

        ;; Position them to overlap
        (let ((node-a (dag-draw-get-node graph 'a))
              (node-b (dag-draw-get-node graph 'b)))
          (setf (dag-draw-node-x-coord node-a) 10.0)
          (setf (dag-draw-node-y-coord node-a) 10.0)
          (setf (dag-draw-node-x-coord node-b) 15.0)  ; Overlap with node A
          (setf (dag-draw-node-y-coord node-b) 15.0)

          (let ((overlaps (dag-draw-quality-detect-node-overlaps graph)))
            (expect (length overlaps) :to-equal 1)
            (expect (nth 0 (car overlaps)) :to-be 'a)
            (expect (nth 1 (car overlaps)) :to-be 'b)
            (expect (nth 2 (car overlaps)) :to-be-greater-than 0)))))

    (it "should not detect overlaps when nodes are separated"
      (let ((graph (dag-draw-create-graph)))
        ;; Create two non-overlapping nodes
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-node graph 'b "Node B")

        ;; Position them far apart
        (let ((node-a (dag-draw-get-node graph 'a))
              (node-b (dag-draw-get-node graph 'b)))
          (setf (dag-draw-node-x-coord node-a) 0.0)
          (setf (dag-draw-node-y-coord node-a) 0.0)
          (setf (dag-draw-node-x-coord node-b) 200.0)
          (setf (dag-draw-node-y-coord node-b) 200.0)

          (let ((overlaps (dag-draw-quality-detect-node-overlaps graph)))
            (expect (length overlaps) :to-equal 0))))))

  (describe "Edge Crossing Detection"
    (it "should detect edge crossings correctly"
      (let ((graph (dag-draw-create-graph)))
        ;; Create a diamond pattern that will have crossing edges
        (dag-draw-add-node graph 'top "Top")
        (dag-draw-add-node graph 'left "Left")
        (dag-draw-add-node graph 'right "Right")
        (dag-draw-add-node graph 'bottom "Bottom")

        ;; Add crossing edges: top->right, left->bottom
        (dag-draw-add-edge graph 'top 'right)
        (dag-draw-add-edge graph 'left 'bottom)

        ;; Position nodes to create crossing
        (let ((top (dag-draw-get-node graph 'top))
              (left (dag-draw-get-node graph 'left))
              (right (dag-draw-get-node graph 'right))
              (bottom (dag-draw-get-node graph 'bottom)))
          (setf (dag-draw-node-x-coord top) 50.0)
          (setf (dag-draw-node-y-coord top) 0.0)
          (setf (dag-draw-node-x-coord left) 0.0)
          (setf (dag-draw-node-y-coord left) 50.0)
          (setf (dag-draw-node-x-coord right) 100.0)
          (setf (dag-draw-node-y-coord right) 50.0)
          (setf (dag-draw-node-x-coord bottom) 50.0)
          (setf (dag-draw-node-y-coord bottom) 100.0)

          (let ((crossings (dag-draw-quality-count-edge-crossings graph)))
            (expect crossings :to-be-greater-than 0)))))

    (it "should not detect crossings for non-crossing edges"
      (let ((graph (dag-draw-create-graph)))
        ;; Create simple linear chain
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")

        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)

        ;; Position in line
        (let ((node-a (dag-draw-get-node graph 'a))
              (node-b (dag-draw-get-node graph 'b))
              (node-c (dag-draw-get-node graph 'c)))
          (setf (dag-draw-node-x-coord node-a) 0.0)
          (setf (dag-draw-node-y-coord node-a) 0.0)
          (setf (dag-draw-node-x-coord node-b) 50.0)
          (setf (dag-draw-node-y-coord node-b) 0.0)
          (setf (dag-draw-node-x-coord node-c) 100.0)
          (setf (dag-draw-node-y-coord node-c) 0.0)

          (let ((crossings (dag-draw-quality-count-edge-crossings graph)))
            (expect crossings :to-equal 0))))))

  (describe "Space Efficiency Assessment"
    (it "should calculate space efficiency correctly"
      (let ((graph (dag-draw-create-graph)))
        ;; Create nodes with known dimensions
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")

        (let ((node-a (dag-draw-get-node graph 'a))
              (node-b (dag-draw-get-node graph 'b)))
          ;; Set specific sizes
          (setf (dag-draw-node-x-size node-a) 20.0)
          (setf (dag-draw-node-y-size node-a) 10.0)
          (setf (dag-draw-node-x-size node-b) 20.0)
          (setf (dag-draw-node-y-size node-b) 10.0)

          ;; Position them efficiently (compact layout)
          (setf (dag-draw-node-x-coord node-a) 0.0)
          (setf (dag-draw-node-y-coord node-a) 0.0)
          (setf (dag-draw-node-x-coord node-b) 25.0)  ; Just enough separation
          (setf (dag-draw-node-y-coord node-b) 0.0)

          (let ((efficiency (dag-draw-quality-calculate-space-efficiency graph)))
            (expect efficiency :to-be-greater-than 0.0)
            (expect efficiency :to-be-less-than-or-equal-to 1.0)))))

    (it "should give higher efficiency for compact layouts"
      (let ((graph-compact (dag-draw-create-graph))
            (graph-sparse (dag-draw-create-graph)))

        ;; Create identical graphs
        (dag-draw-add-node graph-compact 'a "A")
        (dag-draw-add-node graph-compact 'b "B")
        (dag-draw-add-node graph-sparse 'a "A")
        (dag-draw-add-node graph-sparse 'b "B")

        ;; Set same sizes
        (dolist (graph (list graph-compact graph-sparse))
          (let ((node-a (dag-draw-get-node graph 'a))
                (node-b (dag-draw-get-node graph 'b)))
            (setf (dag-draw-node-x-size node-a) 20.0)
            (setf (dag-draw-node-y-size node-a) 10.0)
            (setf (dag-draw-node-x-size node-b) 20.0)
            (setf (dag-draw-node-y-size node-b) 10.0)))

        ;; Compact layout
        (let ((node-a (dag-draw-get-node graph-compact 'a))
              (node-b (dag-draw-get-node graph-compact 'b)))
          (setf (dag-draw-node-x-coord node-a) 0.0)
          (setf (dag-draw-node-y-coord node-a) 0.0)
          (setf (dag-draw-node-x-coord node-b) 25.0)
          (setf (dag-draw-node-y-coord node-b) 0.0))

        ;; Sparse layout
        (let ((node-a (dag-draw-get-node graph-sparse 'a))
              (node-b (dag-draw-get-node graph-sparse 'b)))
          (setf (dag-draw-node-x-coord node-a) 0.0)
          (setf (dag-draw-node-y-coord node-a) 0.0)
          (setf (dag-draw-node-x-coord node-b) 200.0)  ; Much more separated
          (setf (dag-draw-node-y-coord node-b) 0.0))

        (let ((compact-efficiency (dag-draw-quality-calculate-space-efficiency graph-compact))
              (sparse-efficiency (dag-draw-quality-calculate-space-efficiency graph-sparse)))
          (expect compact-efficiency :to-be-greater-than sparse-efficiency)))))

  (describe "Foundation TDD Fix"
    (it "should create quality metrics without crashing"
      (let ((graph (dag-draw-create-graph)))
        ;; Create minimal graph with just one node
        (dag-draw-add-node graph 'a "A")
        
        ;; Ensure node has minimal required properties
        (let ((node-a (dag-draw-get-node graph 'a)))
          (setf (dag-draw-node-x-coord node-a) 0.0)
          (setf (dag-draw-node-y-coord node-a) 0.0))
        
        ;; This should not crash
        (let ((metrics (dag-draw-quality-assess-layout graph)))
          (expect (dag-draw-quality-metrics-p metrics) :to-be-truthy)
          (expect (numberp (dag-draw-quality-metrics-node-overlaps metrics)) :to-be-truthy)
          (expect (numberp (dag-draw-quality-metrics-edge-crossings metrics)) :to-be-truthy))))
    
    (it "should detect zero crossings for linear chain without geometry issues"
      (let ((graph (dag-draw-create-graph)))
        ;; Create simple linear chain A→B→C
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B") 
        (dag-draw-add-node graph 'c "C")
        
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)
        
        ;; Position nodes in perfect line (should have 0 crossings)
        (let ((node-a (dag-draw-get-node graph 'a))
              (node-b (dag-draw-get-node graph 'b))
              (node-c (dag-draw-get-node graph 'c)))
          (setf (dag-draw-node-x-coord node-a) 0.0)
          (setf (dag-draw-node-y-coord node-a) 0.0)
          (setf (dag-draw-node-x-coord node-b) 50.0)
          (setf (dag-draw-node-y-coord node-b) 0.0)  ; Same Y - perfect line
          (setf (dag-draw-node-x-coord node-c) 100.0)
          (setf (dag-draw-node-y-coord node-c) 0.0)) ; Same Y - perfect line
        
        ;; A linear chain should have 0 crossings
        (let ((crossings (dag-draw-quality-count-edge-crossings graph)))
          (expect crossings :to-equal 0))))
    
    (it "should detect zero overlaps for separated nodes" 
      (let ((graph (dag-draw-create-graph)))
        ;; Create two well-separated nodes
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        
        (let ((node-a (dag-draw-get-node graph 'a))
              (node-b (dag-draw-get-node graph 'b)))
          ;; Position them far apart
          (setf (dag-draw-node-x-coord node-a) 0.0)
          (setf (dag-draw-node-y-coord node-a) 0.0)
          (setf (dag-draw-node-x-coord node-b) 200.0)
          (setf (dag-draw-node-y-coord node-b) 200.0))
        
        ;; Should have no overlaps  
        (let ((overlaps (dag-draw-quality-detect-node-overlaps graph)))
          (expect (length overlaps) :to-equal 0)))))

  (describe "Comprehensive Quality Assessment"
    (it "should create quality metrics for a complete layout"
      (let ((graph (dag-draw-create-graph)))
        ;; Create a simple graph
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-edge graph 'a 'b)

        ;; Position nodes
        (let ((node-a (dag-draw-get-node graph 'a))
              (node-b (dag-draw-get-node graph 'b)))
          (setf (dag-draw-node-x-coord node-a) 0.0)
          (setf (dag-draw-node-y-coord node-a) 0.0)
          (setf (dag-draw-node-x-coord node-b) 50.0)
          (setf (dag-draw-node-y-coord node-b) 50.0))

        (let ((metrics (dag-draw-quality-assess-layout graph)))
          (expect (dag-draw-quality-metrics-p metrics) :to-be-truthy)
          (expect (dag-draw-quality-metrics-edge-crossings metrics) :to-be-a 'number)
          (expect (dag-draw-quality-metrics-node-overlaps metrics) :to-be-a 'number)
          (expect (dag-draw-quality-metrics-space-efficiency metrics) :to-be-a 'number)
          (expect (dag-draw-quality-metrics-aesthetic-score metrics) :to-be-a 'number))))

    (it "should identify layouts that need refinement"
      (let ((graph (dag-draw-create-graph)))
        ;; Create overlapping nodes to trigger refinement need
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")

        (let ((node-a (dag-draw-get-node graph 'a))
              (node-b (dag-draw-get-node graph 'b)))
          ;; Create overlap
          (setf (dag-draw-node-x-coord node-a) 0.0)
          (setf (dag-draw-node-y-coord node-a) 0.0)
          (setf (dag-draw-node-x-coord node-b) 10.0)  ; Overlapping position
          (setf (dag-draw-node-y-coord node-b) 10.0))

        (let ((metrics (dag-draw-quality-assess-layout graph)))
          (expect (dag-draw-quality-layout-needs-refinement-p metrics) :to-be-truthy))))

    (it "should compare layout quality correctly"
      (let ((graph-good (dag-draw-create-graph))
            (graph-bad (dag-draw-create-graph)))

        ;; Create identical base graphs
        (dolist (graph (list graph-good graph-bad))
          (dag-draw-add-node graph 'a "A")
          (dag-draw-add-node graph 'b "B"))

        ;; Good layout - no overlaps
        (let ((node-a (dag-draw-get-node graph-good 'a))
              (node-b (dag-draw-get-node graph-good 'b)))
          (setf (dag-draw-node-x-coord node-a) 0.0)
          (setf (dag-draw-node-y-coord node-a) 0.0)
          (setf (dag-draw-node-x-coord node-b) 100.0)
          (setf (dag-draw-node-y-coord node-b) 0.0))

        ;; Bad layout - overlapping
        (let ((node-a (dag-draw-get-node graph-bad 'a))
              (node-b (dag-draw-get-node graph-bad 'b)))
          (setf (dag-draw-node-x-coord node-a) 0.0)
          (setf (dag-draw-node-y-coord node-a) 0.0)
          (setf (dag-draw-node-x-coord node-b) 10.0)  ; Overlap
          (setf (dag-draw-node-y-coord node-b) 10.0))

        (let ((metrics-good (dag-draw-quality-assess-layout graph-good))
              (metrics-bad (dag-draw-quality-assess-layout graph-bad)))
          (expect (dag-draw-quality-compare-layouts metrics-good metrics-bad)
                  :to-equal :a-better))))))

(provide 'dag-draw-quality-test)

;;; dag-draw-quality-test.el ends here
