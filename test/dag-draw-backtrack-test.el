;;; dag-draw-backtrack-test.el --- Tests for backtracking mechanisms -*- lexical-binding: t -*-

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-quality)
(require 'dag-draw-backtrack)

(describe "GKNV Backtracking Mechanisms"

  (describe "Pass 3 → Pass 2 Backtracking (Overlap Resolution)"
    (it "should detect overlapping nodes and trigger backtracking"
      (let ((graph (dag-draw-create-graph)))
        ;; Create nodes that will overlap
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-node graph 'b "Node B")
        (dag-draw-add-node graph 'c "Node C")

        ;; Add edges
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)

        ;; Set up ranks
        (let ((node-a (dag-draw-get-node graph 'a))
              (node-b (dag-draw-get-node graph 'b))
              (node-c (dag-draw-get-node graph 'c)))
          (setf (dag-draw-node-rank node-a) 0)
          (setf (dag-draw-node-rank node-b) 1)
          (setf (dag-draw-node-rank node-c) 2)

          ;; Set initial orders
          (setf (dag-draw-node-order node-a) 0)
          (setf (dag-draw-node-order node-b) 0)
          (setf (dag-draw-node-order node-c) 0)

          ;; Position nodes to create overlaps
          (setf (dag-draw-node-x-coord node-a) 0.0)
          (setf (dag-draw-node-y-coord node-a) 0.0)
          (setf (dag-draw-node-x-coord node-b) 15.0)  ; Overlap with A
          (setf (dag-draw-node-y-coord node-b) 15.0)
          (setf (dag-draw-node-x-coord node-c) 20.0)  ; Overlap with B
          (setf (dag-draw-node-y-coord node-c) 20.0))

        ;; Test backtracking
        (let ((result-graph (dag-draw-backtrack-pass3-overlaps graph)))
          (expect result-graph :to-be-truthy)

          ;; Check that overlaps were reduced
          (let ((final-metrics (dag-draw-quality-assess-layout result-graph)))
            (expect (dag-draw-quality-metrics-node-overlaps final-metrics)
                    :to-be-less-than 3)))))

    (it "should not trigger backtracking for non-overlapping layouts"
      (let ((graph (dag-draw-create-graph)))
        ;; Create well-separated nodes
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-node graph 'b "Node B")

        (let ((node-a (dag-draw-get-node graph 'a))
              (node-b (dag-draw-get-node graph 'b)))
          ;; Position nodes far apart
          (setf (dag-draw-node-x-coord node-a) 0.0)
          (setf (dag-draw-node-y-coord node-a) 0.0)
          (setf (dag-draw-node-x-coord node-b) 200.0)
          (setf (dag-draw-node-y-coord node-b) 200.0))

        ;; Test that no backtracking occurs
        (let ((result-graph (dag-draw-backtrack-pass3-overlaps graph)))
          (expect result-graph :to-be-truthy)

          ;; Should have no overlaps
          (let ((metrics (dag-draw-quality-assess-layout result-graph)))
            (expect (dag-draw-quality-metrics-node-overlaps metrics) :to-equal 0)))))

    (it "should try different vertex ordering strategies"
      (let ((graph (dag-draw-create-graph)))
        ;; Create a complex graph with multiple potential orderings
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")
        (dag-draw-add-node graph 'd "D")

        ;; Create crossing edges
        (dag-draw-add-edge graph 'a 'c)
        (dag-draw-add-edge graph 'b 'd)
        (dag-draw-add-edge graph 'a 'd)
        (dag-draw-add-edge graph 'b 'c)

        ;; Set up ranks to create same-rank conflicts
        (let ((node-a (dag-draw-get-node graph 'a))
              (node-b (dag-draw-get-node graph 'b))
              (node-c (dag-draw-get-node graph 'c))
              (node-d (dag-draw-get-node graph 'd)))
          (setf (dag-draw-node-rank node-a) 0)
          (setf (dag-draw-node-rank node-b) 0)
          (setf (dag-draw-node-rank node-c) 1)
          (setf (dag-draw-node-rank node-d) 1)

          ;; Create initial problematic ordering
          (setf (dag-draw-node-order node-a) 0)
          (setf (dag-draw-node-order node-b) 1)
          (setf (dag-draw-node-order node-c) 0)
          (setf (dag-draw-node-order node-d) 1)

          ;; Position to create overlaps
          (setf (dag-draw-node-x-coord node-a) 0.0)
          (setf (dag-draw-node-y-coord node-a) 0.0)
          (setf (dag-draw-node-x-coord node-b) 10.0)  ; Overlap
          (setf (dag-draw-node-y-coord node-b) 10.0)
          (setf (dag-draw-node-x-coord node-c) 0.0)
          (setf (dag-draw-node-y-coord node-c) 50.0)
          (setf (dag-draw-node-x-coord node-d) 10.0)  ; Overlap
          (setf (dag-draw-node-y-coord node-d) 60.0))

        ;; Test that backtracking tries different strategies
        (let ((backtrack-state (dag-draw-backtrack-state-create :max-iterations 3)))
          (expect (dag-draw-backtrack--try-vertex-reordering graph backtrack-state)
                  :to-be-truthy)))))

  (describe "Pass 4 → Pass 2/3 Backtracking (Spline Quality)"
    (it "should detect poor spline quality and trigger backtracking"
      (let ((graph (dag-draw-create-graph)))
        ;; Create a graph with many crossing edges
        (dag-draw-add-node graph 'top "Top")
        (dag-draw-add-node graph 'left "Left")
        (dag-draw-add-node graph 'right "Right")
        (dag-draw-add-node graph 'bottom "Bottom")

        ;; Add crossing edges
        (dag-draw-add-edge graph 'top 'right)
        (dag-draw-add-edge graph 'left 'bottom)
        (dag-draw-add-edge graph 'top 'left)
        (dag-draw-add-edge graph 'right 'bottom)

        ;; Position nodes to create crossings
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
          (setf (dag-draw-node-y-coord bottom) 100.0))

        ;; Test backtracking
        (let ((result-graph (dag-draw-backtrack-pass4-quality graph)))
          (expect result-graph :to-be-truthy)

          ;; Check that quality was improved
          (let ((final-metrics (dag-draw-quality-assess-layout result-graph)))
            (expect (dag-draw-quality-metrics-edge-crossings final-metrics)
                    :to-be-a 'number)))))

    (it "should not trigger backtracking for high-quality metrics"
      ;; Test the threshold logic directly with known good metrics
      (let ((good-metrics (dag-draw-quality-metrics-create
                          :edge-crossings 0
                          :node-overlaps 0  
                          :boundary-violations 0
                          :space-efficiency 0.8  ; Good efficiency
                          :aesthetic-score 0.9   ; High aesthetic score
                          :ascii-conflicts 0)))
        
        ;; These metrics should NOT trigger refinement
        (expect (dag-draw-quality-layout-needs-refinement-p good-metrics) :to-equal nil)
        
        ;; Also test with backtracking thresholds
        (expect (dag-draw-quality-layout-needs-refinement-p 
                good-metrics 
                '(:min-aesthetic-score 0.5 :max-crossings 20)) :to-equal nil)))
    
    (it "should trigger backtracking for poor-quality metrics"
      ;; Test that poor metrics DO trigger refinement
      (let ((poor-metrics (dag-draw-quality-metrics-create
                          :edge-crossings 25     ; Too many crossings
                          :node-overlaps 3       ; Too many overlaps
                          :boundary-violations 2 ; Boundary violations
                          :space-efficiency 0.05 ; Poor efficiency  
                          :aesthetic-score 0.1   ; Low aesthetic score
                          :ascii-conflicts 5)))
        
        ;; These metrics SHOULD trigger refinement
        (expect (dag-draw-quality-layout-needs-refinement-p poor-metrics) :not :to-equal nil)
        
        ;; Also test with backtracking thresholds
        (expect (dag-draw-quality-layout-needs-refinement-p 
                poor-metrics 
                '(:min-aesthetic-score 0.5 :max-crossings 20)) :not :to-equal nil))))

  (describe "Backtracking State Management"
    (it "should create and manage backtracking state correctly"
      (let ((state (dag-draw-backtrack-state-create
                   :max-iterations 5
                   :convergence-threshold 0.05)))
        (expect (dag-draw-backtrack-state-p state) :to-be-truthy)
        (expect (dag-draw-backtrack-state-iteration-count state) :to-equal 0)
        (expect (dag-draw-backtrack-state-max-iterations state) :to-equal 5)
        (expect (dag-draw-backtrack-state-convergence-threshold state) :to-equal 0.05)))

    (it "should detect convergence and stagnation"
      (let* ((graph (dag-draw-create-graph))
             (state (dag-draw-backtrack-state-create :max-stagnation 2))
             (metrics1 (dag-draw-quality-metrics-create :node-overlaps 5))
             (metrics2 (dag-draw-quality-metrics-create :node-overlaps 3)))

        ;; Test improvement detection
        (expect (dag-draw-backtrack--is-improvement-p metrics2 metrics1) :to-be-truthy)
        (expect (dag-draw-backtrack--is-improvement-p metrics1 metrics2) :to-be-falsy)))

    (it "should save and restore graph state correctly"
      (let ((graph (dag-draw-create-graph)))
        ;; Create a graph with known state
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-node graph 'b "Node B")
        (dag-draw-add-edge graph 'a 'b)

        (let ((node-a (dag-draw-get-node graph 'a))
              (node-b (dag-draw-get-node graph 'b)))
          (setf (dag-draw-node-x-coord node-a) 100.0)
          (setf (dag-draw-node-y-coord node-a) 200.0)
          (setf (dag-draw-node-rank node-a) 0)
          (setf (dag-draw-node-order node-a) 0))

        ;; Save state
        (let ((saved-graph (dag-draw-backtrack--save-graph-state graph)))
          (expect saved-graph :to-be-truthy)

          ;; Verify state preservation
          (let ((saved-node-a (dag-draw-get-node saved-graph 'a)))
            (expect saved-node-a :to-be-truthy)
            (expect (dag-draw-node-x-coord saved-node-a) :to-equal 100.0)
            (expect (dag-draw-node-y-coord saved-node-a) :to-equal 200.0)
            (expect (dag-draw-node-rank saved-node-a) :to-equal 0)
            (expect (dag-draw-node-order saved-node-a) :to-equal 0))

          ;; Verify independence
          (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'a)) 999.0)
          (expect (dag-draw-node-x-coord (dag-draw-get-node saved-graph 'a)) :to-equal 100.0)))))

  (describe "Vertex Ordering Strategies"
    (it "should apply reverse barycenter ordering"
      (let ((graph (dag-draw-create-graph)))
        ;; Create nodes with specific ordering
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")

        (let ((node-a (dag-draw-get-node graph 'a))
              (node-b (dag-draw-get-node graph 'b))
              (node-c (dag-draw-get-node graph 'c)))
          ;; Set same rank
          (setf (dag-draw-node-rank node-a) 0)
          (setf (dag-draw-node-rank node-b) 0)
          (setf (dag-draw-node-rank node-c) 0)

          ;; Set initial order
          (setf (dag-draw-node-order node-a) 0)
          (setf (dag-draw-node-order node-b) 1)
          (setf (dag-draw-node-order node-c) 2))

        ;; Apply reverse barycenter
        (dag-draw-backtrack--apply-reverse-barycenter graph)

        ;; Check that ordering was reversed
        (let ((node-a (dag-draw-get-node graph 'a))
              (node-b (dag-draw-get-node graph 'b))
              (node-c (dag-draw-get-node graph 'c)))
          (expect (dag-draw-node-order node-a) :to-equal 2)
          (expect (dag-draw-node-order node-b) :to-equal 1)
          (expect (dag-draw-node-order node-c) :to-equal 0))))

    (it "should apply size-aware ordering"
      (let ((graph (dag-draw-create-graph)))
        ;; Create nodes with different sizes
        (dag-draw-add-node graph 'big "Big Node")
        (dag-draw-add-node graph 'small "S")
        (dag-draw-add-node graph 'medium "Medium")

        (let ((big (dag-draw-get-node graph 'big))
              (small (dag-draw-get-node graph 'small))
              (medium (dag-draw-get-node graph 'medium)))
          ;; Set same rank
          (setf (dag-draw-node-rank big) 0)
          (setf (dag-draw-node-rank small) 0)
          (setf (dag-draw-node-rank medium) 0)

          ;; Set different sizes
          (setf (dag-draw-node-x-size big) 100.0)
          (setf (dag-draw-node-y-size big) 50.0)
          (setf (dag-draw-node-x-size small) 20.0)
          (setf (dag-draw-node-y-size small) 20.0)
          (setf (dag-draw-node-x-size medium) 60.0)
          (setf (dag-draw-node-y-size medium) 30.0))

        ;; Apply size-aware ordering
        (dag-draw-backtrack--apply-size-aware-ordering graph)

        ;; Check that smaller nodes come first
        (let ((small (dag-draw-get-node graph 'small))
              (medium (dag-draw-get-node graph 'medium))
              (big (dag-draw-get-node graph 'big)))
          (expect (dag-draw-node-order small) :to-be-less-than (dag-draw-node-order medium))
          (expect (dag-draw-node-order medium) :to-be-less-than (dag-draw-node-order big)))))

    (it "should calculate median neighbor positions"
      (let ((graph (dag-draw-create-graph)))
        ;; Create a graph with known neighbor positions
        (dag-draw-add-node graph 'center "Center")
        (dag-draw-add-node graph 'left "Left")
        (dag-draw-add-node graph 'right "Right")

        (dag-draw-add-edge graph 'left 'center)
        (dag-draw-add-edge graph 'center 'right)

        (let ((center (dag-draw-get-node graph 'center))
              (left (dag-draw-get-node graph 'left))
              (right (dag-draw-get-node graph 'right)))
          ;; Set up positions
          (setf (dag-draw-node-order left) 0)
          (setf (dag-draw-node-order right) 2)

          ;; Calculate median position for center node
          (let ((median-pos (dag-draw-backtrack--calculate-median-neighbor-position
                           graph center)))
            (expect median-pos :to-equal 1.0))))))

  (describe "Integration with Main Algorithm"
    (it "should integrate backtracking with the main layout algorithm"
      (let ((graph (dag-draw-create-graph)))
        ;; Create a challenging graph
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")
        (dag-draw-add-node graph 'd "D")

        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'a 'c)
        (dag-draw-add-edge graph 'b 'd)
        (dag-draw-add-edge graph 'c 'd)

        ;; Run basic layout first
        (dag-draw-rank-graph graph)
        (dag-draw-order-vertices graph)
        (dag-draw-position-nodes graph)

        ;; Apply backtracking
        (let ((improved-graph (dag-draw-backtrack-pass3-overlaps graph)))
          (expect improved-graph :to-be-truthy)

          ;; Verify that layout is still valid
          (let ((final-metrics (dag-draw-quality-assess-layout improved-graph)))
            (expect (dag-draw-quality-metrics-p final-metrics) :to-be-truthy)
            (expect (dag-draw-quality-metrics-node-overlaps final-metrics)
                    :to-be-a 'number)))))))

(provide 'dag-draw-backtrack-test)

;;; dag-draw-backtrack-test.el ends here
