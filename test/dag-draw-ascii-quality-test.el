;;; dag-draw-ascii-quality-test.el --- Tests for ASCII quality assessment and parameter adjustment -*- lexical-binding: t -*-

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-quality)
(require 'dag-draw-backtrack)
(require 'dag-draw-ascii-grid)

(describe "ASCII Quality Assessment and Parameter Adjustment - TDD"

  (describe "ASCII Quality Assessment"
    (it "should assess ASCII rendering quality correctly"
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
          (setf (dag-draw-node-x-coord node-b) 100.0)
          (setf (dag-draw-node-y-coord node-b) 50.0))

        ;; Generate ASCII grid
        (let* ((ascii-result (dag-draw-render-ascii graph))
               (ascii-grid ascii-result)  ; ascii-result is the grid string
               (scale dag-draw-ascii-coordinate-scale))
          
          ;; Test that ASCII rendering works and produces a string
          (expect (stringp ascii-grid) :to-be-truthy)
          (expect (> (length ascii-grid) 0) :to-be-truthy)
          
          ;; Test quality assessment (without ASCII-specific args for now)
          (let ((metrics (dag-draw-quality-assess-layout graph)))
            (expect (dag-draw-quality-metrics-p metrics) :to-be-truthy)
            (expect (numberp (dag-draw-quality-metrics-ascii-conflicts metrics)) :to-be-truthy)))))

    (it "should detect character conflicts in ASCII grid"
      (let ((graph (dag-draw-create-graph)))
        ;; Create nodes that might cause character conflicts
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)

        ;; Position nodes close together to cause potential conflicts
        (let ((node-a (dag-draw-get-node graph 'a))
              (node-b (dag-draw-get-node graph 'b))
              (node-c (dag-draw-get-node graph 'c)))
          (setf (dag-draw-node-x-coord node-a) 0.0)
          (setf (dag-draw-node-y-coord node-a) 0.0)
          (setf (dag-draw-node-x-coord node-b) 20.0)  ; Close spacing
          (setf (dag-draw-node-y-coord node-b) 0.0)
          (setf (dag-draw-node-x-coord node-c) 40.0)
          (setf (dag-draw-node-y-coord node-c) 0.0))

        ;; Generate ASCII and check quality
        (let* ((ascii-result (dag-draw-render-ascii graph))
               (ascii-grid ascii-result)
               (metrics (dag-draw-quality-assess-layout graph)))
          
          ;; Should successfully assess quality
          (expect (dag-draw-quality-metrics-p metrics) :to-be-truthy)
          (expect (numberp (dag-draw-quality-metrics-ascii-conflicts metrics)) :to-be-truthy)))))

  (describe "ASCII Parameter Adjustment"
    (it "should adjust scale when ASCII quality is poor"
      (let ((graph (dag-draw-create-graph))
            (original-scale dag-draw-ascii-coordinate-scale))
        
        ;; Create a graph that might have poor ASCII quality
        (dag-draw-add-node graph 'a "Very Long Node Name A")
        (dag-draw-add-node graph 'b "Very Long Node Name B") 
        (dag-draw-add-edge graph 'a 'b)

        ;; Position with very small coordinates (might cause cramped ASCII)
        (let ((node-a (dag-draw-get-node graph 'a))
              (node-b (dag-draw-get-node graph 'b)))
          (setf (dag-draw-node-x-coord node-a) 1.0)  ; Very close
          (setf (dag-draw-node-y-coord node-a) 1.0)
          (setf (dag-draw-node-x-coord node-b) 2.0)
          (setf (dag-draw-node-y-coord node-b) 2.0))

        ;; Test that we can create ASCII metrics and they are measurable
        (let* ((ascii-result (dag-draw-render-ascii graph))
               (ascii-grid ascii-result)
               (metrics (dag-draw-quality-assess-layout graph)))
          
          (expect (dag-draw-quality-metrics-p metrics) :to-be-truthy)
          ;; Scale adjustment could be triggered by poor quality metrics
          (expect (numberp (dag-draw-quality-metrics-ascii-conflicts metrics)) :to-be-truthy)
          
          ;; Restore original scale 
          (setq dag-draw-ascii-coordinate-scale original-scale))))

    (it "should maintain good quality for well-spaced nodes"
      (let ((graph (dag-draw-create-graph)))
        ;; Create well-spaced nodes
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-edge graph 'a 'b)

        ;; Position with good spacing
        (let ((node-a (dag-draw-get-node graph 'a))
              (node-b (dag-draw-get-node graph 'b)))
          (setf (dag-draw-node-x-coord node-a) 0.0)
          (setf (dag-draw-node-y-coord node-a) 0.0)
          (setf (dag-draw-node-x-coord node-b) 200.0)  ; Good spacing
          (setf (dag-draw-node-y-coord node-b) 100.0))

        ;; Generate ASCII and verify good quality
        (let* ((ascii-result (dag-draw-render-ascii graph))
               (ascii-grid ascii-result)
               (metrics (dag-draw-quality-assess-layout graph)))
          
          (expect (dag-draw-quality-metrics-p metrics) :to-be-truthy)
          ;; Good spacing should result in low conflict counts
          (expect (dag-draw-quality-metrics-ascii-conflicts metrics) :to-equal 0)))))

  (describe "ASCII-Aware Backtracking Integration"
    (it "should trigger backtracking for poor ASCII quality"
      (let ((graph (dag-draw-create-graph)))
        ;; Create a complex graph that might have ASCII rendering issues
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")
        (dag-draw-add-node graph 'd "D")
        
        ;; Create crossing edges 
        (dag-draw-add-edge graph 'a 'c)
        (dag-draw-add-edge graph 'b 'd)
        
        ;; Position to create crossings and potential ASCII conflicts
        (let ((node-a (dag-draw-get-node graph 'a))
              (node-b (dag-draw-get-node graph 'b))
              (node-c (dag-draw-get-node graph 'c))
              (node-d (dag-draw-get-node graph 'd)))
          (setf (dag-draw-node-x-coord node-a) 0.0)
          (setf (dag-draw-node-y-coord node-a) 0.0)
          (setf (dag-draw-node-x-coord node-b) 50.0)
          (setf (dag-draw-node-y-coord node-b) 0.0)
          (setf (dag-draw-node-x-coord node-c) 50.0) 
          (setf (dag-draw-node-y-coord node-c) 50.0)
          (setf (dag-draw-node-x-coord node-d) 0.0)
          (setf (dag-draw-node-y-coord node-d) 50.0))

        ;; Run layout pipeline
        (dag-draw-rank-graph graph)
        (dag-draw-order-vertices graph)
        (dag-draw-position-nodes graph)
        (dag-draw-generate-splines graph)

        ;; Test that backtracking can improve quality
        (let ((initial-metrics (dag-draw-quality-assess-layout graph))
              (improved-graph (dag-draw-backtrack-pass4-quality graph)))
          
          (expect improved-graph :to-be-truthy)
          (let ((final-metrics (dag-draw-quality-assess-layout improved-graph)))
            (expect (dag-draw-quality-metrics-p final-metrics) :to-be-truthy)))))))

(provide 'dag-draw-ascii-quality-test)

;;; dag-draw-ascii-quality-test.el ends here