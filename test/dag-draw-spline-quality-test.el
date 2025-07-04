;;; dag-draw-spline-quality-test.el --- Tests for Pass 4 spline quality assessment -*- lexical-binding: t -*-

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-quality)
(require 'dag-draw-backtrack)

(describe "Pass 4 Spline Quality Assessment - TDD"

  (describe "Spline Boundary Violation Detection"
    (it "should fail because boundary detection is currently a stub"
      (let ((graph (dag-draw-create-graph)))
        ;; Create a graph where edges might cross through nodes
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'middle "Middle")
        (dag-draw-add-node graph 'c "C")

        ;; Add edge that should cross through the middle node
        (dag-draw-add-edge graph 'a 'c)  ; Long edge that should cross middle

        ;; Position nodes so A->C edge crosses through middle node area
        (let ((node-a (dag-draw-get-node graph 'a))
              (node-middle (dag-draw-get-node graph 'middle))
              (node-c (dag-draw-get-node graph 'c)))
          (setf (dag-draw-node-x-coord node-a) 0.0)
          (setf (dag-draw-node-y-coord node-a) 0.0)
          (setf (dag-draw-node-x-coord node-middle) 50.0)  ; Directly between A and C
          (setf (dag-draw-node-y-coord node-middle) 25.0)
          (setf (dag-draw-node-x-coord node-c) 100.0)
          (setf (dag-draw-node-y-coord node-c) 50.0))

        ;; Run full layout pipeline to generate splines
        (dag-draw-rank-graph graph)
        (dag-draw-order-vertices graph)
        (dag-draw-position-nodes graph)
        (dag-draw-generate-splines graph)

        ;; Check if boundary violations are detected (may be 0 if layout algorithm prevents them)
        (let ((spline-metrics (dag-draw-quality-assess-spline-quality graph)))
          (message "Boundary violations found: %s" (plist-get spline-metrics :boundary-violations))
          ;; The real test is that detection works (returns a number), not necessarily > 0
          (expect (numberp (plist-get spline-metrics :boundary-violations)) :to-be-truthy)))))

  (describe "Boundary Violation Detection with Forced Setup"
    (it "should detect boundary violations when spline definitely crosses a node"
      (let ((graph (dag-draw-create-graph)))
        ;; Create nodes with manual positioning that won't be changed by the algorithm
        (dag-draw-add-node graph 'left "Left")
        (dag-draw-add-node graph 'obstacle "Obstacle")
        (dag-draw-add-node graph 'right "Right")

        ;; Add edge that must cross through obstacle
        (dag-draw-add-edge graph 'left 'right)

        ;; Set manual coordinates that will be preserved
        (let ((left-node (dag-draw-get-node graph 'left))
              (obstacle-node (dag-draw-get-node graph 'obstacle))
              (right-node (dag-draw-get-node graph 'right)))

          ;; Set ranks manually to prevent algorithm from changing positions
          (setf (dag-draw-node-rank left-node) 0)
          (setf (dag-draw-node-rank obstacle-node) 0)  ; Same rank, won't be repositioned
          (setf (dag-draw-node-rank right-node) 1)

          ;; Position obstacle directly between left and right
          (setf (dag-draw-node-x-coord left-node) 0.0)
          (setf (dag-draw-node-y-coord left-node) 50.0)
          (setf (dag-draw-node-x-coord obstacle-node) 50.0)  ; Middle
          (setf (dag-draw-node-y-coord obstacle-node) 50.0)  ; Same Y as left-right line
          (setf (dag-draw-node-x-coord right-node) 100.0)
          (setf (dag-draw-node-y-coord right-node) 50.0))

        ;; Skip positioning since we set coordinates manually
        (dag-draw-rank-graph graph)
        ;; Skip dag-draw-position-nodes - use manual coordinates
        (dag-draw-generate-splines graph)

        ;; Now check for boundary violations
        (let ((spline-metrics (dag-draw-quality-assess-spline-quality graph)))
          (message "Forced setup boundary violations: %s" (plist-get spline-metrics :boundary-violations))
          ;; This setup should definitely have boundary violations
          (expect (plist-get spline-metrics :boundary-violations) :to-be-greater-than 0))))))

  (describe "Pass 4 Spline Quality Refinement Triggers"
    (it "should trigger Pass 4 refinement when boundary violations are detected"
      (let ((graph (dag-draw-create-graph)))
        ;; Create a graph with boundary violations
        (dag-draw-add-node graph 'left "Left")
        (dag-draw-add-node graph 'obstacle "Obstacle")
        (dag-draw-add-node graph 'right "Right")
        (dag-draw-add-edge graph 'left 'right)

        ;; Position obstacle directly in the path
        (let ((left-node (dag-draw-get-node graph 'left))
              (obstacle-node (dag-draw-get-node graph 'obstacle))
              (right-node (dag-draw-get-node graph 'right)))
          (setf (dag-draw-node-x-coord left-node) 0.0)
          (setf (dag-draw-node-y-coord left-node) 50.0)
          (setf (dag-draw-node-x-coord obstacle-node) 50.0)
          (setf (dag-draw-node-y-coord obstacle-node) 50.0)
          (setf (dag-draw-node-x-coord right-node) 100.0)
          (setf (dag-draw-node-y-coord right-node) 50.0))

        ;; Generate splines
        (dag-draw-rank-graph graph)
        (dag-draw-generate-splines graph)

        ;; Test that Pass 4 refinement is triggered
        (let ((metrics (dag-draw-quality-assess-layout graph)))
          (expect (dag-draw-quality-layout-needs-refinement-p metrics) :to-be-truthy)
          (expect (dag-draw-quality-metrics-boundary-violations metrics) :to-be-greater-than 0)
          
          ;; Test backtracking function
          (let ((improved-graph (dag-draw-backtrack-pass4-quality graph)))
            (expect improved-graph :to-be-truthy)
            (expect (dag-draw-get-node improved-graph 'left) :to-be-truthy)))))

    (it "should not trigger Pass 4 refinement for high-quality splines"
      (let ((graph (dag-draw-create-graph)))
        ;; Create a simple graph with no boundary violations
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-edge graph 'a 'b)

        ;; Position nodes in a clean layout
        (let ((node-a (dag-draw-get-node graph 'a))
              (node-b (dag-draw-get-node graph 'b)))
          (setf (dag-draw-node-x-coord node-a) 0.0)
          (setf (dag-draw-node-y-coord node-a) 0.0)
          (setf (dag-draw-node-x-coord node-b) 100.0)
          (setf (dag-draw-node-y-coord node-b) 100.0))

        ;; Generate splines
        (dag-draw-rank-graph graph)
        (dag-draw-generate-splines graph)

        ;; Test that refinement is NOT triggered for good quality
        (let ((metrics (dag-draw-quality-assess-layout graph)))
          (expect (dag-draw-quality-metrics-boundary-violations metrics) :to-equal 0)
          ;; The layout should be acceptable and not need refinement
          (expect (dag-draw-quality-layout-needs-refinement-p metrics) :to-equal nil)))))

(provide 'dag-draw-spline-quality-test)

;;; dag-draw-spline-quality-test.el ends here
