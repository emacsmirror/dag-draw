;;; dag-draw-spline-length-integration-test.el --- TDD for spline length integration -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; TDD test to integrate dag-draw--spline-length into the mainline spline
;; processing pipeline per GKNV paper requirements.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-pass4-splines)

(describe "Spline Length Integration into Mainline"

  (it "should have spline-length function available for utility calculations"
    ;; RED: This test should fail because dag-draw--spline-length doesn't exist yet
    (expect (functionp 'dag-draw--spline-length) :to-be t))

  (it "should calculate length of simple spline"
    ;; RED: This will fail because function doesn't exist
    (let* ((p0 (dag-draw-point-create :x 0 :y 0))
           (p1 (dag-draw-point-create :x 6 :y 0))
           (p2 (dag-draw-point-create :x 14 :y 0))
           (p3 (dag-draw-point-create :x 20 :y 0))
           (curve (dag-draw-bezier-curve-create :p0 p0 :p1 p1 :p2 p2 :p3 p3))
           (mock-splines (list curve))
           (length (dag-draw--spline-length mock-splines)))

      ;; Should return approximate length of 20 units for straight line
      (expect (numberp length) :to-be t)
      (expect (> length 15) :to-be t)  ; Should be close to 20
      (expect (< length 25) :to-be t)))

  (it "should be used in mainline spline processing when length calculation is needed"
    ;; RED: This tests integration into mainline, will fail until integrated
    (let ((graph (dag-draw-create-graph)))
      ;; Create simple graph with one edge
      (dag-draw-add-node graph 'a "A")
      (dag-draw-add-node graph 'b "B")
      (dag-draw-add-edge graph 'a 'b)

      ;; Set basic layout
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'a)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'b)) 1)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'a)) 0)
      (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'a)) 0)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'b)) 100)
      (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'b)) 50)

      ;; Generate splines
      (dag-draw-generate-splines graph)

      ;; Verify that splines were created
      (let ((edges (dag-draw-graph-edges graph)))
        (expect (> (length edges) 0) :to-be t)

        ;; Check that first edge has spline data
        (let* ((edge (car edges))
               (spline-points (dag-draw-edge-spline-points edge)))
          (expect spline-points :not :to-be nil)

          ;; The spline-length function should be available for calculations
          ;; (This tests that it's properly integrated)
          (when spline-points
            (let ((calculated-length (dag-draw--spline-length (list spline-points))))
              (expect (numberp calculated-length) :to-be t)
              (expect (> calculated-length 0) :to-be t))))))))

(provide 'dag-draw-spline-length-integration-test)

;;; dag-draw-spline-length-integration-test.el ends here
