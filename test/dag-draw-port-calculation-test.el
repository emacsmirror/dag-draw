;;; dag-draw-port-calculation-test.el --- Tests for port calculation improvements -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; TDD for improving port calculation to get precise edge attachment points.
;; Following true TDD: write one failing test, make it pass, refactor, repeat.

;;; Code:

(require 'buttercup)
(require 'dag-draw-render)

(describe
 "dag-draw port calculation"
 (describe
  "edge endpoint calculation"
  (it "should calculate different coordinates for different node edges"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'A "Node A")
        (dag-draw-add-node graph 'B "Node B")
        (dag-draw-add-edge graph 'A 'B)
        (dag-draw-layout-graph graph)

        ;; Get the edge connection points
        (let* ((edge (car (dag-draw-graph-edges graph)))
               (from-node (dag-draw-get-node graph (dag-draw-edge-from-node edge)))
               (to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge)))
               (connection-points (dag-draw--get-edge-connection-points graph edge)))

          ;; Debug: show node coordinates
          (message "From node coords: (%.1f, %.1f) size: (%.1f, %.1f)"
                   (dag-draw-node-x-coord from-node) (dag-draw-node-y-coord from-node)
                   (dag-draw-node-x-size from-node) (dag-draw-node-y-size from-node))
          (message "To node coords: (%.1f, %.1f) size: (%.1f, %.1f)"
                   (dag-draw-node-x-coord to-node) (dag-draw-node-y-coord to-node)
                   (dag-draw-node-x-size to-node) (dag-draw-node-y-size to-node))
          ;; Should have exactly 2 connection points
          (expect (length connection-points) :to-equal 2)

          ;; The two points should be different (not both at same location)
          (let ((from-port (car connection-points))
                (to-port (cadr connection-points)))
            ;; Debug: show the actual coordinates
            (message "From port: (%.1f, %.1f)"
                     (dag-draw-point-x from-port) (dag-draw-point-y from-port))
            (message "To port: (%.1f, %.1f)"
                     (dag-draw-point-x to-port) (dag-draw-point-y to-port))

            ;; With improved compact layout, node height is 14, so bottom port is at y=7
            ;; This is actually correct - we should test that the port calculation works logically
            (expect (dag-draw-point-y from-port) :to-equal 7.0)   ; Bottom of first node (height/2 = 7)
            (expect (dag-draw-point-y to-port) :to-equal 18.0)    ; Top of second node (25 - height/2 = 18)
            (expect (dag-draw-point-x from-port) :to-equal 0.0)   ; Same x for vertical layout
            (expect (dag-draw-point-x to-port) :to-equal 0.0)))))

  (it "should select appropriate sides for horizontally separated nodes"
      ;; GKNV Section 5.1.1: "route the spline to the appropriate side of the node"
      ;; Test behavioral requirements, not exact coordinate calculations
      (let ((graph (dag-draw-create-graph)))
        ;; Create nodes with specific positioning to ensure horizontal separation
        (dag-draw-add-node graph 'LEFT "Left")
        (dag-draw-add-node graph 'RIGHT "Right")
        (dag-draw-add-edge graph 'LEFT 'RIGHT)
        (dag-draw-layout-graph graph)

        ;; Manually adjust node positions to ensure horizontal separation
        (let* ((left-node (dag-draw-get-node graph 'LEFT))
               (right-node (dag-draw-get-node graph 'RIGHT)))
          (setf (dag-draw-node-x-coord left-node) 0.0)
          (setf (dag-draw-node-y-coord left-node) 0.0)
          (setf (dag-draw-node-x-coord right-node) 100.0)  ; Ensure horizontal separation
          (setf (dag-draw-node-y-coord right-node) 0.0)

          ;; Now test port calculation - verify GKNV behavioral requirements
          (let* ((edge (car (dag-draw-graph-edges graph)))
                 (connection-points (dag-draw--get-edge-connection-points graph edge))
                 (from-port (car connection-points))
                 (to-port (cadr connection-points))
                 (left-center-x (dag-draw-node-x-coord left-node))
                 (right-center-x (dag-draw-node-x-coord right-node)))

            ;; GKNV-compliant expectations: verify appropriate side selection
            ;; For left-to-right horizontal edge:
            ;; - Left node should use RIGHT side (port-x > center-x)
            ;; - Right node should use LEFT side (port-x < center-x)
            (expect (dag-draw-point-x from-port) :to-be-greater-than left-center-x)   ; Right side of left node
            (expect (dag-draw-point-x to-port) :to-be-less-than right-center-x)       ; Left side of right node
            (expect (dag-draw-point-y from-port) :to-equal (dag-draw-point-y to-port)) ; Same y level for horizontal
            
            ;; Verify ports are within reasonable bounds (not at exact node centers)
            ;; This ensures actual side attachment rather than center attachment
            (expect (abs (- (dag-draw-point-x from-port) left-center-x)) :to-be-greater-than 10.0)
            (expect (abs (- (dag-draw-point-x to-port) right-center-x)) :to-be-greater-than 10.0)))))))

(provide 'dag-draw-port-calculation-test)

;;; dag-draw-port-calculation-test.el ends here
