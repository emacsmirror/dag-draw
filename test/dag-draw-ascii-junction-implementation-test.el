;;; dag-draw-ascii-junction-implementation-test.el --- TDD tests for junction character implementation -*- lexical-binding: t -*-

;; Copyright (C) 2025

;;; Commentary:
;; Test-driven development for ASCII junction character implementation
;; following decisions D5.1-D5.8 from doc/implementation-decisions.md

;;; Code:

(require 'buttercup)
(require 'dag-draw-ascii-edges)
(require 'dag-draw-ascii-grid)
(require 'dag-draw-core)

(describe "D5.8: Junction Character Context Selection"
  (describe "Context plist usage"
    (it "should accept a context plist with :type key"
      (let ((context (list :type 'direction-change
                          :from-direction 'right
                          :to-direction 'down)))
        (let ((result (dag-draw--get-enhanced-junction-char context)))
          (expect result :to-equal ?┐))))

    (it "should return T-junction for port-start with downward direction"
      (let ((context (list :type 'port-start :direction 'down)))
        (let ((result (dag-draw--get-enhanced-junction-char context)))
          (expect result :to-equal ?┬))))

    (it "should return corner for right-to-down direction change"
      (let ((context (list :type 'direction-change
                          :from-direction 'right
                          :to-direction 'down)))
        (let ((result (dag-draw--get-enhanced-junction-char context)))
          (expect result :to-equal ?┐))))

    (it "should return cross for edge-cross type"
      (let ((context (list :type 'edge-cross)))
        (let ((result (dag-draw--get-enhanced-junction-char context)))
          (expect result :to-equal ?┼))))))

(describe "D5.1: Port Boundary Junction Detection"
  (describe "Port junction detection framework"
    (it "should detect port junctions for a simple edge"
      (let* ((graph (dag-draw-create-graph))
             (node-a (dag-draw-add-node graph 'a "A"))
             (node-b (dag-draw-add-node graph 'b "B"))
             (edge (dag-draw-add-edge graph 'a 'b)))

        ;; Manually set coordinates to simulate after-layout state
        (setf (dag-draw-node-x-coord node-a) 10.0)
        (setf (dag-draw-node-y-coord node-a) 10.0)
        (setf (dag-draw-node-x-coord node-b) 10.0)
        (setf (dag-draw-node-y-coord node-b) 30.0)

        (let ((junctions (dag-draw--detect-port-junctions graph)))
          (expect (length junctions) :to-equal 2)

          ;; Should have one port-start and one port-end
          (let ((types (mapcar (lambda (j) (plist-get j :type)) junctions)))
            (expect types :to-have-same-items-as '(port-start port-end))))))))

(describe "Priority 1: Function Call Signature Fix"
  (describe "Local junction context analysis"
    (it "should detect vertical line with right branch (T-junction ├)"
      (let ((grid (dag-draw--create-ascii-grid 10 10)))
        ;; Set up a T-junction: vertical line with right branch
        ;; Vertical line at x=5
        (dotimes (y 10)
          (aset (aref grid y) 5 ?│))
        ;; Horizontal branch to the right from (5,5)
        (dotimes (x 5)
          (aset (aref grid 5) (+ 5 x) ?─))

        ;; At position (5,5) we should detect a T-junction with vertical main, right branch
        (let ((context (dag-draw--analyze-local-grid-junction-context grid 5 5 ?│ ?─)))
          (expect (plist-get context :type) :to-equal 't-junction)
          (expect (plist-get context :main-direction) :to-be 'down)
          (expect (plist-get context :branch-direction) :to-be 'right))))

    (it "should detect right-to-down corner (┐)"
      (let ((grid (dag-draw--create-ascii-grid 10 10)))
        ;; Horizontal line from left
        (dotimes (x 6)
          (aset (aref grid 5) x ?─))
        ;; Vertical line going down
        (dotimes (y 5)
          (aset (aref grid (+ 5 y)) 5 ?│))

        ;; At position (5,5) we should detect a corner
        (let ((context (dag-draw--analyze-local-grid-junction-context grid 5 5 ?─ ?│)))
          (expect (plist-get context :type) :to-equal 'direction-change)
          (expect (plist-get context :from-direction) :to-be 'right)
          (expect (plist-get context :to-direction) :to-be 'down))))

    (it "should detect edge crossing (┼)"
      (let ((grid (dag-draw--create-ascii-grid 10 10)))
        ;; Horizontal line through (5,5)
        (dotimes (x 10)
          (aset (aref grid 5) x ?─))
        ;; Vertical line through (5,5)
        (dotimes (y 10)
          (aset (aref grid y) 5 ?│))

        ;; At position (5,5) we should detect a cross
        (let ((context (dag-draw--analyze-local-grid-junction-context grid 5 5 ?─ ?│)))
          (expect (plist-get context :type) :to-equal 'edge-cross))))))

(provide 'dag-draw-ascii-junction-implementation-test)
;;; dag-draw-ascii-junction-implementation-test.el ends here
