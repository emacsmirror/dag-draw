;;; dag-draw-coordinate-positioning-test.el --- TDD tests for GKNV coordinate positioning -*- lexical-binding: t -*-

;; Copyright (C) 2024, 2025

;;; Commentary:

;; TDD Implementation of GKNV coordinate positioning algorithm (Pass 3).
;; This assigns actual X,Y coordinates while respecting node separation constraints.

;;; Code:

(require 'buttercup)
(require 'dag-draw-pass3-positioning)
(require 'cl-lib)

(describe "GKNV coordinate positioning with separation constraints"
  (describe "node separation constraint handling"
    (it "should maintain minimum separation between adjacent nodes"
      ;; RED phase: This test will fail because enhanced separation logic doesn't exist yet
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")

        ;; Apply full GKNV layout pipeline with ASCII coordinate mode
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)

        ;; Check that nodes maintain minimum separation (regardless of specific order)
        (let ((x-a (dag-draw-node-x-coord (dag-draw-get-node graph 'a)))
              (x-b (dag-draw-node-x-coord (dag-draw-get-node graph 'b)))
              (x-c (dag-draw-node-x-coord (dag-draw-get-node graph 'c))))
          (expect (and x-a x-b x-c (numberp x-a) (numberp x-b) (numberp x-c)) :to-be t)  ; All coordinates assigned

          ;; Get positions in sorted order for separation checking  
          (let ((positions (sort (list x-a x-b x-c) '<)))
            (let ((left (nth 0 positions))
                  (middle (nth 1 positions))
                  (right (nth 2 positions)))
              ;; Get node widths for GKNV formula: ρ(a,b) = (xsize(a) + xsize(b))/2 + nodesep(G)
              (let* ((nodes (list (dag-draw-get-node graph 'a)
                                  (dag-draw-get-node graph 'b)
                                  (dag-draw-get-node graph 'c)))
                     (left-node (cl-find-if (lambda (n) (= (dag-draw-node-x-coord n) left)) nodes))
                     (middle-node (cl-find-if (lambda (n) (= (dag-draw-node-x-coord n) middle)) nodes))
                     (right-node (cl-find-if (lambda (n) (= (dag-draw-node-x-coord n) right)) nodes))
                     (left-width (dag-draw-node-x-size left-node))
                     (middle-width (dag-draw-node-x-size middle-node))
                     (right-width (dag-draw-node-x-size right-node))
                     (nodesep (dag-draw-graph-node-separation graph))
                     ;; GKNV separation constraints
                     (min-left-middle-sep (+ (/ (+ left-width middle-width) 2.0) nodesep))
                     (min-middle-right-sep (+ (/ (+ middle-width right-width) 2.0) nodesep)))
                ;; Check GKNV separation constraints between adjacent nodes
                (expect (>= (- middle left) min-left-middle-sep) :to-be t)
                (expect (>= (- right middle) min-middle-right-sep) :to-be t)))))))))

(provide 'dag-draw-coordinate-positioning-test)

;;; dag-draw-coordinate-positioning-test.el ends here
