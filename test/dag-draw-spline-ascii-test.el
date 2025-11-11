;;; dag-draw-spline-ascii-test.el --- Tests for spline-based ASCII rendering -*- lexical-binding: t -*-

;; Copyright (C) 2024, 2025

;;; Commentary:

;; TDD Iteration 13: Fix spline integration for curved routing in ASCII.
;; Test using spline data to create smoother ASCII edge routing instead of pure orthogonal.

;;; Code:

(add-to-list 'load-path (expand-file-name "test/helpers" (locate-dominating-file default-directory "Eldev")))

(require 'buttercup)
(require 'dag-draw-render)
(require 'dag-draw-pass4-splines)
(require 'dag-draw-test-harness)

(describe "dag-draw spline integration for ASCII"
  (describe "spline data utilization"
    (it "should use spline points when available for smoother routing"
      (let ((graph (dag-draw-create-graph)))
        ;; Create a graph that will generate splines
        (dag-draw-add-node graph 'A "Node A")
        (dag-draw-add-node graph 'B "Node B")
        (dag-draw-add-edge graph 'A 'B)
        (dag-draw-layout-graph graph)
        
        ;; Generate splines (this should populate edge spline data)
        (dag-draw-generate-splines graph)
        
        ;; Get the edge to verify spline data exists
        (let* ((edge (car (dag-draw-graph-edges graph)))
               (spline-points (dag-draw-edge-spline-points edge)))
          
          ;; Should have spline points generated
          (expect spline-points :not :to-be nil)
          (expect (length spline-points) :to-be-greater-than 2)
          
          ;; ASCII rendering should use this spline data for smoother paths
          (let ((result (dag-draw-render-ascii graph)))
            ;; Use test harness for validation
            (let ((connectivity-validation (dag-draw-test--validate-edge-connectivity result graph)))
              (expect (plist-get connectivity-validation :all-connected) :to-be t))
            (let ((arrow-validation (dag-draw-test--validate-arrows result)))
              (expect (plist-get arrow-validation :valid-arrows) :to-be-greater-than 0)))))))

  (describe "spline sampling for ASCII grid"
    (it "should sample spline curves appropriately for ASCII character grid"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'START "Start")
        (dag-draw-add-node graph 'END "End")
        (dag-draw-add-edge graph 'START 'END)
        (dag-draw-layout-graph graph)
        
        ;; Generate splines
        (dag-draw-generate-splines graph)
        
        ;; Verify the edge has spline data that could be used for ASCII
        (let* ((edge (car (dag-draw-graph-edges graph)))
               (spline-points (dag-draw-edge-spline-points edge)))
          ;; Should have adequate sample points for ASCII grid (optimized to avoid over-sampling)
          (expect (length spline-points) :to-be-greater-than 2)
          
          ;; Each point should have valid coordinates
          (dolist (point spline-points)
            (expect (numberp (dag-draw-point-x point)) :to-be t)
            (expect (numberp (dag-draw-point-y point)) :to-be t)))))))

(provide 'dag-draw-spline-ascii-test)

;;; dag-draw-spline-ascii-test.el ends here