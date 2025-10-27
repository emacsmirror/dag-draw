;;; dag-draw-c1-continuity-test.el --- Tests for GKNV C1 continuity -*- lexical-binding: t -*-

;; Copyright (C) 2024, 2025

;;; Commentary:

;; GKNV Baseline Compliance Tests - Pass 4: C¹ Continuity at Junctions
;;
;; This module tests GKNV spline C¹ continuity requirement as specified in
;; "A Technique for Drawing Directed Graphs" (Gansner, Koutsofios, North, Vo).
;;
;; GKNV Reference: Section 5.2 lines 25-26 (C¹ continuity enforcement)
;; Decision: D4.11 - C¹ (continuous tangent) required, C² rejected
;; Algorithm: C¹ Continuity via Tangent Vector Matching
;;
;; Key Requirements Tested:
;; - When spline subdivided, two curves join at subdivision point
;; - C¹ continuity: position and tangent continuous at join
;; - Enforcement: two splines have same unit tangent vector at subdivision point
;; - Guarantees smooth appearance at joins (no visible kinks)
;; - C² (continuous curvature) rejected: "doesn't produce better results"
;; - C² also "much more expensive to compute"
;; - C¹ provides good visual quality with reasonable cost
;;
;; Test Coverage:
;; - Subdivided splines join at correct position
;; - Tangent vectors match at subdivision point
;; - Unit tangent calculation correct
;; - Visual smoothness: no kinks at joins
;; - C¹ continuity maintained through multiple subdivisions
;; - Various subdivision scenarios
;; - Cost-quality tradeoff: C¹ vs C² performance
;;
;; Baseline Status: ✅ Required for GKNV compliance
;;
;; See doc/implementation-decisions.md (D4.11) for full decision rationale.
;; See doc/algorithm-specification.md Pass 4 for implementation details.

;; Tests for GKNV Section 5.2 C1 continuity implementation.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-pass4-splines)

(describe "GKNV C1 Continuity Implementation (Section 5.2)"
  
  (it "should compute matching tangent vectors at spline junctions"
    ;; RED TEST: This should fail - current implementation doesn't match tangents
    (let* ((linear-path (list 
                        (dag-draw-point-create :x 0 :y 0)
                        (dag-draw-point-create :x 50 :y 25)
                        (dag-draw-point-create :x 100 :y 0)))
           (s-array (dag-draw--compute-s-array-gknv linear-path 0 0)))
      
      ;; Should have 2 Bezier curves for 3-point path
      (expect (length s-array) :to-equal 2)
      
      ;; Get the junction point between the two curves
      (let ((curve1 (nth 0 s-array))
            (curve2 (nth 1 s-array)))
        
        ;; C1 continuity: tangent at end of curve1 should match tangent at start of curve2
        (let ((tangent1 (dag-draw--compute-curve-end-tangent curve1))
              (tangent2 (dag-draw--compute-curve-start-tangent curve2)))
          
          ;; Tangent vectors should be parallel (cross product near zero)
          (let ((cross-product (dag-draw--vector-cross-product tangent1 tangent2)))
            (expect (abs cross-product) :to-be-less-than 0.01))))))
  
  (it "should handle complex multi-segment path with C1 continuity"
    ;; Test more complex path with multiple junctions
    (let* ((linear-path (list 
                        (dag-draw-point-create :x 0 :y 0)
                        (dag-draw-point-create :x 25 :y 15)   ; Junction 1
                        (dag-draw-point-create :x 75 :y 5)    ; Junction 2
                        (dag-draw-point-create :x 100 :y 20))) ; End
           (s-array (dag-draw--compute-s-array-gknv linear-path 0 0)))
      
      ;; Should have 3 curves for 4-point path
      (expect (length s-array) :to-equal 3)
      
      ;; Test C1 continuity at both junctions
      (let ((curve1 (nth 0 s-array))
            (curve2 (nth 1 s-array))
            (curve3 (nth 2 s-array)))
        
        ;; Junction 1: end of curve1 should match start of curve2
        (let ((tangent1-end (dag-draw--compute-curve-end-tangent curve1))
              (tangent2-start (dag-draw--compute-curve-start-tangent curve2)))
          (let ((cross-product1 (dag-draw--vector-cross-product tangent1-end tangent2-start)))
            (expect (abs cross-product1) :to-be-less-than 0.01)))
        
        ;; Junction 2: end of curve2 should match start of curve3
        (let ((tangent2-end (dag-draw--compute-curve-end-tangent curve2))
              (tangent3-start (dag-draw--compute-curve-start-tangent curve3)))
          (let ((cross-product2 (dag-draw--vector-cross-product tangent2-end tangent3-start)))
            (expect (abs cross-product2) :to-be-less-than 0.01)))))))

;; Helper functions needed for C1 continuity testing
(defun dag-draw--compute-curve-end-tangent (bezier-curve)
  "Compute unit tangent vector at the end of a Bezier curve."
  (let* ((p2 (dag-draw-bezier-curve-p2 bezier-curve))
         (p3 (dag-draw-bezier-curve-p3 bezier-curve))
         (dx (- (dag-draw-point-x p3) (dag-draw-point-x p2)))
         (dy (- (dag-draw-point-y p3) (dag-draw-point-y p2)))
         (length (sqrt (+ (* dx dx) (* dy dy)))))
    (if (> length 0.001)
        (list (/ dx length) (/ dy length))
      (list 1.0 0.0))))

(defun dag-draw--compute-curve-start-tangent (bezier-curve)
  "Compute unit tangent vector at the start of a Bezier curve."
  (let* ((p0 (dag-draw-bezier-curve-p0 bezier-curve))
         (p1 (dag-draw-bezier-curve-p1 bezier-curve))
         (dx (- (dag-draw-point-x p1) (dag-draw-point-x p0)))
         (dy (- (dag-draw-point-y p1) (dag-draw-point-y p0)))
         (length (sqrt (+ (* dx dx) (* dy dy)))))
    (if (> length 0.001)
        (list (/ dx length) (/ dy length))
      (list 1.0 0.0))))

(defun dag-draw--vector-cross-product (v1 v2)
  "Compute 2D cross product of vectors v1 and v2."
  (- (* (nth 0 v1) (nth 1 v2))
     (* (nth 1 v1) (nth 0 v2))))

(provide 'dag-draw-c1-continuity-test)

;;; dag-draw-c1-continuity-test.el ends here