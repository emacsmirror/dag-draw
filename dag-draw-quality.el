;;; dag-draw-quality.el --- Layout quality assessment for dag-draw -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Layout quality assessment and metrics for the GKNV algorithm implementation.
;; This module provides functions to evaluate layout quality across multiple
;; dimensions including edge crossings, node overlaps, spline aesthetics,
;; and space efficiency. These metrics drive the iterative refinement process.

;;; Code:

(require 'dash)
(require 'ht)
(require 'dag-draw-core)

;;; Quality Metrics Data Structure

(cl-defstruct (dag-draw-quality-metrics
               (:constructor dag-draw-quality-metrics-create)
               (:copier nil))
  "Quality metrics for a graph layout."
  (edge-crossings 0)           ; Number of edge crossings
  (node-overlaps 0)            ; Number of overlapping node pairs
  (boundary-violations 0)      ; Splines crossing node boundaries
  (total-edge-length 0.0)      ; Sum of all edge lengths
  (space-efficiency 0.0)       ; How well space is utilized (0-1)
  (aesthetic-score 0.0)        ; Overall aesthetic quality (0-1)
  (ascii-conflicts 0)          ; ASCII-specific character conflicts
  (convergence-score 0.0))     ; Convergence quality indicator (0-1)

;;; Node Overlap Detection

(defun dag-draw-quality-detect-node-overlaps (graph)
  "Detect overlapping nodes in GRAPH layout.
Returns list of overlapping node pairs as (node-id-1 node-id-2 overlap-area)."
  (let ((overlaps '())
        (nodes (ht-values (dag-draw-graph-nodes graph))))

    ;; Check every pair of nodes for overlap
    (dotimes (i (length nodes))
      (dotimes (j (1+ i))
        (when (< j (length nodes))
          (let ((node-a (nth i nodes))
                (node-b (nth j nodes)))
            (when (and node-a node-b (not (eq node-a node-b)))
              (let ((overlap-area (dag-draw-quality--calculate-overlap node-a node-b)))
                (when (> overlap-area 0)
                  (push (list (dag-draw-node-id node-a)
                             (dag-draw-node-id node-b)
                             overlap-area)
                        overlaps))))))))
    overlaps))

(defun dag-draw-quality--calculate-overlap (node-a node-b)
  "Calculate overlap area between two nodes. Returns 0 if no overlap."
  (let* ((a-x (or (dag-draw-node-x-coord node-a) 0))
         (a-y (or (dag-draw-node-y-coord node-a) 0))
         (a-w (dag-draw-node-x-size node-a))
         (a-h (dag-draw-node-y-size node-a))
         (b-x (or (dag-draw-node-x-coord node-b) 0))
         (b-y (or (dag-draw-node-y-coord node-b) 0))
         (b-w (dag-draw-node-x-size node-b))
         (b-h (dag-draw-node-y-size node-b))
         ;; Calculate overlap rectangle
         (left (max a-x b-x))
         (right (min (+ a-x a-w) (+ b-x b-w)))
         (top (max a-y b-y))
         (bottom (min (+ a-y a-h) (+ b-y b-h))))

    (if (and (< left right) (< top bottom))
        (* (- right left) (- bottom top))
      0)))

;;; Edge Crossing Detection

(defun dag-draw-quality-count-edge-crossings (graph)
  "Count the number of edge crossings in GRAPH layout.
Returns total number of crossing pairs."
  (let ((edges (dag-draw-graph-edges graph))
        (crossings 0))

    ;; Check every pair of edges for crossings
    (dotimes (i (length edges))
      (dotimes (j i)
        (let ((edge-a (nth i edges))
              (edge-b (nth j edges)))
          (when (dag-draw-quality--edges-cross-p graph edge-a edge-b)
            (setq crossings (1+ crossings))))))
    crossings))

(defun dag-draw-quality--edges-cross-p (graph edge-a edge-b)
  "Check if two edges cross each other."
  (let* ((from-a (dag-draw-get-node graph (dag-draw-edge-from-node edge-a)))
         (to-a (dag-draw-get-node graph (dag-draw-edge-to-node edge-a)))
         (from-b (dag-draw-get-node graph (dag-draw-edge-from-node edge-b)))
         (to-b (dag-draw-get-node graph (dag-draw-edge-to-node edge-b))))

    (when (and from-a to-a from-b to-b)
      ;; Don't count edges as crossing if they share a vertex (adjacent edges)
      (let ((share-vertex (or (eq (dag-draw-edge-from-node edge-a) (dag-draw-edge-from-node edge-b))
                              (eq (dag-draw-edge-from-node edge-a) (dag-draw-edge-to-node edge-b))
                              (eq (dag-draw-edge-to-node edge-a) (dag-draw-edge-from-node edge-b))
                              (eq (dag-draw-edge-to-node edge-a) (dag-draw-edge-to-node edge-b)))))
        (when (not share-vertex)
          ;; Only check geometric intersection for non-adjacent edges
          (dag-draw-quality--line-segments-intersect-p
           (dag-draw-quality--node-center from-a)
           (dag-draw-quality--node-center to-a)
           (dag-draw-quality--node-center from-b)
           (dag-draw-quality--node-center to-b)))))))

(defun dag-draw-quality--node-center (node)
  "Get center point of node."
  (let ((x (or (dag-draw-node-x-coord node) 0))
        (y (or (dag-draw-node-y-coord node) 0))
        (w (dag-draw-node-x-size node))
        (h (dag-draw-node-y-size node)))
    (cons (+ x (/ w 2.0)) (+ y (/ h 2.0)))))

(defun dag-draw-quality--line-segments-intersect-p (p1 q1 p2 q2)
  "Check if line segments (P1,Q1) and (P2,Q2) intersect."
  (let ((o1 (dag-draw-quality--orientation p1 q1 p2))
        (o2 (dag-draw-quality--orientation p1 q1 q2))
        (o3 (dag-draw-quality--orientation p2 q2 p1))
        (o4 (dag-draw-quality--orientation p2 q2 q1)))

    ;; General case: different orientations
    (or (and (/= o1 o2) (/= o3 o4))
        ;; Special cases: collinear points
        (and (= o1 0) (dag-draw-quality--point-on-segment p1 p2 q1))
        (and (= o2 0) (dag-draw-quality--point-on-segment p1 q2 q1))
        (and (= o3 0) (dag-draw-quality--point-on-segment p2 p1 q2))
        (and (= o4 0) (dag-draw-quality--point-on-segment p2 q1 q2)))))

(defun dag-draw-quality--orientation (p q r)
  "Find orientation of ordered triplet (P, Q, R).
Returns 0 if collinear, 1 if clockwise, 2 if counterclockwise."
  (let ((val (- (* (- (cdr q) (cdr p)) (- (car r) (car q)))
                (* (- (car q) (car p)) (- (cdr r) (cdr q))))))
    (cond ((= val 0) 0)           ; collinear
          ((> val 0) 1)           ; clockwise
          (t 2))))                ; counterclockwise

(defun dag-draw-quality--point-on-segment (p q r)
  "Check if point Q lies on line segment PR."
  (and (<= (car q) (max (car p) (car r)))
       (>= (car q) (min (car p) (car r)))
       (<= (cdr q) (max (cdr p) (cdr r)))
       (>= (cdr q) (min (cdr p) (cdr r)))))

;;; Spline Quality Assessment

(defun dag-draw-quality-assess-spline-quality (graph)
  "Assess quality of spline routing in GRAPH.
Returns metrics about boundary violations and routing aesthetics."
  (let ((boundary-violations 0)
        (total-spline-length 0.0)
        (aesthetic-score 0.0)
        (edge-count 0))

    (dolist (edge (dag-draw-graph-edges graph))
      (when (dag-draw-edge-spline-points edge)
        (setq edge-count (1+ edge-count))

        ;; Check for boundary violations
        (when (dag-draw-quality--spline-violates-boundaries-p graph edge)
          (setq boundary-violations (1+ boundary-violations)))

        ;; Calculate spline length
        (let ((length (dag-draw-quality--calculate-spline-length edge)))
          (setq total-spline-length (+ total-spline-length length)))

        ;; Assess aesthetic quality (smoothness, directness)
        (setq aesthetic-score (+ aesthetic-score
                                (dag-draw-quality--assess-spline-aesthetics edge)))))

    (list :boundary-violations boundary-violations
          :total-length total-spline-length
          :aesthetic-score (if (> edge-count 0) (/ aesthetic-score edge-count) 0))))

(defun dag-draw-quality--spline-violates-boundaries-p (graph edge)
  "Check if edge spline violates node boundaries.
GKNV Section 5.2: splines should be clipped to node boundaries, not cross through interiors."
  (let ((spline-points (dag-draw-edge-spline-points edge))
        (nodes (ht-values (dag-draw-graph-nodes graph)))
        (violations-found nil))

    (when spline-points
      ;; Check each spline point against all node boundaries
      (dolist (point spline-points)
        (dolist (node nodes)
          ;; Skip nodes that are endpoints of this edge
          (let ((from-node-id (dag-draw-edge-from-node edge))
                (to-node-id (dag-draw-edge-to-node edge))
                (node-id (dag-draw-node-id node)))
            (unless (or (eq node-id from-node-id) (eq node-id to-node-id))
              ;; Check if spline point is inside this node's boundary
              (when (dag-draw-quality--point-inside-node-p point node)
                (setq violations-found t))))))

    violations-found)))

(defun dag-draw-quality--point-inside-node-p (point node)
  "Check if POINT is inside NODE's boundary rectangle."
  (let ((px (dag-draw-point-x point))
        (py (dag-draw-point-y point))
        (nx (or (dag-draw-node-x-coord node) 0))
        (ny (or (dag-draw-node-y-coord node) 0))
        (nw (dag-draw-node-x-size node))
        (nh (dag-draw-node-y-size node)))

    ;; Point is inside if it's within the node's bounding box
    (and (>= px (- nx (/ nw 2.0)))           ; Left boundary
         (<= px (+ nx (/ nw 2.0)))           ; Right boundary
         (>= py (- ny (/ nh 2.0)))           ; Top boundary
         (<= py (+ ny (/ nh 2.0))))))         ; Bottom boundary

(defun dag-draw-quality--calculate-spline-length (edge)
  "Calculate total length of edge spline."
  (let ((points (dag-draw-edge-spline-points edge))
        (total-length 0.0))

    (when (and points (> (length points) 1))
      (dotimes (i (1- (length points)))
        (let ((p1 (nth i points))
              (p2 (nth (1+ i) points)))
          (when (and p1 p2)
            (let ((dx (- (dag-draw-point-x p2) (dag-draw-point-x p1)))
                  (dy (- (dag-draw-point-y p2) (dag-draw-point-y p1))))
              (setq total-length (+ total-length (sqrt (+ (* dx dx) (* dy dy))))))))))
    total-length))

(defun dag-draw-quality--assess-spline-aesthetics (edge)
  "Assess aesthetic quality of individual spline (0-1 score)."
  ;; Simplified aesthetic assessment - measures directness vs smoothness tradeoff
  (let ((points (dag-draw-edge-spline-points edge)))
    (if (and points (> (length points) 2))
        ;; Calculate ratio of direct distance to actual spline length
        (let* ((start (car points))
               (end (car (last points)))
               (direct-distance (sqrt (+ (expt (- (dag-draw-point-x end) (dag-draw-point-x start)) 2)
                                        (expt (- (dag-draw-point-y end) (dag-draw-point-y start)) 2))))
               (spline-length (dag-draw-quality--calculate-spline-length edge)))
          (if (> spline-length 0)
              (min 1.0 (/ direct-distance spline-length))
            0.0))
      1.0))) ; Single point or line gets perfect score

;;; Space Efficiency Assessment

(defun dag-draw-quality-calculate-space-efficiency (graph)
  "Calculate how efficiently the layout uses available space (0-1 score)."
  (let* ((nodes (ht-values (dag-draw-graph-nodes graph)))
         (total-node-area 0.0)
         (bounding-box-area 0.0))

    (when nodes
      ;; Calculate total area used by nodes
      (dolist (node nodes)
        (setq total-node-area (+ total-node-area
                                (* (dag-draw-node-x-size node)
                                   (dag-draw-node-y-size node)))))

      ;; Calculate bounding box area
      (let ((bounds (dag-draw-quality--calculate-layout-bounds graph)))
        (when bounds
          (setq bounding-box-area (* (nth 2 bounds) (nth 3 bounds)))))

      ;; Return ratio (higher is better space utilization)
      (if (> bounding-box-area 0)
          (/ total-node-area bounding-box-area)
        0.0))))

(defun dag-draw-quality--calculate-layout-bounds (graph)
  "Calculate bounding box of entire layout. Returns (min-x min-y width height)."
  (let ((nodes (ht-values (dag-draw-graph-nodes graph)))
        (min-x most-positive-fixnum)
        (min-y most-positive-fixnum)
        (max-x most-negative-fixnum)
        (max-y most-negative-fixnum))

    (dolist (node nodes)
      (let ((x (or (dag-draw-node-x-coord node) 0))
            (y (or (dag-draw-node-y-coord node) 0))
            (w (dag-draw-node-x-size node))
            (h (dag-draw-node-y-size node)))
        (setq min-x (min min-x x))
        (setq min-y (min min-y y))
        (setq max-x (max max-x (+ x w)))
        (setq max-y (max max-y (+ y h)))))

    (when (and (< min-x most-positive-fixnum)
               (< min-y most-positive-fixnum))
      (list min-x min-y (- max-x min-x) (- max-y min-y)))))

;;; ASCII-Specific Quality Assessment

(defun dag-draw-quality-assess-ascii-quality (graph ascii-grid scale)
  "Assess quality of ASCII rendering for GRAPH.
Returns metrics about character conflicts and text fitting."
  (let ((character-conflicts 0)
        (text-fitting-issues 0)
        (grid-efficiency 0.0))

    ;; Check for character-level conflicts in ASCII grid
    (when ascii-grid
      (setq character-conflicts (dag-draw-quality--count-ascii-conflicts ascii-grid)))

    ;; Check if node text fits properly in ASCII representation
    (ht-each (lambda (node-id node)
               (unless (dag-draw-quality--node-text-fits-ascii-p node scale)
                 (setq text-fitting-issues (1+ text-fitting-issues))))
             (dag-draw-graph-nodes graph))

    ;; Calculate grid space efficiency
    (when ascii-grid
      (setq grid-efficiency (dag-draw-quality--calculate-ascii-efficiency ascii-grid)))

    (list :character-conflicts character-conflicts
          :text-fitting-issues text-fitting-issues
          :grid-efficiency grid-efficiency)))

(defun dag-draw-quality--count-ascii-conflicts (grid)
  "Count character conflicts in ASCII grid."
  ;; Simplified implementation - would check for overlapping characters
  ;; that shouldn't be together (e.g., multiple different characters at same position)
  0)

(defun dag-draw-quality--node-text-fits-ascii-p (node scale)
  "Check if node text fits in ASCII representation at given SCALE."
  ;; Simplified check - compares text length to available ASCII space
  (let* ((text (dag-draw-node-label node))
         (available-width (* (dag-draw-node-x-size node) scale))
         (available-height (* (dag-draw-node-y-size node) scale)))
    (and (> available-width (length text))
         (> available-height 1))))

(defun dag-draw-quality--calculate-ascii-efficiency (grid)
  "Calculate efficiency of ASCII grid usage (0-1 score)."
  (when grid
    (let ((total-cells 0)
          (used-cells 0)
          (grid-height (length grid)))

      (dotimes (y grid-height)
        (let ((row (aref grid y))
              (grid-width (length row)))
          (dotimes (x grid-width)
            (setq total-cells (1+ total-cells))
            (unless (eq (aref row x) ?\s)
              (setq used-cells (1+ used-cells))))))

      (if (> total-cells 0)
          (/ (float used-cells) total-cells)
        0.0))))

;;; Comprehensive Quality Assessment

(defun dag-draw-quality-assess-layout (graph &optional ascii-grid scale)
  "Perform comprehensive quality assessment of GRAPH layout.
Optionally include ASCII-specific metrics if ASCII-GRID and SCALE provided."
  (let* ((overlaps (dag-draw-quality-detect-node-overlaps graph))
         (crossings (dag-draw-quality-count-edge-crossings graph))
         (spline-metrics (dag-draw-quality-assess-spline-quality graph))
         (space-efficiency (dag-draw-quality-calculate-space-efficiency graph))
         (ascii-metrics (when ascii-grid
                         (dag-draw-quality-assess-ascii-quality graph ascii-grid scale))))

    (dag-draw-quality-metrics-create
     :edge-crossings crossings
     :node-overlaps (length overlaps)
     :boundary-violations (plist-get spline-metrics :boundary-violations)
     :total-edge-length (plist-get spline-metrics :total-length)
     :space-efficiency space-efficiency
     :aesthetic-score (plist-get spline-metrics :aesthetic-score)
     :ascii-conflicts (if ascii-metrics (plist-get ascii-metrics :character-conflicts) 0)
     :convergence-score 0.0))) ; Will be set by iteration management

(defun dag-draw-quality-layout-needs-refinement-p (metrics &optional thresholds)
  "Determine if layout needs refinement based on quality METRICS.
THRESHOLDS is optional plist of quality thresholds."
  (let ((max-overlaps (or (plist-get thresholds :max-overlaps) 0))
        (max-crossings (or (plist-get thresholds :max-crossings) 10))
        (min-space-efficiency (or (plist-get thresholds :min-space-efficiency) 0.1))
        (min-aesthetic-score (or (plist-get thresholds :min-aesthetic-score) 0.3)))

    (or (> (dag-draw-quality-metrics-node-overlaps metrics) max-overlaps)
        (> (dag-draw-quality-metrics-edge-crossings metrics) max-crossings)
        (< (dag-draw-quality-metrics-space-efficiency metrics) min-space-efficiency)
        (< (dag-draw-quality-metrics-aesthetic-score metrics) min-aesthetic-score)
        (> (dag-draw-quality-metrics-boundary-violations metrics) 0)
        (> (dag-draw-quality-metrics-ascii-conflicts metrics) 0))))

(defun dag-draw-quality-compare-layouts (metrics-a metrics-b)
  "Compare two layout quality metrics. Returns :a-better, :b-better, or :equal."
  (let ((score-a (dag-draw-quality--calculate-overall-score metrics-a))
        (score-b (dag-draw-quality--calculate-overall-score metrics-b)))
    (cond ((> score-a score-b) :a-better)
          ((> score-b score-a) :b-better)
          (t :equal))))

(defun dag-draw-quality--calculate-overall-score (metrics)
  "Calculate overall quality score from metrics (higher is better)."
  ;; Weighted combination of various quality factors
  (- 1.0
     (* 0.3 (min 1.0 (/ (dag-draw-quality-metrics-edge-crossings metrics) 20.0)))
     (* 0.3 (min 1.0 (/ (dag-draw-quality-metrics-node-overlaps metrics) 5.0)))
     (* 0.2 (min 1.0 (/ (dag-draw-quality-metrics-boundary-violations metrics) 10.0)))
     (* 0.1 (min 1.0 (/ (dag-draw-quality-metrics-ascii-conflicts metrics) 5.0)))
     (* 0.1 (- 1.0 (dag-draw-quality-metrics-space-efficiency metrics)))))

(provide 'dag-draw-quality)

;;; dag-draw-quality.el ends here
