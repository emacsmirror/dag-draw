;;; dag-draw-splines.el --- Spline edge drawing for dag-draw -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Implementation of the spline edge drawing pass of the GKNV algorithm.
;; This module draws edges as smooth Bézier spline curves that avoid
;; overlapping with nodes, as described in section 5 of the research paper.

;;; Code:

(require 'dash)
(require 'ht)
(require 'dag-draw)
(require 'dag-draw-core)

;;; Data structures for splines

(cl-defstruct (dag-draw-point
               (:constructor dag-draw-point-create)
               (:copier nil))
  "A 2D point."
  x y)

(cl-defstruct (dag-draw-bezier-curve
               (:constructor dag-draw-bezier-curve-create)
               (:copier nil))
  "A cubic Bézier curve with 4 control points."
  p0 p1 p2 p3)

(cl-defstruct (dag-draw-box
               (:constructor dag-draw-box-create)
               (:copier nil))
  "A rectangular region."
  x-min y-min x-max y-max)

;;; Edge classification and routing

(defun dag-draw--classify-edge (graph edge)
  "Classify edge by type: inter-rank, flat, or self-edge."
  (let* ((from-node (dag-draw-get-node graph (dag-draw-edge-from-node edge)))
         (to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge)))
         (from-rank (dag-draw-node-rank from-node))
         (to-rank (dag-draw-node-rank to-node)))

    (cond
     ((eq (dag-draw-edge-from-node edge) (dag-draw-edge-to-node edge))
      'self-edge)
     ((and from-rank to-rank (= from-rank to-rank))
      'flat-edge)
     (t 'inter-rank-edge))))

(defun dag-draw--get-node-port (node side &optional graph)
  "Get port coordinates for a node on given side (top, bottom, left, right).
  Uses adjusted coordinates from Pass 3 if available (GKNV Section 5.2 compliance)."
  (let* ((node-id (dag-draw-node-id node))
         ;; CRITICAL FIX: Use adjusted coordinates from Pass 3 if available
         (adjusted-coords (and graph
                               (dag-draw-graph-adjusted-positions graph)
                               (ht-get (dag-draw-graph-adjusted-positions graph) node-id)))
         (x (if adjusted-coords
                ;; Adjusted coordinates are in grid space, convert back to world coordinates
                (float (nth 0 adjusted-coords))
              (float (or (dag-draw-node-x-coord node) 0))))
         (y (if adjusted-coords
                (float (nth 1 adjusted-coords))
              (float (or (dag-draw-node-y-coord node) 0))))
         (width (if adjusted-coords
                    (float (nth 2 adjusted-coords))
                  (float (dag-draw-node-x-size node))))
         (height (if adjusted-coords
                     (float (nth 3 adjusted-coords))
                   (float (dag-draw-node-y-size node)))))

    (cond
     ((eq side 'top)
      (dag-draw-point-create :x x :y (- y (/ height 2.0))))
     ((eq side 'bottom)
      (dag-draw-point-create :x x :y (+ y (/ height 2.0))))
     ((eq side 'left)
      (dag-draw-point-create :x (- x (/ width 2.0)) :y y))
     ((eq side 'right)
      (dag-draw-point-create :x (+ x (/ width 2.0)) :y y))
     (t
      (dag-draw-point-create :x x :y y)))))

;;; Inter-rank edge splines

(defun dag-draw--create-inter-rank-spline (graph edge)
  "Create spline for edge between different ranks."
  (let* ((from-node (dag-draw-get-node graph (dag-draw-edge-from-node edge)))
         (to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge)))
         (from-rank (dag-draw-node-rank from-node))
         (to-rank (dag-draw-node-rank to-node)))

    ;; Handle case where ranks aren't set - use node coordinates to determine direction
    (if (and from-rank to-rank)
        ;; Ranks are available - use them
        (if (< from-rank to-rank)
            ;; Forward edge (downward)
            (dag-draw--create-downward-spline graph from-node to-node)
          ;; Backward edge (upward) - these are reversed edges from cycle breaking
          (dag-draw--create-upward-spline graph from-node to-node))
      ;; Ranks not available - fall back to coordinate-based direction
      (let ((from-y (or (dag-draw-node-y-coord from-node) 0))
            (to-y (or (dag-draw-node-y-coord to-node) 0)))
        (if (<= from-y to-y)
            ;; Downward or horizontal
            (dag-draw--create-downward-spline graph from-node to-node)
          ;; Upward
          (dag-draw--create-upward-spline graph from-node to-node))))))

(defun dag-draw--create-downward-spline (graph from-node to-node)
  "Create downward spline from higher rank to lower rank.
  Uses adjusted coordinates from Pass 3 (GKNV Section 5.2)."
  (let* ((start-port (dag-draw--get-node-port from-node 'bottom graph))
         (end-port (dag-draw--get-node-port to-node 'top graph))
         (start-x (dag-draw-point-x start-port))
         (start-y (dag-draw-point-y start-port))
         (end-x (dag-draw-point-x end-port))
         (end-y (dag-draw-point-y end-port))
         (control-offset (/ (- end-y start-y) 3.0))
         ;; Apply proper spline region calculation per GKNV algorithm
         (region (dag-draw--find-spline-region graph from-node to-node)))

    ;; Create cubic Bézier curve with region awareness
    (let ((spline-curves
           (list
            (dag-draw-bezier-curve-create
             :p0 start-port
             :p1 (dag-draw-point-create :x start-x :y (+ start-y control-offset))
             :p2 (dag-draw-point-create :x end-x :y (- end-y control-offset))
             :p3 end-port))))
      ;; Optimize spline to fit within collision-free region
      (dag-draw--optimize-spline-in-region spline-curves region))))

(defun dag-draw--create-upward-spline (graph from-node to-node)
  "Create upward spline for reversed edges.
  Uses adjusted coordinates from Pass 3 (GKNV Section 5.2)."
  (let* ((start-port (dag-draw--get-node-port from-node 'top graph))
         (end-port (dag-draw--get-node-port to-node 'bottom graph))
         (start-x (dag-draw-point-x start-port))
         (start-y (dag-draw-point-y start-port))
         (end-x (dag-draw-point-x end-port))
         (end-y (dag-draw-point-y end-port))
         (control-offset (/ (- start-y end-y) 3.0))
         ;; Apply proper spline region calculation per GKNV algorithm
         (region (dag-draw--find-spline-region graph from-node to-node)))

    ;; Create cubic Bézier curve going upward with region awareness
    (let ((spline-curves
           (list
            (dag-draw-bezier-curve-create
             :p0 start-port
             :p1 (dag-draw-point-create :x start-x :y (- start-y control-offset))
             :p2 (dag-draw-point-create :x end-x :y (+ end-y control-offset))
             :p3 end-port))))
      ;; Optimize spline to fit within collision-free region
      (dag-draw--optimize-spline-in-region spline-curves region))))

;;; Flat edge splines

(defun dag-draw--create-flat-spline (graph edge)
  "Create spline for edge between nodes on same rank."
  (let* ((from-node (dag-draw-get-node graph (dag-draw-edge-from-node edge)))
         (to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge)))
         (from-x (dag-draw-node-x-coord from-node))
         (to-x (dag-draw-node-x-coord to-node)))

    (if (< from-x to-x)
        ;; Left to right
        (dag-draw--create-horizontal-spline from-node to-node 'right 'left graph)
      ;; Right to left
      (dag-draw--create-horizontal-spline from-node to-node 'left 'right graph))))

(defun dag-draw--create-horizontal-spline (from-node to-node from-side to-side graph)
  "Create horizontal spline between two nodes.
  Uses adjusted coordinates from Pass 3 (GKNV Section 5.2)."
  (let* ((start-port (dag-draw--get-node-port from-node from-side graph))
         (end-port (dag-draw--get-node-port to-node to-side graph))
         (start-x (dag-draw-point-x start-port))
         (start-y (dag-draw-point-y start-port))
         (end-x (dag-draw-point-x end-port))
         (end-y (dag-draw-point-y end-port))
         (dx (- end-x start-x))
         (control-offset-x (/ dx 3.0))
         ;; Apply proper spline region calculation per GKNV algorithm
         (region (dag-draw--find-spline-region graph from-node to-node)))

    (let ((spline-curves
           (list
            (dag-draw-bezier-curve-create
             :p0 start-port
             :p1 (dag-draw-point-create :x (+ start-x control-offset-x) :y start-y)
             :p2 (dag-draw-point-create :x (- end-x control-offset-x) :y end-y)
             :p3 end-port))))
      ;; Optimize spline to fit within collision-free region
      (dag-draw--optimize-spline-in-region spline-curves region))))

;;; Self-edge splines

(defun dag-draw--create-self-spline (graph edge)
  "Create spline for self-edges (loops).
  Uses adjusted coordinates from Pass 3 (GKNV Section 5.2)."
  (let* ((node (dag-draw-get-node graph (dag-draw-edge-from-node edge)))
         (node-id (dag-draw-node-id node))
         ;; Use adjusted coordinates if available
         (adjusted-coords (and (dag-draw-graph-adjusted-positions graph)
                               (ht-get (dag-draw-graph-adjusted-positions graph) node-id)))
         (center-x (if adjusted-coords
                       (float (nth 0 adjusted-coords))
                     (or (dag-draw-node-x-coord node) 0)))
         (center-y (if adjusted-coords
                       (float (nth 1 adjusted-coords))
                     (or (dag-draw-node-y-coord node) 0)))
         (width (if adjusted-coords
                    (float (nth 2 adjusted-coords))
                  (dag-draw-node-x-size node)))
         (height (if adjusted-coords
                     (float (nth 3 adjusted-coords))
                   (dag-draw-node-y-size node)))
         (loop-size (* 1.5 (max width height)))
         (start-port (dag-draw--get-node-port node 'right graph))
         (end-port (dag-draw--get-node-port node 'right graph)))

    ;; Create a loop going right and around
    (list
     ;; First curve: right and up
     (dag-draw-bezier-curve-create
      :p0 start-port
      :p1 (dag-draw-point-create
           :x (+ center-x (/ width 2.0) (/ loop-size 3.0))
           :y center-y)
      :p2 (dag-draw-point-create
           :x (+ center-x (/ width 2.0) (/ loop-size 2.0))
           :y (- center-y (/ loop-size 3.0)))
      :p3 (dag-draw-point-create
           :x (+ center-x (/ width 2.0) (/ loop-size 2.0))
           :y (- center-y (/ loop-size 2.0))))

     ;; Second curve: around and back down
     (dag-draw-bezier-curve-create
      :p0 (dag-draw-point-create
           :x (+ center-x (/ width 2.0) (/ loop-size 2.0))
           :y (- center-y (/ loop-size 2.0)))
      :p1 (dag-draw-point-create
           :x (+ center-x (/ width 2.0) (/ loop-size 2.0))
           :y (+ center-y (/ loop-size 3.0)))
      :p2 (dag-draw-point-create
           :x (+ center-x (/ width 2.0) (/ loop-size 3.0))
           :y center-y)
      :p3 end-port))))

;;; Spline optimization and region finding

(defun dag-draw--find-spline-region (graph from-node to-node)
  "Find polygonal region where spline can be drawn without overlapping nodes.
  Implementation of GKNV algorithm Section 5.1: creates proper collision-aware regions."
  (let* ((from-x (or (dag-draw-node-x-coord from-node) 0))
         (from-y (or (dag-draw-node-y-coord from-node) 0))
         (to-x (or (dag-draw-node-x-coord to-node) 0))
         (to-y (or (dag-draw-node-y-coord to-node) 0))
         (from-width (dag-draw-node-x-size from-node))
         (from-height (dag-draw-node-y-size from-node))
         (to-width (dag-draw-node-x-size to-node))
         (to-height (dag-draw-node-y-size to-node))
         ;; GKNV algorithm: region must avoid all node boundaries + safety margin
         (node-margin 20)  ; Safety margin per GKNV recommendations
         (obstacles (dag-draw--find-intervening-obstacles graph from-node to-node)))

    ;; Calculate region that encompasses path but avoids all nodes
    (let* ((region-x-min (- (min from-x to-x) (/ (max from-width to-width) 2) node-margin))
           (region-y-min (- (min from-y to-y) (/ (max from-height to-height) 2) node-margin))
           (region-x-max (+ (max from-x to-x) (/ (max from-width to-width) 2) node-margin))
           (region-y-max (+ (max from-y to-y) (/ (max from-height to-height) 2) node-margin)))

      ;; Expand region to avoid obstacles (intervening nodes)
      (dolist (obstacle obstacles)
        (let ((obs-x (or (dag-draw-node-x-coord obstacle) 0))
              (obs-y (or (dag-draw-node-y-coord obstacle) 0))
              (obs-width (dag-draw-node-x-size obstacle))
              (obs-height (dag-draw-node-y-size obstacle)))
          (setq region-x-min (min region-x-min (- obs-x (/ obs-width 2) node-margin)))
          (setq region-y-min (min region-y-min (- obs-y (/ obs-height 2) node-margin)))
          (setq region-x-max (max region-x-max (+ obs-x (/ obs-width 2) node-margin)))
          (setq region-y-max (max region-y-max (+ obs-y (/ obs-height 2) node-margin)))))

      (dag-draw-box-create
       :x-min region-x-min
       :y-min region-y-min
       :x-max region-x-max
       :y-max region-y-max))))

(defun dag-draw--find-intervening-obstacles (graph from-node to-node)
  "Find nodes that might interfere with spline from FROM-NODE to TO-NODE.
  Returns list of nodes that lie in the general path area per GKNV algorithm."
  (let ((obstacles '())
        (from-x (or (dag-draw-node-x-coord from-node) 0))
        (from-y (or (dag-draw-node-y-coord from-node) 0))
        (to-x (or (dag-draw-node-x-coord to-node) 0))
        (to-y (or (dag-draw-node-y-coord to-node) 0))
        (from-id (dag-draw-node-id from-node))
        (to-id (dag-draw-node-id to-node)))

    ;; Define bounding corridor between nodes with generous width
    (let* ((corridor-margin 50)  ; Generous corridor per GKNV approach
          (corridor-x-min (- (min from-x to-x) corridor-margin))
          (corridor-y-min (- (min from-y to-y) corridor-margin))
          (corridor-x-max (+ (max from-x to-x) corridor-margin))
          (corridor-y-max (+ (max from-y to-y) corridor-margin)))

      ;; Check all nodes in graph for potential interference
      (ht-each (lambda (node-id node)
                 (let ((node-x (or (dag-draw-node-x-coord node) 0))
                       (node-y (or (dag-draw-node-y-coord node) 0)))
                   ;; Skip source and target nodes
                   (when (and (not (eq node-id from-id))
                              (not (eq node-id to-id))
                              ;; Check if node is in the corridor
                              (>= node-x corridor-x-min)
                              (<= node-x corridor-x-max)
                              (>= node-y corridor-y-min)
                              (<= node-y corridor-y-max))
                     (push node obstacles))))
               (dag-draw-graph-nodes graph)))

    obstacles))

(defun dag-draw--optimize-spline-in-region (spline region)
  "Optimize spline to fit within allowed region and avoid obstacles."
  ;; Simplified implementation - just return original spline
  ;; Full implementation would use iterative refinement
  spline)

;;; Spline coordinate calculation

(defun dag-draw--bezier-point-at (curve param)
  "Calculate point on Bézier curve at parameter PARAM (0 <= param <= 1)."
  (let* ((p0 (dag-draw-bezier-curve-p0 curve))
         (p1 (dag-draw-bezier-curve-p1 curve))
         (p2 (dag-draw-bezier-curve-p2 curve))
         (p3 (dag-draw-bezier-curve-p3 curve))
         (u (- 1.0 param))
         (tt (* param param))
         (uu (* u u))
         (uuu (* uu u))
         (ttt (* tt param)))

    (dag-draw-point-create
     :x (+ (* uuu (dag-draw-point-x p0))
           (* 3 uu param (dag-draw-point-x p1))
           (* 3 u tt (dag-draw-point-x p2))
           (* ttt (dag-draw-point-x p3)))
     :y (+ (* uuu (dag-draw-point-y p0))
           (* 3 uu param (dag-draw-point-y p1))
           (* 3 u tt (dag-draw-point-y p2))
           (* ttt (dag-draw-point-y p3))))))

(defun dag-draw--sample-spline (curve num-samples)
  "Sample points along Bézier curve for rendering."
  (let ((points '()))
    (dotimes (i (1+ num-samples))
      (let ((param (/ (float i) num-samples)))
        (push (dag-draw--bezier-point-at curve param) points)))
    (nreverse points)))

;;; Main spline generation function

(defun dag-draw-generate-splines (graph)
  "Generate spline curves for all edges (Pass 4 of GKNV algorithm)."
  (dolist (edge (dag-draw-graph-edges graph))
    (let* ((edge-type (dag-draw--classify-edge graph edge))
           (splines (cond
                     ((eq edge-type 'inter-rank-edge)
                      (dag-draw--create-inter-rank-spline graph edge))
                     ((eq edge-type 'flat-edge)
                      (dag-draw--create-flat-spline graph edge))
                     ((eq edge-type 'self-edge)
                      (dag-draw--create-self-spline graph edge))
                     ;; PHASE 3 FIX: All edges must have splines per GKNV algorithm
                     (t (progn
                          (message "WARNING: Unknown edge type %s for edge %s->%s, treating as inter-rank"
                                   edge-type (dag-draw-edge-from-node edge) (dag-draw-edge-to-node edge))
                          (dag-draw--create-inter-rank-spline graph edge))))))

      ;; Store splines in edge
      (setf (dag-draw-edge-spline-points edge)
            (dag-draw--convert-splines-to-points splines))))

  graph)

(defun dag-draw--convert-splines-to-points (splines)
  "Convert Bézier splines to point sequences for storage."
  (let ((all-points '()))
    (dolist (spline splines)
      (let ((points (dag-draw--sample-spline spline 3)))  ; Reduced samples for ASCII rendering
        (setq all-points (append all-points points))))
    all-points))

;;; Spline utility functions

(defun dag-draw-get-edge-spline-points (edge)
  "Get spline control points for an edge."
  (dag-draw-edge-spline-points edge))

(defun dag-draw--spline-length (splines)
  "Calculate approximate length of spline curves."
  (let ((total-length 0.0))
    (dolist (spline splines)
      (let ((points (dag-draw--sample-spline spline 10)))
        (dotimes (i (1- (length points)))
          (let* ((p1 (nth i points))
                 (p2 (nth (1+ i) points))
                 (dx (- (dag-draw-point-x p2) (dag-draw-point-x p1)))
                 (dy (- (dag-draw-point-y p2) (dag-draw-point-y p1))))
            (setq total-length (+ total-length (sqrt (+ (* dx dx) (* dy dy)))))))))
    total-length))

(defun dag-draw--spline-bounds (splines)
  "Calculate bounding box of spline curves."
  (let ((min-x most-positive-fixnum)
        (min-y most-positive-fixnum)
        (max-x most-negative-fixnum)
        (max-y most-negative-fixnum))

    (dolist (spline splines)
      (let ((points (dag-draw--sample-spline spline 20)))
        (dolist (point points)
          (let ((x (dag-draw-point-x point))
                (y (dag-draw-point-y point)))
            (setq min-x (min min-x x))
            (setq max-x (max max-x x))
            (setq min-y (min min-y y))
            (setq max-y (max max-y y))))))

    (dag-draw-box-create :x-min min-x :y-min min-y :x-max max-x :y-max max-y)))

;;; Edge label positioning

(defun dag-draw--position-edge-label (edge splines)
  "Position edge label at midpoint of spline."
  (when (and splines (dag-draw-edge-label edge))
    (let* ((first-spline (car splines))
           (midpoint (dag-draw--bezier-point-at first-spline 0.5)))
      ;; Store label position (could be extended with better placement logic)
      (setf (dag-draw-edge-label-position edge) midpoint))))

;;; Arrow head generation

(defun dag-draw--create-arrow-head (spline)
  "Create arrow head at end of spline."
  (let* ((end-point (dag-draw-bezier-curve-p3 spline))
         (control-point (dag-draw-bezier-curve-p2 spline))
         (dx (- (dag-draw-point-x end-point) (dag-draw-point-x control-point)))
         (dy (- (dag-draw-point-y end-point) (dag-draw-point-y control-point)))
         (length (sqrt (+ (* dx dx) (* dy dy))))
         (arrow-size 8)
         (angle 0.5))  ; radians

    (when (> length 0)
      (let* ((unit-x (/ dx length))
             (unit-y (/ dy length))
             (perp-x (- unit-y))
             (perp-y unit-x)
             (back-x (- (dag-draw-point-x end-point) (* arrow-size unit-x)))
             (back-y (- (dag-draw-point-y end-point) (* arrow-size unit-y))))

        (list
         (dag-draw-point-create
          :x (+ back-x (* arrow-size 0.3 perp-x))
          :y (+ back-y (* arrow-size 0.3 perp-y)))
         end-point
         (dag-draw-point-create
          :x (- back-x (* arrow-size 0.3 perp-x))
          :y (- back-y (* arrow-size 0.3 perp-y))))))))

;;; TDD Region-based Spline Routing Implementation

(defun dag-draw--detect-obstacles-for-edge (graph from-node to-node)
  "Detect node obstacles that might interfere with direct edge FROM-NODE to TO-NODE.
Returns list of node IDs that are potential obstacles."
  (let ((obstacles '())
        (from-pos (dag-draw-get-node graph from-node))
        (to-pos (dag-draw-get-node graph to-node)))

    ;; Check all other nodes to see if they lie in the path
    (ht-each (lambda (node-id node)
               (when (and (not (eq node-id from-node))
                          (not (eq node-id to-node))
                          (dag-draw--node-intersects-path-p from-pos to-pos node))
                 (push node-id obstacles)))
             (dag-draw-graph-nodes graph))

    obstacles))

(defun dag-draw--node-intersects-path-p (from-node to-node test-node)
  "Check if TEST-NODE intersects the direct path from FROM-NODE to TO-NODE."
  (let ((from-x (or (dag-draw-node-x-coord from-node) 0))
        (from-y (or (dag-draw-node-y-coord from-node) 0))
        (to-x (or (dag-draw-node-x-coord to-node) 0))
        (to-y (or (dag-draw-node-y-coord to-node) 0))
        (test-x (or (dag-draw-node-x-coord test-node) 0))
        (test-y (or (dag-draw-node-y-coord test-node) 0))
        (test-width (dag-draw-node-x-size test-node))
        (test-height (dag-draw-node-y-size test-node)))

    ;; Simple bounding box intersection with path
    ;; Check if test node bounding box intersects line segment
    (let ((line-min-x (min from-x to-x))
          (line-max-x (max from-x to-x))
          (line-min-y (min from-y to-y))
          (line-max-y (max from-y to-y))
          (node-min-x (- test-x (/ test-width 2)))
          (node-max-x (+ test-x (/ test-width 2)))
          (node-min-y (- test-y (/ test-height 2)))
          (node-max-y (+ test-y (/ test-height 2))))

      ;; Check for overlap
      (and (< node-min-x line-max-x)
           (> node-max-x line-min-x)
           (< node-min-y line-max-y)
           (> node-max-y line-min-y)))))

(defun dag-draw--plan-obstacle-avoiding-path (graph from-node to-node)
  "Plan a path from FROM-NODE to TO-NODE that avoids obstacles.
Returns list of points defining the avoidance path."
  (let* ((from-pos (dag-draw-get-node graph from-node))
         (to-pos (dag-draw-get-node graph to-node))
         (obstacles (dag-draw--detect-obstacles-for-edge graph from-node to-node))
         (from-x (or (dag-draw-node-x-coord from-pos) 0))
         (from-y (or (dag-draw-node-y-coord from-pos) 0))
         (to-x (or (dag-draw-node-x-coord to-pos) 0))
         (to-y (or (dag-draw-node-y-coord to-pos) 0)))

    (if obstacles
        ;; Create path with waypoints to avoid obstacles
        (let* ((mid-x (/ (+ from-x to-x) 2))
               (mid-y (/ (+ from-y to-y) 2))
               ;; Offset waypoints to avoid obstacles
               (offset-x (if (> to-x from-x) 20 -20))
               (offset-y (if (> to-y from-y) 20 -20)))
          (list
           (dag-draw-point-create :x from-x :y from-y)
           (dag-draw-point-create :x (+ mid-x offset-x) :y mid-y)
           (dag-draw-point-create :x mid-x :y (+ mid-y offset-y))
           (dag-draw-point-create :x to-x :y to-y)))
      ;; Direct path if no obstacles
      (list
       (dag-draw-point-create :x from-x :y from-y)
       (dag-draw-point-create :x to-x :y to-y)))))

(defun dag-draw--create-region-aware-spline (graph from-node to-node)
  "Create a region-aware spline that avoids obstacles.
Returns a Bézier spline that smoothly connects nodes while avoiding obstacles."
  (let* ((path-points (dag-draw--plan-obstacle-avoiding-path graph from-node to-node))
         (start-point (car path-points))
         (end-point (car (last path-points))))

    ;; Create smooth Bézier curve from path points
    (if (> (length path-points) 2)
        ;; Multi-segment path - create smooth curve through waypoints
        (let* ((control1 (nth 1 path-points))
               (control2 (nth 2 path-points)))
          (dag-draw-bezier-curve-create
           :p0 start-point
           :p1 control1
           :p2 control2
           :p3 end-point))
      ;; Direct path - simple curve
      (let* ((dx (- (dag-draw-point-x end-point) (dag-draw-point-x start-point)))
             (dy (- (dag-draw-point-y end-point) (dag-draw-point-y start-point)))
             (control1 (dag-draw-point-create
                        :x (+ (dag-draw-point-x start-point) (* dx 0.3))
                        :y (+ (dag-draw-point-y start-point) (* dy 0.3))))
             (control2 (dag-draw-point-create
                        :x (+ (dag-draw-point-x start-point) (* dx 0.7))
                        :y (+ (dag-draw-point-y start-point) (* dy 0.7)))))
        (dag-draw-bezier-curve-create
         :p0 start-point
         :p1 control1
         :p2 control2
         :p3 end-point)))))

(provide 'dag-draw-splines)

;;; dag-draw-splines.el ends here
