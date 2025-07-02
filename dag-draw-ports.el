;;; dag-draw-ports.el --- Port calculation logic for dag-draw -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Port calculation and edge connection logic for dag-draw graphs.
;; This module handles determining where edges should connect to nodes,
;; supporting both simple port calculation and distributed multi-edge layouts.

;;; Code:

(require 'dash)
(require 'dag-draw-core)
(require 'dag-draw-ascii-grid)

;;; Node Port Integration Functions

(defun dag-draw--get-node-port-grid (node side min-x min-y scale &optional graph)
  "Get port coordinates for a node on given side, accounting for grid rounding.
This function calculates ports based on actual grid positions after coordinate
conversion and rounding, ensuring precise alignment with rendered boxes.
If GRAPH is provided and contains adjusted positions, uses those coordinates."
  (let* ((node-id (dag-draw-node-id node))
         ;; Check if node has manually set coordinates
         (manual-x (dag-draw-node-x-coord node))
         (manual-y (dag-draw-node-y-coord node))
         (has-manual-coords (and manual-x manual-y))
         ;; Always prioritize adjusted coordinates when available
         ;; The adjusted-positions contain the final collision-resolved positions
         (adjusted-positions-table (and graph (dag-draw-graph-adjusted-positions graph)))
         (adjusted-coords (and adjusted-positions-table
                               (ht-get adjusted-positions-table node-id)))
         (x (if adjusted-coords
                ;; Adjusted coordinates are already in grid space: (x y width height)
                (float (nth 0 adjusted-coords))
              ;; Convert manual world coordinates to grid coordinates
              (dag-draw--world-to-grid-coord (or manual-x 0) min-x scale)))
         (y (if adjusted-coords
                (float (nth 1 adjusted-coords))
              ;; Convert manual world coordinates to grid coordinates
              (dag-draw--world-to-grid-coord (or manual-y 0) min-y scale)))
         (width (if adjusted-coords
                    (float (nth 2 adjusted-coords))
                  (dag-draw--world-to-grid-size (dag-draw-node-x-size node) scale)))
         (height (if adjusted-coords
                     (float (nth 3 adjusted-coords))
                   (dag-draw--world-to-grid-size (dag-draw-node-y-size node) scale)))
         ;; Both paths now use converted coordinates
         (grid-center-x (+ x (/ width 2.0)))
         (grid-center-y (+ y (/ height 2.0)))
         (grid-width width)
         (grid-height height)
         ;; Use unified coordinate system consistently
         (grid-x (round (- grid-center-x (/ grid-width 2))))
         (grid-y (round (- grid-center-y (/ grid-height 2))))
         (grid-x-end (+ grid-x grid-width -1))
         (grid-y-end (+ grid-y grid-height -1))
         ;; Ensure all coordinates are integer-quantized
         (actual-center-x (round grid-center-x))
         (actual-center-y (round grid-center-y)))

    ;; Port calculation using coordinate priority: adjusted > manual > default

    ;; Return grid coordinates directly to avoid double conversion
    (let ((result-port
           (cond
            ((eq side 'top)
             (dag-draw-point-create :x actual-center-x :y (- grid-y 1)))  ; OUTSIDE box, above it
            ((eq side 'bottom)
             (dag-draw-point-create :x actual-center-x :y (+ grid-y-end 1)))  ; OUTSIDE box, below it
            ((eq side 'left)
             (dag-draw-point-create :x (- grid-x 1) :y (+ grid-y 1)))  ; OUTSIDE box, left of it
            ((eq side 'right)
             (dag-draw-point-create :x (+ grid-x-end 1) :y (+ grid-y 1)))  ; OUTSIDE box, right of it
            (t
             (dag-draw-point-create :x actual-center-x :y actual-center-y)))))

      ;; Port calculation complete

      result-port)))

(defun dag-draw--determine-port-side (node port min-x min-y scale &optional graph)
  "Determine which side of NODE the PORT is on (top/bottom/left/right).
This function helps identify port orientation for proper connection logic."
  (let* ((node-center (dag-draw--get-node-center-grid node min-x min-y scale graph))
         (center-x (dag-draw-point-x node-center))
         (center-y (dag-draw-point-y node-center))
         (port-x (dag-draw-point-x port))
         (port-y (dag-draw-point-y port))
         (dx (- port-x center-x))
         (dy (- port-y center-y)))

    ;; Determine which side based on the largest directional difference
    (cond
     ((and (>= (abs dy) (abs dx)) (< dy 0)) 'top)
     ((and (>= (abs dy) (abs dx)) (> dy 0)) 'bottom)
     ((and (> (abs dx) (abs dy)) (< dx 0)) 'left)
     ((and (> (abs dx) (abs dy)) (> dx 0)) 'right)
     (t 'center)))) ; Fallback for center ports

(defun dag-draw--calculate-edge-ports (from-node to-node)
  "Calculate appropriate ports for edge between FROM-NODE and TO-NODE.
Returns list of (from-port to-port) based on edge direction.
Returns nil if either node lacks coordinates."
  (let* ((from-x (dag-draw-node-x-coord from-node))
         (from-y (dag-draw-node-y-coord from-node))
         (to-x (dag-draw-node-x-coord to-node))
         (to-y (dag-draw-node-y-coord to-node)))

    ;; Return nil if any coordinate is missing - prevents arithmetic errors
    (when (and from-x from-y to-x to-y)
      (let* ((dx (- to-x from-x))
             (dy (- to-y from-y))
             ;; Calculate node-size-aware thresholds instead of hard-coded 20
             (horizontal-threshold (/ (+ (dag-draw-node-x-size from-node) (dag-draw-node-x-size to-node)) 4.0))
             (vertical-threshold (/ (+ (dag-draw-node-y-size from-node) (dag-draw-node-y-size to-node)) 4.0)))

        ;; Determine primary direction using adaptive thresholds
        (cond
         ;; Vertical edge (down)
         ((and (< (abs dx) horizontal-threshold) (> dy 0))
          (list (dag-draw--get-node-port from-node 'bottom)
                (dag-draw--get-node-port to-node 'top)))
         ;; Vertical edge (up)
         ((and (< (abs dx) horizontal-threshold) (< dy 0))
          (list (dag-draw--get-node-port from-node 'top)
                (dag-draw--get-node-port to-node 'bottom)))
         ;; Horizontal edge (right)
         ((and (< (abs dy) vertical-threshold) (> dx 0))
          (list (dag-draw--get-node-port from-node 'right)
                (dag-draw--get-node-port to-node 'left)))
         ;; Horizontal edge (left)
         ((and (< (abs dy) vertical-threshold) (< dx 0))
          (list (dag-draw--get-node-port from-node 'left)
                (dag-draw--get-node-port to-node 'right)))
         ;; Diagonal edge - prefer vertical direction (including equal distances)
         ((>= (abs dy) (abs dx))
          (if (> dy 0)
              (list (dag-draw--get-node-port from-node 'bottom)
                    (dag-draw--get-node-port to-node 'top))
            (list (dag-draw--get-node-port from-node 'top)
                  (dag-draw--get-node-port to-node 'bottom))))
         ;; Diagonal edge - prefer horizontal direction
         (t
          (if (> dx 0)
              (list (dag-draw--get-node-port from-node 'right)
                    (dag-draw--get-node-port to-node 'left))
            (list (dag-draw--get-node-port from-node 'left)
                  (dag-draw--get-node-port to-node 'right)))))))))

(defun dag-draw--calculate-edge-ports-grid (from-node to-node min-x min-y scale &optional graph)
  "Calculate appropriate ports for edge between FROM-NODE and TO-NODE using grid coordinates.
Returns list of (from-port to-port) based on edge direction with grid-aware positioning."
  (let* ((from-x (dag-draw-node-x-coord from-node))
         (from-y (dag-draw-node-y-coord from-node))
         (to-x (dag-draw-node-x-coord to-node))
         (to-y (dag-draw-node-y-coord to-node)))

    ;; Return nil if any coordinate is missing - prevents arithmetic errors
    (when (and from-x from-y to-x to-y)
      (let* ((dx (- to-x from-x))
             (dy (- to-y from-y))
             ;; Calculate node-size-aware thresholds instead of hard-coded 20
             (horizontal-threshold (/ (+ (dag-draw-node-x-size from-node) (dag-draw-node-x-size to-node)) 4.0))
             (vertical-threshold (/ (+ (dag-draw-node-y-size from-node) (dag-draw-node-y-size to-node)) 4.0)))

        ;; Determine primary direction using adaptive thresholds (same logic as original)
        (cond
         ;; Vertical edge (down)
         ((and (< (abs dx) horizontal-threshold) (> dy 0))
          (list (dag-draw--get-node-port-grid from-node 'bottom min-x min-y scale graph)
                (dag-draw--get-node-port-grid to-node 'top min-x min-y scale graph)))
         ;; Vertical edge (up)
         ((and (< (abs dx) horizontal-threshold) (< dy 0))
          (list (dag-draw--get-node-port-grid from-node 'top min-x min-y scale graph)
                (dag-draw--get-node-port-grid to-node 'bottom min-x min-y scale graph)))
         ;; Horizontal edge (right)
         ((and (< (abs dy) vertical-threshold) (> dx 0))
          (let ((from-port (dag-draw--get-node-port-grid from-node 'right min-x min-y scale graph))
                (to-port (dag-draw--get-node-port-grid to-node 'left min-x min-y scale graph)))
            (list from-port to-port)))
         ;; Horizontal edge (left)
         ((and (< (abs dy) vertical-threshold) (< dx 0))
          (list (dag-draw--get-node-port-grid from-node 'left min-x min-y scale graph)
                (dag-draw--get-node-port-grid to-node 'right min-x min-y scale graph)))
         ;; Diagonal edge - prefer vertical direction (including equal distances)
         ((>= (abs dy) (abs dx))
          (if (> dy 0)
              (list (dag-draw--get-node-port-grid from-node 'bottom min-x min-y scale graph)
                    (dag-draw--get-node-port-grid to-node 'top min-x min-y scale graph))
            (list (dag-draw--get-node-port-grid from-node 'top min-x min-y scale graph)
                  (dag-draw--get-node-port-grid to-node 'bottom min-x min-y scale graph))))
         ;; Diagonal edge - prefer horizontal direction
         (t
          (if (> dx 0)
              (list (dag-draw--get-node-port-grid from-node 'right min-x min-y scale graph)
                    (dag-draw--get-node-port-grid to-node 'left min-x min-y scale graph))
            (list (dag-draw--get-node-port-grid from-node 'left min-x min-y scale graph)
                  (dag-draw--get-node-port-grid to-node 'right min-x min-y scale graph)))))))))

(defun dag-draw--calculate-distributed-edge-ports (graph edge from-node to-node min-x min-y scale)
  "Calculate edge ports using simplified GKNV-compliant approach.
Removes complex multi-edge distribution in favor of basic direction-based ports."
  ;; Simplified: always use standard grid-based port calculation
  ;; This aligns with GKNV Section 5.1.1: \"route to appropriate side\"
  (dag-draw--calculate-edge-ports-grid from-node to-node min-x min-y scale graph))


(defun dag-draw--world-point-to-grid (world-point min-x min-y scale)
  "Convert world coordinate point to ASCII grid coordinates with precise rounding."
  (dag-draw-point-create
   :x (dag-draw--world-to-grid-coord (dag-draw-point-x world-point) min-x scale)
   :y (dag-draw--world-to-grid-coord (dag-draw-point-y world-point) min-y scale)))

(defun dag-draw--get-edge-connection-points (graph edge &optional min-x min-y scale)
  "Get connection points for edge in ASCII rendering context.
If grid parameters are provided, uses grid-aware port calculation for precise alignment."
  (let* ((from-node (dag-draw-get-node graph (dag-draw-edge-from-node edge)))
         (to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge)))
         (result (if (and min-x min-y scale)
                     ;; Use simplified GKNV-compliant port calculation
                     (dag-draw--calculate-distributed-edge-ports graph edge from-node to-node min-x min-y scale)
                   (dag-draw--calculate-edge-ports from-node to-node))))
    result))

(provide 'dag-draw-ports)

;;; dag-draw-ports.el ends here
