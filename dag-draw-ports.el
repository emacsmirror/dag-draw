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
         ;; Use adjusted coordinates only if no manual coordinates are set
         (adjusted-coords (and graph
                               (dag-draw-graph-adjusted-positions graph)
                               (ht-get (dag-draw-graph-adjusted-positions graph) node-id)
                               (not has-manual-coords)))
         (x (if adjusted-coords
                ;; Adjusted coordinates are already in grid space: (x y width height)
                (float (nth 0 adjusted-coords))
              ;; Prioritize manual coordinates - need conversion
              (float (or manual-x 0))))
         (y (if adjusted-coords
                (float (nth 1 adjusted-coords))
              (float (or manual-y 0))))
         (width (if adjusted-coords
                    (float (nth 2 adjusted-coords))
                  (float (dag-draw-node-x-size node))))
         (height (if adjusted-coords
                     (float (nth 3 adjusted-coords))
                   (float (dag-draw-node-y-size node))))
         ;; If using adjusted coordinates, they're already in grid space
         (grid-center-x (if adjusted-coords
                            (+ x (/ width 2.0))
                          (dag-draw--world-to-grid-coord x min-x scale)))
         (grid-center-y (if adjusted-coords
                            (+ y (/ height 2.0))
                          (dag-draw--world-to-grid-coord y min-y scale)))
         (grid-width (if adjusted-coords width (dag-draw--world-to-grid-size width scale)))
         (grid-height (if adjusted-coords height (dag-draw--world-to-grid-size height scale)))
         ;; Calculate actual box boundaries
         (grid-x (if adjusted-coords (round x) (round (- grid-center-x (/ grid-width 2)))))
         (grid-y (if adjusted-coords (round y) (round (- grid-center-y (/ grid-height 2)))))
         (grid-x-end (+ grid-x grid-width -1))
         (grid-y-end (+ grid-y grid-height -1))
         ;; Calculate actual center after rounding
         (actual-center-x (+ grid-x (/ grid-width 2.0)))
         (actual-center-y (+ grid-y (/ grid-height 2.0))))

    ;; COORDINATE SYSTEM FIX: Return grid coordinates directly to avoid double conversion
    (cond
     ((eq side 'top)
      (dag-draw-point-create :x actual-center-x :y grid-y))
     ((eq side 'bottom)
      (dag-draw-point-create :x actual-center-x :y grid-y-end))
     ((eq side 'left)
      (dag-draw-point-create :x grid-x :y (+ grid-y 1)))  ; Content line, not center
     ((eq side 'right)
      (dag-draw-point-create :x grid-x-end :y (+ grid-y 1)))
     (t
      (dag-draw-point-create :x actual-center-x :y actual-center-y)))))

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
  "Calculate edge ports with distribution logic for multiple edges from same node.
This prevents corner crowding by spreading multiple edges across different port positions."
  (let* ((from-node-id (dag-draw-edge-from-node edge))
         (edges-from-node (dag-draw-get-edges-from graph from-node-id))
         (edge-count (length edges-from-node))
         (edge-index (--find-index (eq edge it) edges-from-node)))

    (if (and (> edge-count 1) edge-index)
        ;; Multiple edges from same node - distribute ports
        (dag-draw--calculate-distributed-ports-multi-edge
         from-node to-node edge-index edge-count min-x min-y scale graph)
      ;; Single edge - use standard port calculation
      (dag-draw--calculate-edge-ports-grid from-node to-node min-x min-y scale graph))))

(defun dag-draw--calculate-distributed-ports-multi-edge (from-node to-node edge-index edge-count min-x min-y scale graph)
  "Calculate distributed ports for multiple edges from same node.
EDGE-INDEX is the 0-based index of this edge among all edges from the node.
EDGE-COUNT is the total number of edges from the node."
  (let* ((from-x (dag-draw-node-x-coord from-node))
         (from-y (dag-draw-node-y-coord from-node))
         (to-x (dag-draw-node-x-coord to-node))
         (to-y (dag-draw-node-y-coord to-node))
         (dx (- to-x from-x))
         (dy (- to-y from-y))
         ;; Determine primary side based on direction
         (horizontal-threshold (/ (+ (dag-draw-node-x-size from-node) (dag-draw-node-x-size to-node)) 4.0))
         (vertical-threshold (/ (+ (dag-draw-node-y-size from-node) (dag-draw-node-y-size to-node)) 4.0))
         (primary-side (cond
                        ;; Vertical edges
                        ((and (< (abs dx) horizontal-threshold) (> dy 0)) 'bottom)
                        ((and (< (abs dx) horizontal-threshold) (< dy 0)) 'top)
                        ;; Horizontal edges
                        ((and (< (abs dy) vertical-threshold) (> dx 0)) 'right)
                        ((and (< (abs dy) vertical-threshold) (< dx 0)) 'left)
                        ;; Diagonal - prefer vertical
                        ((>= (abs dy) (abs dx)) (if (> dy 0) 'bottom 'top))
                        ;; Diagonal - prefer horizontal
                        (t (if (> dx 0) 'right 'left)))))

    ;; Calculate distributed port for from-node
    (let ((from-port (dag-draw--get-distributed-port from-node primary-side edge-index edge-count min-x min-y scale graph))
          ;; Use standard port calculation for to-node (no distribution needed for target)
          (to-port (cond
                    ((eq primary-side 'bottom) (dag-draw--get-node-port-grid to-node 'top min-x min-y scale graph))
                    ((eq primary-side 'top) (dag-draw--get-node-port-grid to-node 'bottom min-x min-y scale graph))
                    ((eq primary-side 'right) (dag-draw--get-node-port-grid to-node 'left min-x min-y scale graph))
                    ((eq primary-side 'left) (dag-draw--get-node-port-grid to-node 'right min-x min-y scale graph)))))

      (list from-port to-port))))

(defun dag-draw--get-distributed-port (node side edge-index edge-count min-x min-y scale &optional graph)
  "Get a distributed port position for NODE on SIDE.
EDGE-INDEX is 0-based index of this edge, EDGE-COUNT is total edges from node."
  ;; Get the basic side port position
  (let* ((base-port (dag-draw--get-node-port-grid node side min-x min-y scale graph))
         (base-x (dag-draw-point-x base-port))
         (base-y (dag-draw-point-y base-port)))

    ;; For multiple edges, offset from the base position
    (if (= edge-count 1)
        base-port
      ;; Calculate offset based on edge index
      (let* ((offset-factor (if (> edge-count 1) (/ (- edge-index (/ (1- edge-count) 2.0)) edge-count) 0))
             (max-offset 3.0) ; Maximum offset for horizontal sides
             (vertical-offset 1.0)) ; Smaller offset for vertical sides to maintain alignment
        (cond
         ;; Horizontal sides: distribute vertically
         ((or (eq side 'left) (eq side 'right))
          (dag-draw-point-create :x base-x :y (+ base-y (* offset-factor max-offset))))
         ;; Vertical sides: distribute horizontally with smaller offset
         ((or (eq side 'top) (eq side 'bottom))
          (dag-draw-point-create :x (+ base-x (* offset-factor vertical-offset)) :y base-y)))))))

(defun dag-draw--world-point-to-grid (world-point min-x min-y scale)
  "Convert world coordinate point to ASCII grid coordinates with precise rounding."
  (dag-draw-point-create
   :x (dag-draw--world-to-grid-coord (dag-draw-point-x world-point) min-x scale)
   :y (dag-draw--world-to-grid-coord (dag-draw-point-y world-point) min-y scale)))

(defun dag-draw--get-edge-connection-points (graph edge &optional min-x min-y scale)
  "Get connection points for edge in ASCII rendering context.
If grid parameters are provided, uses grid-aware port calculation for precise alignment."
  (let* ((from-node (dag-draw-get-node graph (dag-draw-edge-from-node edge)))
         (to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge))))
    (if (and min-x min-y scale)
        ;; Use enhanced port calculation with multi-edge distribution
        (dag-draw--calculate-distributed-edge-ports graph edge from-node to-node min-x min-y scale)
      (dag-draw--calculate-edge-ports from-node to-node))))

(provide 'dag-draw-ports)

;;; dag-draw-ports.el ends here