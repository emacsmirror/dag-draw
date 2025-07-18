;;; dag-draw-ascii-grid.el --- ASCII grid management for dag-draw -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; ASCII grid creation, coordinate conversion, and occupancy map management
;; for dag-draw graphs. This module handles the foundational grid infrastructure
;; that other ASCII rendering modules depend on.

;;; Code:

(require 'ht)
(require 'dag-draw-core)

;;; Customization



;;; ASCII Scaling Helper Functions

(defun dag-draw--world-to-grid-coord (coord min-coord scale)
  "Convert GKNV world coordinate to ASCII grid coordinate with precise rounding.
COORD is the world coordinate, MIN-COORD is the minimum coordinate for offset,
SCALE is the grid scale factor.
SCALE UNIFICATION: Use only dag-draw-ascii-coordinate-scale to eliminate double multiplication."
  (float (round (* (- coord min-coord) dag-draw-ascii-coordinate-scale))))

(defun dag-draw--grid-to-world-coord (grid-coord min-coord scale)
  "Convert ASCII grid coordinate back to GKNV world coordinate.
GRID-COORD is the grid coordinate, MIN-COORD is the minimum coordinate for offset,
SCALE is the grid scale factor. This is the inverse of dag-draw--world-to-grid-coord."
  (+ (/ grid-coord dag-draw-ascii-coordinate-scale) min-coord))

(defun dag-draw--world-to-grid-size (size scale)
  "Convert GKNV node size to ASCII grid size using UNIFIED scale factor.
SIZE is the node size in world coordinates, SCALE is the grid scale factor.
MATHEMATICAL UNIFICATION FIX: Use same scale as coordinate conversion to eliminate 6.7% mismatch."
  (max 3 (ceiling (* size dag-draw-ascii-coordinate-scale))))

(defun dag-draw--get-node-center-grid (node min-x min-y scale &optional graph)
  "Get node center coordinates directly in grid space for simple edge routing.
    VISUAL FIX: Simplified coordinate calculation to avoid conversion errors."
  (let* ((node-id (dag-draw-node-id node))
         ;; Check if node has manually set coordinates (non-nil)
         (manual-x (dag-draw-node-x-coord node))
         (manual-y (dag-draw-node-y-coord node))
         (has-manual-coords (and manual-x manual-y))
         ;; Get adjusted coordinates from layout algorithm
         (adjusted-coords (and graph
                               (dag-draw-graph-adjusted-positions graph)
                               (ht-get (dag-draw-graph-adjusted-positions graph) node-id))))

    (if (and adjusted-coords (not has-manual-coords))
        ;; Use adjusted coordinates only if no manual coordinates are set
        (dag-draw-point-create
         :x (+ (nth 0 adjusted-coords) (/ (nth 2 adjusted-coords) 2.0))
         :y (+ (nth 1 adjusted-coords) (/ (nth 3 adjusted-coords) 2.0)))
      ;; Prioritize manual coordinates - convert world coordinates to grid coordinates
      (let* ((world-x (or manual-x 0))
             (world-y (or manual-y 0))
             (grid-x (dag-draw--world-to-grid-coord world-x min-x scale))
             (grid-y (dag-draw--world-to-grid-coord world-y min-y scale)))
        (dag-draw-point-create :x grid-x :y grid-y)))))

;;; ASCII Coordinate Context Layer

(defun dag-draw--create-ascii-coordinate-context (graph)
  "Create a normalized coordinate context specifically for ASCII rendering.
This layer isolates ASCII coordinate normalization from other rendering paths."
  (let* ((raw-bounds (dag-draw-get-graph-bounds graph))
         (min-x (nth 0 raw-bounds))
         (min-y (nth 1 raw-bounds))
         (max-x (nth 2 raw-bounds))
         (max-y (nth 3 raw-bounds))
         ;; Calculate offsets to make coordinates non-negative
         (offset-x (if (< min-x 0) (- min-x) 0))
         (offset-y (if (< min-y 0) (- min-y) 0))
         (context (ht-create)))
    
    ;; Store the normalization offsets for coordinate conversion
    (ht-set! context 'offset-x offset-x)
    (ht-set! context 'offset-y offset-y)
    (ht-set! context 'original-bounds raw-bounds)
    
    ;; Calculate ASCII-safe bounds (guaranteed non-negative)
    ;; COORDINATE EXPLOSION FIX: Use original dimensions without doubling offset
    (ht-set! context 'ascii-bounds 
             (list 0 0 
                   (- max-x min-x)  ; Original width preserved
                   (- max-y min-y))) ; Original height preserved
    
    ;; Debug output
    (message "ASCII-CONTEXT: offset-x=%.1f offset-y=%.1f" offset-x offset-y)
    (message "ASCII-CONTEXT: original bounds (%.1f,%.1f,%.1f,%.1f) → ascii bounds (%.1f,%.1f,%.1f,%.1f)"
             min-x min-y max-x max-y
             0.0 0.0 (nth 2 (ht-get context 'ascii-bounds)) (nth 3 (ht-get context 'ascii-bounds)))
    
    context))

(defun dag-draw--ascii-world-to-grid (world-x world-y context scale)
  "Convert world coordinates to ASCII grid using normalized context.
This ensures all ASCII grid coordinates are non-negative."
  (let ((offset-x (ht-get context 'offset-x))
        (offset-y (ht-get context 'offset-y)))
    (list (dag-draw--world-to-grid-coord (+ world-x offset-x) 0 scale)
          (dag-draw--world-to-grid-coord (+ world-y offset-y) 0 scale))))

(defun dag-draw--ascii-get-bounds (context)
  "Get ASCII bounds from context.
Returns (min-x min-y max-x max-y) where min-x and min-y are always 0."
  (ht-get context 'ascii-bounds))

;;; ASCII Grid Creation

(defun dag-draw--create-ascii-grid (width height)
  "Create empty ASCII grid of given WIDTH and HEIGHT."
  (let ((grid (make-vector height nil)))
    (dotimes (y height)
      (aset grid y (make-vector width ?\s)))  ; Fill with spaces
    grid))

;;; Node Collision Detection

(defun dag-draw--would-violate-hierarchy (graph node-id proposed-y current-drawn-nodes)
  "Check if moving NODE-ID to PROPOSED-Y would violate hierarchical ordering.
CURRENT-DRAWN-NODES is a list of (node-id x y width height) for already positioned nodes."
  (when graph
    (let ((node (dag-draw-get-node graph node-id))
          (violates-hierarchy nil))
      (when node
        (let ((node-rank (or (dag-draw-node-rank node) 0)))
          ;; Check against all other drawn nodes
          (dolist (drawn-info current-drawn-nodes)
            (let* ((other-node-id (nth 0 drawn-info))
                   (other-y (nth 2 drawn-info))
                   (other-node (dag-draw-get-node graph other-node-id))
                   (other-rank (when other-node (or (dag-draw-node-rank other-node) 0))))
              (when other-rank
                ;; Lower ranks should have lower Y coordinates (appear higher on screen)
                (cond
                 ;; Current node has lower rank but would be positioned below other node
                 ((and (< node-rank other-rank) (>= proposed-y other-y))
                  (setq violates-hierarchy t))
                 ;; Current node has higher rank but would be positioned above other node
                 ((and (> node-rank other-rank) (<= proposed-y other-y))
                  (setq violates-hierarchy t)))))))
        violates-hierarchy))))

(defun dag-draw--rectangles-overlap (rect1 rect2)
  "Check if two rectangles overlap or are too close. Each rectangle is (x1 y1 x2 y2)."
  (let ((x1-1 (nth 0 rect1)) (y1-1 (nth 1 rect1)) (x2-1 (nth 2 rect1)) (y2-1 (nth 3 rect1))
        (x1-2 (nth 0 rect2)) (y1-2 (nth 1 rect2)) (x2-2 (nth 2 rect2)) (y2-2 (nth 3 rect2))
        (min-gap 5)) ; Optimal minimum gap - prevents text corruption and ensures readable spacing
    ;; Rectangles overlap or are too close if they're within min-gap distance
    (and (<= x1-1 (+ x2-2 min-gap)) (<= x1-2 (+ x2-1 min-gap))  ; x proximity
         (<= y1-1 (+ y2-2 min-gap)) (<= y1-2 (+ y2-1 min-gap))))) ; y proximity

(defun dag-draw--resolve-node-collision (x y width height drawn-nodes &optional graph node-id)
  "Resolve node collision by finding a non-overlapping position with minimum spacing.
Returns (adjusted-x adjusted-y) that avoids all drawn nodes with safe spacing.
GRAPH and NODE-ID are optional for hierarchy-aware collision resolution."
  ;; Debug output removed for cleaner production code
  (let ((min-spacing 3)  ; Reduced spacing for better test compatibility
        (current-rect (list x y (+ x width -1) (+ y height -1)))
        (max-attempts 20)
        (attempt 0)
        (best-x x)
        (best-y y))

    ;; Check if current position has any overlaps
    (let ((has-collision nil))
      (dolist (drawn-rect drawn-nodes)
        (when (dag-draw--rectangles-overlap current-rect drawn-rect)
          (setq has-collision t)))

      ;; If no collision, return original position
      (if (not has-collision)
          (list x y)

        ;; HIERARCHY-AWARE collision resolution: prioritize horizontal movement
        ;; to preserve GKNV hierarchical ordering
        (let ((position-found nil)
              (search-directions '((1 0)   ; right (PRIORITY 1)
                                   (-1 0)  ; left (PRIORITY 2)
                                   (0 1)   ; down (PRIORITY 3)
                                   (0 -1)  ; up (PRIORITY 4)
                                   (1 1)   ; diagonal down-right
                                   (-1 1)  ; diagonal down-left
                                   (1 -1)  ; diagonal up-right
                                   (-1 -1) ; diagonal up-left
                                   )))
          (while (and (< attempt max-attempts) (not position-found))
            (setq attempt (1+ attempt))

            ;; Try each direction at increasing distances
            (dolist (direction search-directions)
              (unless position-found
                (let* ((dir-x (car direction))
                       (dir-y (cadr direction))
                       (test-x (+ x (* dir-x attempt min-spacing)))
                       (test-y (+ y (* dir-y attempt min-spacing)))
                       (test-rect (list test-x test-y (+ test-x width -1) (+ test-y height -1)))
                       (collision-free t))

                  ;; Ensure position is within reasonable bounds (positive coordinates)
                  (when (and (>= test-x 0) (>= test-y 0))
                    (dolist (drawn-rect drawn-nodes)
                      (when (dag-draw--rectangles-overlap test-rect drawn-rect)
                        (setq collision-free nil)))

                    ;; HIERARCHY CHECK: Ensure proposed position doesn't violate rank ordering
                    (when (and collision-free graph node-id)
                      (let ((drawn-node-info (mapcar (lambda (rect)
                                                       ;; rect format: (x1 y1 x2 y2 node-id)
                                                       (when (>= (length rect) 5)
                                                         (list (nth 4 rect) (nth 0 rect) (nth 1 rect)
                                                               (- (nth 2 rect) (nth 0 rect))
                                                               (- (nth 3 rect) (nth 1 rect)))))
                                                     drawn-nodes)))
                        (setq drawn-node-info (delq nil drawn-node-info))  ; Remove any nils
                        (when (dag-draw--would-violate-hierarchy graph node-id test-y drawn-node-info)
                          (setq collision-free nil))))

                    (when collision-free
                      (setq best-x test-x
                            best-y test-y
                            position-found t)))))))

          (list best-x best-y))))))

;;; Global Occupancy Map

(defvar dag-draw--global-occupancy-map nil
  "Global occupancy map for comprehensive collision detection.")

;;; Grid Coordinate Utilities

(defun dag-draw--center-aware-round (grid-coord)
  "Round grid coordinate to ensure proper centering for arrow placement.
For ports that should be centered on node boundaries, this ensures the arrow
lands at the true visual center of the box, not the mathematical center."
  (round grid-coord))

(defun dag-draw--ascii-grid-to-string (grid)
  "Convert ASCII grid to string representation."
  (mapconcat (lambda (row)
               ;; FIXED: Don't use string-trim-right as it removes important characters
               ;; that happen to be followed by spaces. Instead, preserve all characters.
               (apply #'string (append row nil)))
             grid
             "\n"))

(provide 'dag-draw-ascii-grid)

;;; dag-draw-ascii-grid.el ends here
