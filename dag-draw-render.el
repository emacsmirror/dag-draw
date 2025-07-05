;;; dag-draw-render.el --- Graph rendering for dag-draw -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Implementation of graph rendering in multiple formats: SVG, ASCII art, and DOT.
;; This module takes positioned graphs from the layout algorithm and converts them
;; into visual representations with nodes, edges, and labels.

;;; Code:

(require 'dash)
(require 'ht)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-splines)
(require 'dag-draw-svg)
(require 'dag-draw-dot)
(require 'dag-draw-ascii-grid)
(require 'dag-draw-ports)
(require 'dag-draw-ascii-nodes)
(require 'dag-draw-ascii-edges)
(require 'dag-draw-position)
(require 'dag-draw-render-gknv-compliant)

;;; Customization

(defgroup dag-draw-render nil
  "Rendering options for dag-draw graphs."
  :group 'dag-draw
  :prefix "dag-draw-render-")

(defcustom dag-draw-render-svg-node-fill "#f0f0f0"
  "Default fill color for SVG nodes."
  :type 'string
  :group 'dag-draw-render)

(defcustom dag-draw-render-svg-node-stroke "#000000"
  "Default stroke color for SVG nodes."
  :type 'string
  :group 'dag-draw-render)

(defcustom dag-draw-render-svg-edge-stroke "#666666"
  "Default stroke color for SVG edges."
  :type 'string
  :group 'dag-draw-render)

;; PHASE 1 FIX: Removed dag-draw-render-ascii-grid-scale - use dag-draw-ascii-coordinate-scale only
;; COORDINATE SYSTEM FIX: Scale factors unified in dag-draw-ascii-grid.el

;;; Helper Functions

(defun dag-draw--calculate-final-bounds (adjusted-positions)
  "Calculate final bounds based on collision-adjusted positions.
ADJUSTED-POSITIONS is a hash table mapping node-id to (x y width height)."
  (if (not adjusted-positions)
      (list 0 0 20 20)  ; Default bounds for empty graphs
    (let ((min-x most-positive-fixnum)
          (min-y most-positive-fixnum)
          (max-x most-negative-fixnum)
          (max-y most-negative-fixnum))
      (ht-each (lambda (node-id coords)
                 (let ((x (nth 0 coords))
                       (y (nth 1 coords))
                       (width (nth 2 coords))
                       (height (nth 3 coords)))
                   (setq min-x (min min-x x))
                   (setq min-y (min min-y y))
                   (setq max-x (max max-x (+ x width)))
                   (setq max-y (max max-y (+ y height)))))
               adjusted-positions)
      (list min-x min-y max-x max-y))))

;;; Main Rendering Functions

(defun dag-draw-render-ascii (graph)
  "GKNV-compliant ASCII rendering: pure coordinate conversion without regeneration.
This function respects the GKNV 4-pass algorithm and performs only coordinate conversion."
  ;; GKNV COMPLIANCE: Ensure 4-pass algorithm has completed
  (unless (and (dag-draw-graph-edges graph)
               (dag-draw-edge-spline-points (car (dag-draw-graph-edges graph))))
    ;; Run GKNV algorithm ONCE if not already done
    (dag-draw-layout-graph graph))

  ;; Handle empty graphs
  (if (= (ht-size (dag-draw-graph-nodes graph)) 0)
      "(Empty Graph)"
    ;; PURE COORDINATE CONVERSION: No coordinate system changes, no regeneration
    (dag-draw--convert-gknv-to-ascii-grid graph)))

(defun dag-draw--convert-gknv-to-ascii-grid (graph)
  "Pure conversion of GKNV final coordinates to ASCII grid.
Does NOT modify graph coordinates or regenerate splines."

  (let* (;; Step 1: Calculate conversion parameters from GKNV final coordinates
         (bounds (dag-draw-get-graph-bounds graph))
         (min-x (nth 0 bounds))
         (min-y (nth 1 bounds))
         (max-x (nth 2 bounds))
         (max-y (nth 3 bounds))
         (scale dag-draw-ascii-coordinate-scale)

         ;; Step 2: Calculate grid size needed for GKNV coordinates
         (width (- max-x min-x))
         (height (- max-y min-y))
         (grid-width (max 20 (+ 10 (ceiling (* width scale)))))
         (grid-height (max 10 (+ 5 (ceiling (* height scale)))))

         ;; Step 3: Create ASCII grid
         (grid (dag-draw--create-ascii-grid grid-width grid-height)))

    (message "\n=== GKNV-COMPLIANT ASCII CONVERSION ===")
    (message "GKNV bounds: (%.1f,%.1f) to (%.1f,%.1f)" min-x min-y max-x max-y)
    (message "ASCII grid: %dx%d, scale=%.3f" grid-width grid-height scale)

    ;; Step 4: Draw nodes using GKNV final coordinates
    (dag-draw--draw-nodes-gknv-compliant graph grid min-x min-y scale)

    ;; Step 5: Draw edges using GKNV final splines
    (dag-draw--draw-edges-gknv-compliant graph grid min-x min-y scale)

    ;; Step 6: Convert grid to string
    (dag-draw--ascii-grid-to-string grid)))

;; (defun dag-draw--draw-nodes-gknv-compliant (graph grid min-x min-y scale)
;;   "Draw nodes using GKNV final coordinates without modification."

;;   (ht-each (lambda (node-id node)
;;              (let* (;; Use GKNV final coordinates directly
;;                     (world-x (dag-draw-node-x-coord node))
;;                     (world-y (dag-draw-node-y-coord node))
;;                     (world-width (dag-draw-node-x-size node))
;;                     (world-height (dag-draw-node-y-size node))
;;                     (node-label (dag-draw-node-label node))

;;                     ;; Convert to grid coordinates
;;                     (grid-center-x (dag-draw--world-to-grid-coord world-x min-x scale))
;;                     (grid-center-y (dag-draw--world-to-grid-coord world-y min-y scale))
;;                     (grid-width (dag-draw--world-to-grid-size world-width scale))
;;                     (grid-height (dag-draw--world-to-grid-size world-height scale))

;;                     ;; Calculate grid position (top-left corner)
;;                     (grid-x (round (- grid-center-x (/ grid-width 2))))
;;                     (grid-y (round (- grid-center-y (/ grid-height 2)))))

;;                (message "GKNV-NODE: %s world(%.1f,%.1f) → grid(%d,%d) size(%dx%d)"
;;                         node-id world-x world-y grid-x grid-y grid-width grid-height)

;;                ;; Draw node box at calculated position
;;                (dag-draw--draw-node-box grid grid-x grid-y grid-width grid-height node-label)))
;;            (dag-draw-graph-nodes graph)))

(defun dag-draw--draw-edges-gknv-compliant (graph grid min-x min-y scale)
  "Draw edges using GKNV final splines without regeneration."

  (dolist (edge (dag-draw-graph-edges graph))
    (let ((spline-points (dag-draw-edge-spline-points edge)))
      (when spline-points
        (let* ((start-point (car spline-points))
               (end-point (car (last spline-points)))

               ;; Convert GKNV spline endpoints to grid coordinates
               (start-world-x (dag-draw-point-x start-point))
               (start-world-y (dag-draw-point-y start-point))
               (end-world-x (dag-draw-point-x end-point))
               (end-world-y (dag-draw-point-y end-point))

               (start-grid-x (round (dag-draw--world-to-grid-coord start-world-x min-x scale)))
               (start-grid-y (round (dag-draw--world-to-grid-coord start-world-y min-y scale)))
               (end-grid-x (round (dag-draw--world-to-grid-coord end-world-x min-x scale)))
               (end-grid-y (round (dag-draw--world-to-grid-coord end-world-y min-y scale))))

          (message "GKNV-EDGE: %s→%s world(%.1f,%.1f)→(%.1f,%.1f) grid(%d,%d)→(%d,%d)"
                   (dag-draw-edge-from-node edge) (dag-draw-edge-to-node edge)
                   start-world-x start-world-y end-world-x end-world-y
                   start-grid-x start-grid-y end-grid-x end-grid-y)

          ;; Draw edge using proper port-based connection
          (dag-draw--draw-edge-with-proper-ports graph edge grid start-grid-x start-grid-y
                                                 end-grid-x end-grid-y min-x min-y scale))))))

(defun dag-draw--draw-edge-with-proper-ports (graph edge grid start-x start-y end-x end-y min-x min-y scale)
  "Draw edge with ports calculated from GKNV node boundaries."

  (let* ((from-node (dag-draw-get-node graph (dag-draw-edge-from-node edge)))
         (to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge)))

         ;; Calculate actual node boundaries in grid coordinates
         (from-world-x (dag-draw-node-x-coord from-node))
         (from-world-y (dag-draw-node-y-coord from-node))
         (from-world-width (dag-draw-node-x-size from-node))
         (from-world-height (dag-draw-node-y-size from-node))

         (to-world-x (dag-draw-node-x-coord to-node))
         (to-world-y (dag-draw-node-y-coord to-node))
         (to-world-width (dag-draw-node-x-size to-node))
         (to-world-height (dag-draw-node-y-size to-node))

         ;; Convert to grid boundaries
         (from-grid-center-x (dag-draw--world-to-grid-coord from-world-x min-x scale))
         (from-grid-center-y (dag-draw--world-to-grid-coord from-world-y min-y scale))
         (from-grid-width (dag-draw--world-to-grid-size from-world-width scale))
         (from-grid-height (dag-draw--world-to-grid-size from-world-height scale))

         (to-grid-center-x (dag-draw--world-to-grid-coord to-world-x min-x scale))
         (to-grid-center-y (dag-draw--world-to-grid-coord to-world-y min-y scale))
         (to-grid-width (dag-draw--world-to-grid-size to-world-width scale))
         (to-grid-height (dag-draw--world-to-grid-size to-world-height scale))

         ;; Calculate proper ports on node boundaries
         (from-port (dag-draw--calculate-boundary-port from-grid-center-x from-grid-center-y
                                                       from-grid-width from-grid-height 'bottom))
         (to-port (dag-draw--calculate-boundary-port to-grid-center-x to-grid-center-y
                                                     to-grid-width to-grid-height 'top)))

    (message "GKNV-PORTS: %s port(%d,%d) → %s port(%d,%d)"
             (dag-draw-edge-from-node edge) (nth 0 from-port) (nth 1 from-port)
             (dag-draw-edge-to-node edge) (nth 0 to-port) (nth 1 to-port))

    ;; Draw simple line between proper ports
    (dag-draw--draw-simple-line grid (nth 0 from-port) (nth 1 from-port)
                                (nth 0 to-port) (nth 1 to-port))))

(defun dag-draw--calculate-boundary-port (center-x center-y width height side)
  "Calculate port position on node boundary for given side."
  (let ((left (round (- center-x (/ width 2))))
        (right (round (+ center-x (/ width 2))))
        (top (round (- center-y (/ height 2))))
        (bottom (round (+ center-y (/ height 2)))))

    (cond
     ((eq side 'top) (list (round center-x) top))
     ((eq side 'bottom) (list (round center-x) bottom))
     ((eq side 'left) (list left (round center-y)))
     ((eq side 'right) (list right (round center-y)))
     (t (list (round center-x) (round center-y))))))

(defun dag-draw--draw-node-box (grid x y width height label)
  "Draw a node box with label at specified grid position."
  (let ((grid-height (length grid))
        (grid-width (if (> (length grid) 0) (length (aref grid 0)) 0)))

    ;; Only draw if within grid bounds
    (when (and (>= x 0) (>= y 0)
               (< (+ x width) grid-width)
               (< (+ y height) grid-height))

      ;; Draw top border
      (dotimes (i width)
        (dag-draw--set-char grid (+ x i) y
                            (cond ((= i 0) ?┌)
                                  ((= i (1- width)) ?┐)
                                  (t ?─))))

      ;; Draw middle rows with label
      (dotimes (row (- height 2))
        (let ((actual-row (+ y row 1)))
          ;; Left border
          (dag-draw--set-char grid x actual-row ?│)
          ;; Content area
          (dotimes (col (- width 2))
            (let ((char-pos (+ x col 1)))
              (if (and (= row 0) (< col (length label)))
                  (dag-draw--set-char grid char-pos actual-row (aref label col))
                (dag-draw--set-char grid char-pos actual-row ?\s))))
          ;; Right border
          (dag-draw--set-char grid (+ x width -1) actual-row ?│)))

      ;; Draw bottom border
      (dotimes (i width)
        (dag-draw--set-char grid (+ x i) (+ y height -1)
                            (cond ((= i 0) ?└)
                                  ((= i (1- width)) ?┘)
                                  (t ?─)))))))

(defun dag-draw--draw-simple-line (grid x1 y1 x2 y2)
  "Draw a simple line from (x1,y1) to (x2,y2) with arrow."
  (let ((grid-height (length grid))
        (grid-width (if (> (length grid) 0) (length (aref grid 0)) 0)))

    ;; Draw vertical line first
    (if (not (= y1 y2))
        (let ((start-y (min y1 y2))
              (end-y (max y1 y2)))
          (dotimes (i (1+ (- end-y start-y)))
            (let ((y (+ start-y i)))
              (when (and (>= x1 0) (< x1 grid-width) (>= y 0) (< y grid-height))
                (dag-draw--set-char grid x1 y ?│))))))

    ;; Draw horizontal line
    (if (not (= x1 x2))
        (let ((start-x (min x1 x2))
              (end-x (max x1 x2)))
          (dotimes (i (1+ (- end-x start-x)))
            (let ((x (+ start-x i)))
              (when (and (>= x 0) (< x grid-width) (>= y2 0) (< y2 grid-height))
                (dag-draw--set-char grid x y2 ?─))))))

    ;; Add arrow at end point
    (when (and (>= x2 0) (< x2 grid-width) (>= y2 0) (< y2 grid-height))
      (let ((arrow-char (cond ((> y2 y1) ?▼)  ; Down arrow
                              ((< y2 y1) ?▲)  ; Up arrow
                              ((> x2 x1) ?▶)  ; Right arrow
                              ((< x2 x1) ?◀)  ; Left arrow
                              (t ?●))))       ; Point
        (dag-draw--set-char grid x2 y2 arrow-char)))))

(defun dag-draw--set-char (grid x y char)
  "Safely set character in grid at position (x,y)."
  (let ((grid-height (length grid))
        (grid-width (if (> (length grid) 0) (length (aref grid 0)) 0)))
    (when (and (>= x 0) (< x grid-width) (>= y 0) (< y grid-height))
      (aset (aref grid y) x char))))

(defun dag-draw--pre-calculate-final-node-positions (graph grid min-x min-y scale)
  "PHASE 2 FIX: Pre-calculate final node positions with collision detection.
This ensures edges and nodes use the same coordinate system."
  (let ((drawn-nodes '())  ; Track already drawn nodes for collision detection
        (adjusted-positions (ht-create)))  ; Track final positions
    ;; HIERARCHY-AWARE PROCESSING: Sort nodes by rank to ensure proper ordering
    (let ((sorted-node-ids (sort (ht-keys (dag-draw-graph-nodes graph))
                                 (lambda (a b)
                                   (let* ((node-a (ht-get (dag-draw-graph-nodes graph) a))
                                          (node-b (ht-get (dag-draw-graph-nodes graph) b))
                                          (rank-a (or (dag-draw-node-rank node-a) 0))
                                          (rank-b (or (dag-draw-node-rank node-b) 0)))
                                     (< rank-a rank-b))))))
      (dolist (node-id sorted-node-ids)
        (let* ((node (ht-get (dag-draw-graph-nodes graph) node-id))
               (x (or (dag-draw-node-x-coord node) 0))
               (y (or (dag-draw-node-y-coord node) 0))
               (width (dag-draw-node-x-size node))
               (height (dag-draw-node-y-size node))
               ;; Convert world coordinates to grid coordinates
               (grid-center-x (dag-draw--world-to-grid-coord x min-x scale))
               (grid-center-y (dag-draw--world-to-grid-coord y min-y scale))
               (grid-width (dag-draw--world-to-grid-size width scale))
               (grid-height (dag-draw--world-to-grid-size height scale))
               ;; Calculate top-left corner for box drawing
               (grid-x (- grid-center-x (/ grid-width 2)))
               (grid-y (- grid-center-y (/ grid-height 2))))


          ;; Round coordinates to match occupancy map exactly
          (let ((final-x (round grid-x))
                (final-y (round grid-y))
                (final-width (round grid-width))
                (final-height (round grid-height)))

            ;; Apply collision avoidance spacing
            (let* ((adjusted-pos (dag-draw--resolve-node-collision
                                  final-x final-y final-width final-height drawn-nodes graph node-id))
                   (adjusted-x (car adjusted-pos))
                   (adjusted-y (cadr adjusted-pos)))
              (let ((current-rect (list adjusted-x adjusted-y
                                        (+ adjusted-x final-width -1)
                                        (+ adjusted-y final-height -1))))

                ;; Track this node for future collision detection
                (push (append current-rect (list node-id)) drawn-nodes)

                ;; Store final adjusted position
                (ht-set! adjusted-positions node-id
                         (list adjusted-x adjusted-y final-width final-height))))))))

    ;; Store adjusted positions in graph for both edge and node drawing to use
    (setf (dag-draw-graph-adjusted-positions graph) adjusted-positions)))

(defun dag-draw--regenerate-splines-after-collision (graph)
  "GHOST NODE FIX: Regenerate splines using collision-adjusted node positions.
This ensures splines connect nodes at their final positions after collision detection."
  (let ((adjusted-positions (dag-draw-graph-adjusted-positions graph)))
    (when adjusted-positions
      ;; Temporarily update node coordinates to collision-adjusted positions
      (let ((original-coords (ht-create)))
        ;; Save original coordinates and set adjusted coordinates
        (ht-each (lambda (node-id coords)
                   (let* ((node (ht-get (dag-draw-graph-nodes graph) node-id))
                          (adjusted-x (nth 0 coords))
                          (adjusted-y (nth 1 coords))
                          ;; COORDINATE SYSTEM FIX: Convert grid coordinates back to world coordinates
                          ;; adjusted coords are in grid space, need to convert to world space for spline generation
                          (grid-center-x (+ adjusted-x (/ (nth 2 coords) 2.0)))
                          (grid-center-y (+ adjusted-y (/ (nth 3 coords) 2.0)))
                          ;; COORDINATE SYSTEM FIX: Calculate bounds from adjusted positions, not original nodes
                          ;; This ensures the coordinate conversion uses the correct reference frame
                          (bounds (dag-draw--calculate-adjusted-bounds graph adjusted-positions))
                          (min-x (nth 0 bounds))
                          (min-y (nth 1 bounds))
                          (scale (or dag-draw-ascii-coordinate-scale 0.15))
                          (world-x (dag-draw--grid-to-world-coord grid-center-x min-x scale))
                          (world-y (dag-draw--grid-to-world-coord grid-center-y min-y scale)))
                     (when node
                       ;; Save original coordinates
                       (ht-set! original-coords node-id
                                (list (dag-draw-node-x-coord node) (dag-draw-node-y-coord node)))
                       ;; Set collision-adjusted coordinates
                       (setf (dag-draw-node-x-coord node) world-x)
                       (setf (dag-draw-node-y-coord node) world-y))))
                 adjusted-positions)

        ;; GHOST NODE FIX: Clear existing splines to prevent duplication
        (dolist (edge (dag-draw-graph-edges graph))
          (setf (dag-draw-edge-spline-points edge) nil))

        ;; Regenerate splines with updated coordinates
        (dag-draw-generate-splines graph)

        ;; Restore original coordinates (optional - may not be needed)
        (ht-each (lambda (node-id original-coord-pair)
                   (let ((node (ht-get (dag-draw-graph-nodes graph) node-id)))
                     (when node
                       (setf (dag-draw-node-x-coord node) (nth 0 original-coord-pair))
                       (setf (dag-draw-node-y-coord node) (nth 1 original-coord-pair)))))
                 original-coords)))))

(defun dag-draw--calculate-adjusted-bounds (graph adjusted-positions)
  "Calculate bounds from adjusted grid positions instead of original world coordinates.
This provides the correct reference frame for converting adjusted grid coords back to world coords."
  (if (= (ht-size adjusted-positions) 0)
      ;; Empty - return default bounds
      (list 0 0 100 100)
    (let ((min-x most-positive-fixnum)
          (min-y most-positive-fixnum)
          (max-x most-negative-fixnum)
          (max-y most-negative-fixnum))

      (ht-each (lambda (node-id coords)
                 (let* ((grid-x (nth 0 coords))
                        (grid-y (nth 1 coords))
                        (grid-width (nth 2 coords))
                        (grid-height (nth 3 coords))
                        ;; Calculate grid bounds
                        (left grid-x)
                        (right (+ grid-x grid-width))
                        (top grid-y)
                        (bottom (+ grid-y grid-height)))

                   (setq min-x (min min-x left))
                   (setq max-x (max max-x right))
                   (setq min-y (min min-y top))
                   (setq max-y (max max-y bottom))))
               adjusted-positions)

      ;; Convert grid bounds back to world bounds using the coordinate scale
      (let ((scale (or dag-draw-ascii-coordinate-scale 0.15)))
        (list (/ min-x scale) (/ min-y scale) (/ max-x scale) (/ max-y scale))))))

(defun dag-draw--convert-splines-to-grid-coordinates (graph min-x min-y scale)
  "Convert spline coordinates from GKNV world coordinates to ASCII grid coordinates.
This ensures splines and final node positions use the same coordinate system."
  (dolist (edge (dag-draw-graph-edges graph))
    (let ((spline-points (dag-draw-edge-spline-points edge)))
      (when spline-points
        ;; Convert each spline point from world coordinates to grid coordinates
        (let* ((start-point (car spline-points))
               (end-point (car (last spline-points)))
               (converted-points
                (mapcar (lambda (point)
                          (dag-draw-point-create
                           :x (dag-draw--world-to-grid-coord (dag-draw-point-x point) min-x scale)
                           :y (dag-draw--world-to-grid-coord (dag-draw-point-y point) min-y scale)))
                        spline-points)))
          (message "SPLINE-CONVERT: Edge %s->%s world (%.1f,%.1f)->(%.1f,%.1f) → grid (%.1f,%.1f)->(%.1f,%.1f)"
                   (dag-draw-edge-from-node edge) (dag-draw-edge-to-node edge)
                   (dag-draw-point-x start-point) (dag-draw-point-y start-point)
                   (dag-draw-point-x end-point) (dag-draw-point-y end-point)
                   (dag-draw-point-x (car converted-points)) (dag-draw-point-y (car converted-points))
                   (dag-draw-point-x (car (last converted-points))) (dag-draw-point-y (car (last converted-points))))
          ;; Update the edge with converted spline points
          (setf (dag-draw-edge-spline-points edge) converted-points))))))

(defun dag-draw-display-in-buffer (graph &optional buffer-name format)
  "Display rendered GRAPH in a buffer."
  (let* ((buffer (get-buffer-create (or buffer-name "*DAG Draw*")))
         (output-format (or format 'ascii))
         (content (dag-draw-render-graph graph output-format)))

    (with-current-buffer buffer
      (erase-buffer)
      (insert content)
      (goto-char (point-min))

      ;; Set appropriate mode
      (cond
       ((eq output-format 'svg)
        (when (fboundp 'nxml-mode) (nxml-mode)))
       ((eq output-format 'dot)
        (when (fboundp 'graphviz-dot-mode) (graphviz-dot-mode)))
       (t
        (text-mode))))

    (display-buffer buffer)
    buffer))

;;; SVG and DOT rendering functions are provided by their respective modules

;;; Enhanced spline segment drawing functions for Phase C improvements

(defun dag-draw--optimize-spline-sampling (spline-points grid-width grid-height)
  "Optimize spline point sampling for better ASCII grid representation."
  (when (and spline-points (> (length spline-points) 1))
    (let* ((total-length (dag-draw--calculate-spline-path-length spline-points))
           ;; Adaptive sampling density based on grid size and path complexity
           (optimal-density (max 0.5 (min 2.0 (/ total-length (max grid-width grid-height)))))
           (target-points (max 3 (round (* total-length optimal-density))))
           (sampled-points nil))

      ;; Ensure we include start and end points
      (push (car spline-points) sampled-points)

      ;; Sample intermediate points based on path curvature
      (when (> target-points 2)
        (let ((step-size (/ (float (1- (length spline-points))) (float (1- target-points)))))
          (dotimes (i (- target-points 2))
            (let* ((float-index (+ 1 (* (1+ i) step-size)))
                   (index (floor float-index))
                   (fraction (- float-index index)))
              (when (< index (1- (length spline-points)))
                (let* ((p1 (nth index spline-points))
                       (p2 (nth (1+ index) spline-points))
                       ;; Linear interpolation between points
                       (interp-x (+ (* (1- fraction) (dag-draw-point-x p1))
                                    (* fraction (dag-draw-point-x p2))))
                       (interp-y (+ (* (1- fraction) (dag-draw-point-y p1))
                                    (* fraction (dag-draw-point-y p2)))))
                  (push (dag-draw-point-create :x interp-x :y interp-y) sampled-points)))))))

      ;; Add end point
      (push (car (last spline-points)) sampled-points)

      ;; Return in correct order
      (nreverse sampled-points))))

(defun dag-draw--calculate-spline-path-length (spline-points)
  "Calculate approximate length of spline path for sampling optimization."
  (let ((total-length 0.0))
    (when (> (length spline-points) 1)
      (dotimes (i (1- (length spline-points)))
        (let* ((p1 (nth i spline-points))
               (p2 (nth (1+ i) spline-points))
               (dx (- (dag-draw-point-x p2) (dag-draw-point-x p1)))
               (dy (- (dag-draw-point-y p2) (dag-draw-point-y p1))))
          (setq total-length (+ total-length (sqrt (+ (* dx dx) (* dy dy))))))))
    total-length))


(provide 'dag-draw-render)

;;; dag-draw-render.el ends here
