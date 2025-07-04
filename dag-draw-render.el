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
  "Render GRAPH as ASCII art with box-drawing characters."
  ;; GHOST NODE FIX: Only run layout if graph hasn't been laid out yet
  ;; Check if splines exist to determine if layout has been done
  (unless (and (dag-draw-graph-edges graph)
               (dag-draw-edge-spline-points (car (dag-draw-graph-edges graph))))
    ;; GKNV COMPLIANCE FIX: Ensure all 4 passes are executed before rendering
    ;; The layout algorithm must be called to generate splines (Pass 4)
    (dag-draw-layout-graph graph))

  ;; Handle empty graphs explicitly
  (if (= (ht-size (dag-draw-graph-nodes graph)) 0)
      "(Empty Graph)"
    ;; ASCII COORDINATE CONTEXT: Create normalized coordinate system for ASCII rendering only
    (let* ((ascii-context (dag-draw--create-ascii-coordinate-context graph))
           (ascii-bounds (dag-draw--ascii-get-bounds ascii-context))
           (min-x (nth 0 ascii-bounds))  ; Always 0
           (min-y (nth 1 ascii-bounds))  ; Always 0 
           (max-x (nth 2 ascii-bounds))
           (max-y (nth 3 ascii-bounds)))


      (let* (;; PHASE 1 FIX: Use unified coordinate scale throughout system
             (scale dag-draw-ascii-coordinate-scale)
             (width-diff (- max-x min-x))
             (height-diff (- max-y min-y))
             ;; Calculate maximum node extents to ensure complete boxes fit
             (max-node-x-extent 0)
             (max-node-y-extent 0)
             ;; Adaptive spacing buffer based on graph complexity + collision detection range
             (node-count (ht-size (dag-draw-graph-nodes graph)))
             ;; COORDINATE PRESERVATION: Minimal collision buffers for compact output
             ;; PHASE 1 FIX: Further reduce collision buffers to prevent grid bloat
             (base-collision-buffer (cond
                                     ((< node-count 3) 2)    ; Small graphs - minimal buffer
                                     ((< node-count 5) 3)    ; Medium graphs - small buffer
                                     (t 4)))                  ; Large graphs - modest buffer (was 8!)
             ;; Reduce collision movement allowance to preserve coordinate precision
             (max-collision-movement (cond
                                      ((< node-count 3) 3)    ; Small graphs - minimal movement
                                      ((< node-count 5) 4)    ; Medium graphs - small movement (was 8!)
                                      (t 6)))                  ; Large graphs - controlled movement (was 12!)
             (collision-spacing-buffer (+ base-collision-buffer max-collision-movement)))                ; Large graphs - generous buffer

        ;; Find maximum node extents
        (ht-each (lambda (node-id node)
                   (let ((x-extent (/ (dag-draw-node-x-size node) 2.0))
                         (y-extent (/ (dag-draw-node-y-size node) 2.0)))
                     (setq max-node-x-extent (max max-node-x-extent x-extent))
                     (setq max-node-y-extent (max max-node-y-extent y-extent))))
                 (dag-draw-graph-nodes graph))

        (let* (;; Adjust bounds to prevent grid coordinate clipping with collision spacing buffer
               (adjusted-min-x (- min-x max-node-x-extent collision-spacing-buffer))
               (adjusted-min-y (- min-y max-node-y-extent collision-spacing-buffer))
               (adjusted-max-x (+ max-x max-node-x-extent collision-spacing-buffer))
               (adjusted-max-y (+ max-y max-node-y-extent collision-spacing-buffer))
               (total-width (- adjusted-max-x adjusted-min-x))
               (total-height (- adjusted-max-y adjusted-min-y))
               ;; Handle empty graphs and ensure minimum grid size
               ;; DYNAMIC GRID SIZING: Compact multiplier for professional output
               ;; PHASE 1 FIX: Dramatically reduce grid bloat (was causing 108-line output)
               (dynamic-multiplier (cond
                                    ((= node-count 1) 1.2)    ; Single node - compact
                                    ((< node-count 4) 1.4)    ; Small graphs - reasonable
                                    ((< node-count 8) 1.6)    ; Medium graphs - moderate (was 2.7!)
                                    (t 1.8)))                  ; Large graphs - controlled (was 3.0!)
               ;; COLLISION SAFETY: Minimal padding for compact professional output
               ;; PHASE 1 FIX: Reduce excessive padding that was inflating grid size
               (extra-width-padding (max 10 (* node-count 2)))  ; Reduced from (* node-count 5)
               (extra-height-padding (max 5 (ceiling (* node-count 1.5))))  ; Ensure integer
               (grid-width (max 1 (+ extra-width-padding (ceiling (* (max 1 total-width) dag-draw-ascii-coordinate-scale dynamic-multiplier)))))
               (grid-height (max 1 (+ extra-height-padding (ceiling (* (max 1 total-height) dag-draw-ascii-coordinate-scale dynamic-multiplier)))))
               (grid (dag-draw--create-ascii-grid grid-width grid-height)))

          (message "BOUNDS-DEBUG: original min-x=%.1f min-y=%.1f → adjusted min-x=%.1f min-y=%.1f (collision-buffer=%.1f)" 
                   min-x min-y adjusted-min-x adjusted-min-y collision-spacing-buffer)

          ;; GKNV COMPLIANCE FIX: Convert GKNV world coordinates to ASCII grid coordinates
          ;; This is NOT recalculating positions - just converting coordinate systems for ASCII rendering
          (dag-draw--pre-calculate-final-node-positions graph grid adjusted-min-x adjusted-min-y scale)

          ;; PHASE 1B FIX: Recalculate grid size after collision detection to accommodate moved nodes
          (let* ((adjusted-positions (dag-draw-graph-adjusted-positions graph))
                 (final-bounds (dag-draw--calculate-final-bounds adjusted-positions)))
            (when final-bounds
              (let* ((final-min-x (nth 0 final-bounds))
                     (final-min-y (nth 1 final-bounds))  
                     (final-max-x (nth 2 final-bounds))
                     (final-max-y (nth 3 final-bounds))
                     (final-width (- final-max-x final-min-x))
                     (final-height (- final-max-y final-min-y))
                     (required-grid-width (max grid-width (+ 10 (ceiling final-width))))
                     (required-grid-height (max grid-height (+ 5 (ceiling final-height)))))
                ;; Recreate grid if it needs to be larger to accommodate collision-adjusted positions
                (when (or (> required-grid-width grid-width) (> required-grid-height grid-height))
                  (setq grid (dag-draw--create-ascii-grid required-grid-width required-grid-height))))))

          ;; ASCII COORDINATE CONTEXT: Temporarily normalize graph coordinates for ASCII rendering
          ;; This replaces the old normalization approach with a cleaner ASCII-specific layer
          (let ((original-coords (dag-draw--ascii-normalize-graph-coordinates graph ascii-context)))
            
            ;; COORDINATE SYSTEM FIX: Re-enable spline regeneration after ASCII coordinate normalization
            ;; This ensures arrows point to actual node positions in ASCII coordinate space
            (dag-draw--regenerate-splines-after-collision graph)

            ;; COORDINATE SYSTEM UNIFICATION: Use ASCII context for consistent coordinate system
            ;; This ensures both splines and nodes use the same normalized coordinate space
            (message "ASCII-COORD-DEBUG: Converting splines with ASCII context min-x=%.1f min-y=%.1f scale=%.3f" min-x min-y scale)
            (dag-draw--convert-splines-to-grid-coordinates graph min-x min-y scale)

            ;; GKNV Section 5.2 FIX: Draw nodes FIRST so arrows can properly integrate with boundaries
            (dag-draw--ascii-draw-nodes graph grid min-x min-y scale)

            ;; Draw edges LAST with arrows that can now terminate ON actual node boundaries
            (message "ASCII-COORD-DEBUG: Drawing edges with ASCII context min-x=%.1f min-y=%.1f scale=%.3f" min-x min-y scale)
            (dag-draw--ascii-draw-edges graph grid min-x min-y scale)
            
            ;; ASCII COORDINATE CONTEXT: Restore original coordinates to avoid affecting other rendering
            (dag-draw--ascii-restore-graph-coordinates graph original-coords))

          ;; Post-process junction characters to improve visual connections
          (dag-draw--post-process-junction-characters grid)

          ;; Convert grid to string
          (dag-draw--ascii-grid-to-string grid))))))

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

(defun dag-draw--detect-enhanced-spline-direction (optimized-points)
  "Detect final direction of spline path with enhanced accuracy."
  (when (and optimized-points (>= (length optimized-points) 2))
    (let* ((last-point (car (last optimized-points)))
           (second-last-point (car (last optimized-points 2)))
           ;; For better direction detection, look at more points if available
           (direction-points (if (>= (length optimized-points) 3)
                                 (last optimized-points 3)
                               optimized-points)))

      (if (>= (length direction-points) 2)
          (let* ((p1 (car (last direction-points 2)))
                 (p2 (car (last direction-points)))
                 (dx (- (dag-draw-point-x p2) (dag-draw-point-x p1)))
                 (dy (- (dag-draw-point-y p2) (dag-draw-point-y p1))))
            (dag-draw--detect-direction (dag-draw-point-x p1) (dag-draw-point-y p1)
                                        (dag-draw-point-x p2) (dag-draw-point-y p2)))
        'right))))

(defun dag-draw--draw-enhanced-horizontal-segment (grid x1 x2 y occupancy-map)
  "Draw horizontal segment with enhanced spline smoothing and intelligent collision handling."
  (let* ((x1 (round x1)) (x2 (round x2)) (y (round y))
         (grid-height (length grid))
         (grid-width (if (> grid-height 0) (length (aref grid 0)) 0))
         (start-x (min x1 x2))
         (end-x (max x1 x2)))

    (dotimes (i (1+ (- end-x start-x)))
      (let ((x (+ start-x i)))
        (when (and (>= x 0) (< x grid-width) (>= y 0) (< y grid-height))
          ;; Enhanced collision detection - can overwrite edges but respect nodes
          ;; GKNV FIX: Ensure integer coordinates for array access
          (let* ((int-x (round x))
                 (int-y (round y))
                 (current-char (aref (aref grid int-y) int-x)))
            (when (and (not (aref (aref occupancy-map int-y) int-x))  ; Not in a node
                       (memq current-char '(?\s ?│)))         ; Can overwrite space or vertical line
              (aset (aref grid int-y) int-x ?─))))))))

(defun dag-draw--draw-enhanced-vertical-segment (grid x y1 y2 occupancy-map)
  "Draw vertical segment with enhanced spline smoothing and intelligent collision handling."
  (let* ((x (round x)) (y1 (round y1)) (y2 (round y2))
         (grid-height (length grid))
         (grid-width (if (> grid-height 0) (length (aref grid 0)) 0))
         (start-y (min y1 y2))
         (end-y (max y1 y2)))

    (dotimes (i (1+ (- end-y start-y)))
      (let ((y (+ start-y i)))
        (when (and (>= x 0) (< x grid-width) (>= y 0) (< y grid-height))
          ;; Enhanced collision detection - can overwrite edges but respect nodes
          ;; GKNV FIX: Ensure integer coordinates for array access
          (let* ((int-x (round x))
                 (int-y (round y))
                 (current-char (aref (aref grid int-y) int-x)))
            (when (and (not (aref (aref occupancy-map int-y) int-x))  ; Not in a node
                       (memq current-char '(?\s ?─)))         ; Can overwrite space or horizontal line
              (dag-draw--ultra-safe-draw-char grid int-x int-y ?│ occupancy-map))))))))

(defun dag-draw--draw-enhanced-diagonal-segment (grid x1 y1 x2 y2 occupancy-map)
  "Draw diagonal segment with enhanced smooth approximation using intelligent path selection."
  (let* ((x1 (round x1)) (y1 (round y1)) (x2 (round x2)) (y2 (round y2))
         (grid-height (length grid))
         (grid-width (if (> grid-height 0) (length (aref grid 0)) 0))
         (dx (- x2 x1))
         (dy (- y2 y1))
         (abs-dx (abs dx))
         (abs-dy (abs dy)))

    ;; Choose best routing strategy based on diagonal characteristics
    (cond
     ;; Nearly horizontal diagonal - use horizontal-first L-shape with corner
     ((> abs-dx (* 2 abs-dy))
      (dag-draw--draw-horizontal-first-l-path grid x1 y1 x2 y2 occupancy-map))

     ;; Nearly vertical diagonal - use vertical-first L-shape with corner
     ((> abs-dy (* 2 abs-dx))
      (dag-draw--draw-vertical-first-l-path grid x1 y1 x2 y2 occupancy-map))

     ;; Balanced diagonal - use adaptive smooth approximation
     (t
      (dag-draw--draw-adaptive-smooth-diagonal grid x1 y1 x2 y2 occupancy-map)))))

(defun dag-draw--draw-horizontal-first-l-path (grid x1 y1 x2 y2 occupancy-map)
  "Draw L-shaped path: horizontal first, then vertical, with proper corner character."
  ;; Draw horizontal segment
  (dag-draw--draw-enhanced-horizontal-segment grid x1 x2 y1 occupancy-map)
  ;; Draw vertical segment
  (dag-draw--draw-enhanced-vertical-segment grid x2 y1 y2 occupancy-map)
  ;; Add corner character at junction
  (dag-draw--add-enhanced-corner-char grid x2 y1 x1 y1 x2 y2 occupancy-map))

(defun dag-draw--draw-vertical-first-l-path (grid x1 y1 x2 y2 occupancy-map)
  "Draw L-shaped path: vertical first, then horizontal, with proper corner character."
  ;; Draw vertical segment
  (dag-draw--draw-enhanced-vertical-segment grid x1 y1 y2 occupancy-map)
  ;; Draw horizontal segment
  (dag-draw--draw-enhanced-horizontal-segment grid x1 x2 y2 occupancy-map)
  ;; Add corner character at junction
  (dag-draw--add-enhanced-corner-char grid x1 y2 x1 y1 x2 y2 occupancy-map))

(defun dag-draw--draw-adaptive-smooth-diagonal (grid x1 y1 x2 y2 occupancy-map)
  "Draw smooth diagonal approximation using adaptive stepping."
  (let* ((dx (- x2 x1))
         (dy (- y2 y1))
         (steps (max (abs dx) (abs dy)))
         (x-step (if (> steps 0) (/ (float dx) steps) 0))
         (y-step (if (> steps 0) (/ (float dy) steps) 0)))

    (dotimes (i (1+ steps))
      (let* ((curr-x (round (+ x1 (* i x-step))))
             (curr-y (round (+ y1 (* i y-step))))
             (grid-height (length grid))
             (grid-width (if (> grid-height 0) (length (aref grid 0)) 0)))

        (when (and (>= curr-x 0) (< curr-x grid-width)
                   (>= curr-y 0) (< curr-y grid-height))
          ;; GKNV FIX: Ensure integer coordinates for array access
          (let* ((int-curr-x (round curr-x))
                 (int-curr-y (round curr-y))
                 (current-char (aref (aref grid int-curr-y) int-curr-x)))
            (when (and (not (aref (aref occupancy-map int-curr-y) int-curr-x))
                       (memq current-char '(?\s)))
              ;; Use appropriate line character based on local direction
              (let ((char (if (= i 0) ?│   ; Start with vertical
                            (if (= i steps) ?│  ; End with vertical
                              (dag-draw--choose-diagonal-char dx dy)))))
                (aset (aref grid int-curr-y) int-curr-x char)))))))))

(defun dag-draw--choose-diagonal-char (dx dy)
  "Choose appropriate character for diagonal direction."
  (cond
   ((and (> dx 0) (> dy 0)) ?│)  ; Down-right: prefer vertical
   ((and (> dx 0) (< dy 0)) ?│)  ; Up-right: prefer vertical
   ((and (< dx 0) (> dy 0)) ?│)  ; Down-left: prefer vertical
   ((and (< dx 0) (< dy 0)) ?│)  ; Up-left: prefer vertical
   (t ?─)))                      ; Default to horizontal

(defun dag-draw--add-enhanced-corner-char (grid corner-x corner-y from-x from-y to-x to-y occupancy-map)
  "Add appropriate corner character at path junction with enhanced detection."
  ;; GKNV FIX: Ensure integer coordinates for array access
  (let* ((int-corner-x (round corner-x))
         (int-corner-y (round corner-y))
         (int-from-x (round from-x))
         (int-from-y (round from-y))
         (int-to-x (round to-x))
         (int-to-y (round to-y))
         (grid-height (length grid))
         (grid-width (if (> grid-height 0) (length (aref grid 0)) 0)))

    (when (and (>= int-corner-x 0) (< int-corner-x grid-width)
               (>= int-corner-y 0) (< int-corner-y grid-height)
               (not (aref (aref occupancy-map int-corner-y) int-corner-x)))

      ;; Determine corner character based on incoming and outgoing directions
      (let* ((from-dx (- int-corner-x int-from-x))
             (from-dy (- int-corner-y int-from-y))
             (to-dx (- int-to-x int-corner-x))
             (to-dy (- int-to-y int-corner-y))
             (corner-char (dag-draw--select-corner-character from-dx from-dy to-dx to-dy)))

        (when corner-char
          (aset (aref grid int-corner-y) int-corner-x corner-char))))))

(defun dag-draw--select-corner-character (from-dx from-dy to-dx to-dy)
  "Select appropriate corner character based on entry and exit directions."
  (cond
   ;; Horizontal in, vertical out
   ((and (not (= from-dx 0)) (= from-dy 0) (= to-dx 0) (not (= to-dy 0)))
    (if (and (> from-dx 0) (> to-dy 0)) ?┐    ; Right in, down out
      (if (and (> from-dx 0) (< to-dy 0)) ?┘  ; Right in, up out
        (if (and (< from-dx 0) (> to-dy 0)) ?┌ ; Left in, down out
          ?└))))                             ; Left in, up out

   ;; Vertical in, horizontal out
   ((and (= from-dx 0) (not (= from-dy 0)) (not (= to-dx 0)) (= to-dy 0))
    (if (and (> from-dy 0) (> to-dx 0)) ?└    ; Down in, right out
      (if (and (> from-dy 0) (< to-dx 0)) ?┘  ; Down in, left out
        (if (and (< from-dy 0) (> to-dx 0)) ?┌ ; Up in, right out
          ?┐))))                             ; Up in, left out

   ;; Default: no corner needed
   (t nil)))

(provide 'dag-draw-render)

;;; dag-draw-render.el ends here
