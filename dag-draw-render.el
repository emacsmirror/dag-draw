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
(require 'dag-draw-pass4-splines)
(require 'dag-draw-svg)
(require 'dag-draw-dot)
(require 'dag-draw-ascii-grid)
(require 'dag-draw-ports)
(require 'dag-draw-ascii-nodes)
(require 'dag-draw-ascii-edges)
(require 'dag-draw-pass3-positioning)

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
    
    ;; Check coordinate mode to decide rendering approach
    (let ((coordinate-mode (or (dag-draw-graph-coordinate-mode graph) 'high-res)))
      (if (eq coordinate-mode 'ascii)
          ;; ASCII-native mode: coordinates are already in grid units, no conversion needed
          (dag-draw--render-ascii-native graph)
        ;; High-res mode: convert GKNV coordinates to ASCII grid
        (dag-draw--convert-gknv-to-ascii-grid graph)))))

(defun dag-draw--render-ascii-native (graph)
  "Render graph with ASCII-native coordinates - no scale conversion needed.
Coordinates are already in grid units from ASCII-native GKNV positioning."
  
  ;; Calculate grid bounds directly from ASCII coordinates
  (let* ((nodes (ht-values (dag-draw-graph-nodes graph)))
         (min-x (apply #'min (mapcar (lambda (n) (or (dag-draw-node-x-coord n) 0)) nodes)))
         (max-x (apply #'max (mapcar (lambda (n) (+ (or (dag-draw-node-x-coord n) 0)
                                                     (length (dag-draw-node-label n)))) nodes)))
         (min-y (apply #'min (mapcar (lambda (n) (or (dag-draw-node-y-coord n) 0)) nodes)))
         (max-y (apply #'max (mapcar (lambda (n) (+ (or (dag-draw-node-y-coord n) 0) 3)) nodes)))
         
         ;; Create grid with padding
         (grid-width (max 20 (+ (- max-x min-x) 10)))
         (grid-height (max 10 (+ (- max-y min-y) 5)))
         (grid (dag-draw--create-ascii-grid grid-width grid-height)))
    
    (message "\n=== ASCII-NATIVE RENDERING ===")
    (message "Grid bounds: (%d,%d) to (%d,%d)" min-x min-y max-x max-y)
    (message "Grid size: %dx%d" grid-width grid-height)
    
    ;; Draw nodes directly using ASCII coordinates
    (dolist (node nodes)
      (let ((x (- (or (dag-draw-node-x-coord node) 0) min-x))
            (y (- (or (dag-draw-node-y-coord node) 0) min-y))
            (label (dag-draw-node-label node))
            (width (+ (length (dag-draw-node-label node)) 4))  ; Label + padding
            (height 3))  ; Standard node height
        (dag-draw--draw-node-box grid x y width height label)))
    
    ;; Draw edges directly using ASCII coordinates  
    (dolist (edge (dag-draw-graph-edges graph))
      (let* ((from-node (dag-draw-get-node graph (dag-draw-edge-from-node edge)))
             (to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge)))
             (from-x (- (or (dag-draw-node-x-coord from-node) 0) min-x))
             (from-y (- (or (dag-draw-node-y-coord from-node) 0) min-y))
             (to-x (- (or (dag-draw-node-x-coord to-node) 0) min-x))
             (to-y (- (or (dag-draw-node-y-coord to-node) 0) min-y)))
        (dag-draw--draw-simple-edge grid from-x from-y to-x to-y)))
    
    ;; Convert grid to string
    (dag-draw--ascii-grid-to-string grid)))

(defun dag-draw--convert-gknv-to-ascii-grid (graph)
  "Pure conversion of GKNV final coordinates to ASCII grid.
Does NOT modify graph coordinates or regenerate splines."

  (let* (;; Step 1: Calculate conversion parameters from GKNV final coordinates
         (bounds (dag-draw-get-graph-bounds graph))
         (min-x (nth 0 bounds))
         (min-y (nth 1 bounds))
         (max-x (nth 2 bounds))
         (max-y (nth 3 bounds))
         
         ;; Step 2: Calculate optimal scale dynamically based on graph complexity
         ;; Default target dimensions for ASCII rendering (reasonable terminal size)
         (target-width 80)
         (target-height 24)
         (scale (dag-draw--calculate-optimal-ascii-scale graph target-width target-height))

         ;; Step 3: Calculate grid size needed for GKNV coordinates
         (width (- max-x min-x))
         (height (- max-y min-y))
         (grid-width (max 20 (+ 10 (ceiling (* width scale)))))
         (grid-height (max 10 (+ 5 (ceiling (* height scale)))))

         ;; Step 3: Create ASCII grid
         (grid (dag-draw--create-ascii-grid grid-width grid-height)))

    (message "\n=== GKNV-COMPLIANT ASCII CONVERSION ===")
    (message "GKNV bounds: (%.1f,%.1f) to (%.1f,%.1f)" min-x min-y max-x max-y)
    (message "ASCII grid: %dx%d, scale=%.3f" grid-width grid-height scale)

    ;; Step 4: Draw nodes using GKNV final coordinates (FIRST - establish boundaries)
    (dag-draw--draw-nodes-gknv-compliant graph grid min-x min-y scale)

    ;; Step 5: Draw edges using GKNV clipped splines (SECOND - terminate at boundaries)
    (dag-draw--draw-edges-gknv-compliant graph grid min-x min-y scale)

    ;; Step 6: Enhance boundary connections for better visual clarity (disabled due to scoping issue)
    ;; TODO: Fix boundary enhancement function scoping issue
    ;; (dag-draw--enhance-boundary-connections grid graph min-x min-y scale)

    ;; Step 7: Convert grid to string
    (dag-draw--ascii-grid-to-string grid)))

(defun dag-draw--draw-nodes-gknv-compliant (graph grid min-x min-y scale)
  "Draw nodes using GKNV final coordinates without modification."

  (ht-each (lambda (node-id node)
             (let* (;; GKNV Pass 3 Authority: Use protected coordinates from adjusted-positions
                    ;; Section 4: "The third pass finds optimal coordinates for nodes"
                    (adjusted-positions (dag-draw-graph-adjusted-positions graph))
                    (adjusted-coords (and adjusted-positions (ht-get adjusted-positions node-id)))
                    (world-x (if adjusted-coords
                                 (nth 0 adjusted-coords)  ; Use protected GKNV coordinates
                               (or (dag-draw-node-x-coord node) 0)))  ; Fallback
                    (world-y (if adjusted-coords
                                 (nth 1 adjusted-coords)  ; Use protected GKNV coordinates
                               (or (dag-draw-node-y-coord node) 0)))  ; Fallback
                    (world-width (dag-draw-node-x-size node))
                    (world-height (dag-draw-node-y-size node))
                    (node-label (dag-draw-node-label node))

                    ;; Convert to grid coordinates
                    (grid-center-x (dag-draw--world-to-grid-coord world-x min-x scale))
                    (grid-center-y (dag-draw--world-to-grid-coord world-y min-y scale))
                    (grid-width (dag-draw--world-to-grid-size world-width scale))
                    (grid-height (dag-draw--world-to-grid-size world-height scale))

                    ;; Calculate grid position (top-left corner)
                    (grid-x (round (- grid-center-x (/ grid-width 2))))
                    (grid-y (round (- grid-center-y (/ grid-height 2)))))

               (message "GKNV-NODE: %s world(%.1f,%.1f) → grid(%d,%d) size(%dx%d)"
                        node-id world-x world-y grid-x grid-y grid-width grid-height)

               ;; Draw node box at calculated position
               (dag-draw--draw-node-box grid grid-x grid-y grid-width grid-height node-label)))
           (dag-draw-graph-nodes graph)))

(defun dag-draw--avoid-ascii-collision (x y width height drawn-nodes)
  "Adjust node position to avoid collision with already drawn nodes.
Returns (adjusted-x adjusted-y) that doesn't overlap with drawn-nodes."

  (let ((current-rect (list x y (+ x width -1) (+ y height -1)))
        (min-separation 3))  ; Minimum 3-character separation between nodes

    ;; Check for collisions with already drawn nodes
    (dolist (drawn-rect drawn-nodes)
      (when (dag-draw--ascii-rectangles-overlap current-rect drawn-rect)
        ;; Collision detected - move to the right with separation
        (let ((collision-right (nth 2 drawn-rect)))
          (setq x (+ collision-right min-separation))
          (setq current-rect (list x y (+ x width -1) (+ y height -1))))))

    (list x y)))

(defun dag-draw--ascii-rectangles-overlap (rect1 rect2)
  "Check if two rectangles overlap in ASCII grid space.
Each rect is (left top right bottom)."
  (let ((x1-left (nth 0 rect1)) (y1-top (nth 1 rect1))
        (x1-right (nth 2 rect1)) (y1-bottom (nth 3 rect1))
        (x2-left (nth 0 rect2)) (y2-top (nth 1 rect2))
        (x2-right (nth 2 rect2)) (y2-bottom (nth 3 rect2)))

    ;; Rectangles overlap if they overlap in both X and Y dimensions
    (and (<= x1-left x2-right) (<= x2-left x1-right)
         (<= y1-top y2-bottom) (<= y2-top y1-bottom))))


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

         ;; Use actual spline endpoints for ports (preserves flexible positioning)
         (spline-points (dag-draw-edge-spline-points edge))
         (from-port (if (and spline-points (> (length spline-points) 0))
                        ;; Use actual spline start point
                        (let* ((start-point (car spline-points))
                               (start-x (dag-draw--world-to-grid-coord (dag-draw-point-x start-point) min-x scale))
                               (start-y (dag-draw--world-to-grid-coord (dag-draw-point-y start-point) min-y scale)))
                          (list (round start-x) (round start-y)))
                      ;; Fallback to boundary calculation
                      (dag-draw--calculate-boundary-port from-grid-center-x from-grid-center-y
                                                         from-grid-width from-grid-height 'bottom)))
         (to-port (if (and spline-points (> (length spline-points) 0))
                      ;; Use actual spline end point
                      (let* ((end-point (car (last spline-points)))
                             (end-x (dag-draw--world-to-grid-coord (dag-draw-point-x end-point) min-x scale))
                             (end-y (dag-draw--world-to-grid-coord (dag-draw-point-y end-point) min-y scale)))
                        (list (round end-x) (round end-y)))
                    ;; Fallback to boundary calculation
                    (dag-draw--calculate-boundary-port to-grid-center-x to-grid-center-y
                                                       to-grid-width to-grid-height 'top))))

    (message "GKNV-PORTS: %s port(%d,%d) → %s port(%d,%d)"
             (dag-draw-edge-from-node edge) (nth 0 from-port) (nth 1 from-port)
             (dag-draw-edge-to-node edge) (nth 0 to-port) (nth 1 to-port))

    ;; Draw line between proper ports (splines are pre-clipped to boundaries)
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

      ;; Draw middle rows with label (supports multiline text)
      (let ((label-lines (split-string label "\n")))  ; Split multiline labels
        (dotimes (row (- height 2))
          (let ((actual-row (+ y row 1))
                (current-line (if (< row (length label-lines))
                                (nth row label-lines)
                              "")))  ; Empty string for rows without text
            ;; Left border
            (dag-draw--set-char grid x actual-row ?│)
            ;; Content area with proper multiline text rendering
            (dotimes (col (- width 2))
              (let ((char-pos (+ x col 1)))
                (if (< col (length current-line))
                    (dag-draw--set-char grid char-pos actual-row (aref current-line col))
                  (dag-draw--set-char grid char-pos actual-row ?\s))))
            ;; Right border
            (dag-draw--set-char grid (+ x width -1) actual-row ?│))))

      ;; Draw bottom border
      (dotimes (i width)
        (dag-draw--set-char grid (+ x i) (+ y height -1)
                            (cond ((= i 0) ?└)
                                  ((= i (1- width)) ?┘)
                                  (t ?─)))))))


(defun dag-draw--draw-simple-line (grid x1 y1 x2 y2)
  "Draw a simple line from (x1,y1) to (x2,y2) with arrow.
GKNV-compliant: splines are now pre-clipped to boundaries, so simple drawing works."
  (let ((grid-height (length grid))
        (grid-width (if (> (length grid) 0) (length (aref grid 0)) 0)))

    ;; Draw vertical line first  
    (when (not (= y1 y2))
      (let ((start-y (min y1 y2))
            (end-y (max y1 y2)))
        (dotimes (i (1+ (- end-y start-y)))
          (let ((y (+ start-y i)))
            (when (and (>= x1 0) (< x1 grid-width) (>= y 0) (< y grid-height))
              (dag-draw--set-char grid x1 y ?│))))))

    ;; Draw horizontal line
    (when (not (= x1 x2))
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

;; GKNV-Compliant Rendering - No Hollow Routing Workarounds Needed
;; Splines are now pre-clipped to boundaries, enabling simple line drawing

(defun dag-draw--set-char (grid x y char)
  "Safely set character in grid at position (x,y)."
  (let ((grid-height (length grid))
        (grid-width (if (> (length grid) 0) (length (aref grid 0)) 0)))
    (when (and (>= x 0) (< x grid-width) (>= y 0) (< y grid-height))
      (aset (aref grid y) x char))))


;;; SVG and DOT rendering functions are provided by their respective modules

;;; Enhanced spline segment drawing functions for Phase C improvements

(defun dag-draw--draw-simple-edge (grid from-x from-y to-x to-y)
  "Draw a simple edge from (FROM-X,FROM-Y) to (TO-X,TO-Y) in GRID.
This is a basic ASCII edge drawing for ASCII-native coordinate mode."
  
  ;; Calculate port positions (center bottom of from-node, center top of to-node)
  (let* ((from-port-x from-x)
         (from-port-y (+ from-y 3))  ; Bottom of from-node
         (to-port-x to-x)
         (to-port-y to-y)            ; Top of to-node
         ;; Calculate inter-rank routing position (midway between ranks)
         (routing-y (- to-port-y 1))) ; One row above destination node
    
    ;; Draw vertical line down from from-node to routing level
    (when (< from-port-y routing-y)
      (let ((y from-port-y))
        (while (< y routing-y)
          (dag-draw--set-grid-char grid from-port-x y ?│)
          (setq y (1+ y)))))
    
    ;; Draw horizontal line at inter-rank routing level
    (when (/= from-port-x to-port-x)
      (let ((start-x (min from-port-x to-port-x))
            (end-x (max from-port-x to-port-x)))
        (while (<= start-x end-x)
          (dag-draw--set-grid-char grid start-x routing-y ?─)
          (setq start-x (1+ start-x)))))
    
    ;; Draw final vertical segment down to destination node
    (when (< routing-y to-port-y)
      (dag-draw--set-grid-char grid to-port-x routing-y ?│))
    
    ;; Draw arrow at destination
    (dag-draw--set-grid-char grid to-port-x to-port-y ?▼)))

(defun dag-draw--set-grid-char (grid x y char)
  "Set character at position (X,Y) in GRID to CHAR, with bounds checking."
  (when (and (>= x 0) (< x (length (aref grid 0)))
             (>= y 0) (< y (length grid)))
    (aset (aref grid y) x char)))

(provide 'dag-draw-render)

;;; dag-draw-render.el ends here
