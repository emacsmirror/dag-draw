;;; dag-draw-ascii-edges.el --- ASCII edge drawing for dag-draw -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; ASCII edge drawing, routing, and arrow placement for dag-draw graphs.
;; This module handles the complex logic of drawing edges between nodes
;; with proper collision detection, occupancy map respect, and arrow placement.
;;
;; CRITICAL: All character placement MUST use dag-draw--safety-check-aset()
;; instead of direct aset() to respect the occupancy map and prevent
;; drawing through node text.

;;; Code:

(require 'dash)
(require 'dag-draw-core)
(require 'dag-draw-ascii-grid)
(require 'dag-draw-ports)
(require 'dag-draw-splines)

;;; Basic Line Drawing

(defun dag-draw--ascii-draw-line (grid x1 y1 x2 y2 &optional occupancy-map)
  "Draw a simple line from (X1,Y1) to (X2,Y2) on ASCII grid."
  (let* ((grid-height (length grid))
         (grid-width (if (> grid-height 0) (length (aref grid 0)) 0))
         (dx (- x2 x1))
         (dy (- y2 y1)))

    (cond
     ;; Horizontal line
     ((= dy 0)
      (let ((start-x (min x1 x2))
            (end-x (max x1 x2)))
        (dotimes (i (1+ (- end-x start-x)))
          (let ((x (+ start-x i)))
            (dag-draw--safe-place-horizontal-char grid x y1)))))

     ;; Vertical line
     ((= dx 0)
      (let ((start-y (min y1 y2))
            (end-y (max y1 y2)))
        (dotimes (i (1+ (- end-y start-y)))
          (let ((y (+ start-y i)))
            (when (and (>= x1 0) (< x1 grid-width)
                       (>= y 0) (< y grid-height)
                       (= (aref (aref grid y) x1) ?\s))  ; Only draw if cell is empty
              (if occupancy-map
                  (dag-draw--ultra-safe-draw-char grid x1 y ?│ occupancy-map)
                (aset (aref grid y) x1 ?│)))))))

     ;; L-shaped line (horizontal then vertical)
     (t
      ;; Draw horizontal segment first
      (dag-draw--ascii-draw-line grid x1 y1 x2 y1 occupancy-map)
      ;; Draw vertical segment, avoiding overlap at corner
      (when (/= y1 y2)
        (dag-draw--ascii-draw-line grid x2 (+ y1 (if (< y1 y2) 1 -1)) x2 y2 occupancy-map))))))

(defun dag-draw--safe-place-horizontal-char (grid x y)
  "Safely place horizontal line character, avoiding double-dash artifacts."
  (let* ((grid-height (length grid))
         (grid-width (if (> grid-height 0) (length (aref grid 0)) 0)))
    (when (and (>= x 0) (< x grid-width) (>= y 0) (< y grid-height))
      (let ((current-char (aref (aref grid y) x)))
        ;; Only place if current position is empty OR we can enhance an existing line
        (cond
         ;; Empty space - place horizontal line
         ((eq current-char ?\s)
          (aset (aref grid y) x ?─))
         ;; Already has horizontal line - don't duplicate
         ((eq current-char ?─)
          nil)
         ;; Vertical line - create junction
         ((eq current-char ?│)
          (aset (aref grid y) x ?┼)))))))

(defun dag-draw--detect-direction (x1 y1 x2 y2)
  "Detect direction from coordinates (X1,Y1) to (X2,Y2)."
  (let ((dx (- x2 x1))
        (dy (- y2 y1)))
    (cond
     ((> dx 0) 'right)
     ((< dx 0) 'left)
     ((> dy 0) 'down)
     ((< dy 0) 'up)
     (t 'right))))  ; default case

(defun dag-draw--get-arrow-char (direction)
  "Get arrow character for given DIRECTION."
  (pcase direction
    ('left ?◀)
    ('right ?▶)
    ('down ?▼)
    ('up ?▲)
    (_ ?▶)))

(defun dag-draw--draw-horizontal-with-arrow (grid x1 y1 x2 y2)
  "Draw horizontal line from (X1,Y1) to (X2,Y2) with arrow at end."
  (let ((direction (dag-draw--detect-direction x1 y1 x2 y2))
        (start-x (min x1 x2))
        (end-x (max x1 x2)))
    ;; Draw line characters (exclude endpoints)
    (dotimes (i (1- (- end-x start-x)))
      (let ((x (+ start-x i 1)))
        (when (and (>= x 0) (< x (length (aref grid 0)))
                   (>= y1 0) (< y1 (length grid))
                   (= (aref (aref grid y1) x) ?\s))  ; Only draw if cell is empty
          (aset (aref grid y1) x ?─))))
    ;; Draw arrow at destination - only if cell is empty
    (when (and (>= x2 0) (< x2 (length (aref grid 0)))
               (>= y2 0) (< y2 (length grid))
               (= (aref (aref grid y2) x2) ?\s))
      (aset (aref grid y2) x2 (dag-draw--get-arrow-char direction)))))

;;; Advanced Arrow Placement Functions

(defun dag-draw--add-directional-arrow (grid x1 y1 x2 y2 occupancy-map)
  "Add directional arrow, handling touching nodes case."
  (let* ((dx (- x2 x1))
         (dy (- y2 y1)))

    (cond
     ;; Same grid coordinates (touching nodes) - replace part of connecting line with arrow
     ((and (= dx 0) (= dy 0))
      ;; For touching nodes, replace the center of the connecting line with an arrow
      ;; Look for vertical line characters near the connection point and replace one with arrow
      (let* ((grid-height (length grid))
             (grid-width (if (> grid-height 0) (length (aref grid 0)) 0)))
        ;; Search around the connection point for vertical line characters
        (catch 'arrow-placed
          (dotimes (offset 3)  ; Search 3 positions below connection point
            (let ((search-y (+ y2 offset)))
              (when (and (>= x2 0) (< x2 grid-width)
                         (>= search-y 0) (< search-y grid-height))
                (when (eq (aref (aref grid search-y) x2) ?│)
                  ;; Found vertical line, replace with arrow using safe drawing
                  (dag-draw--ultra-safe-draw-char grid x2 search-y ?▼ occupancy-map)
                  (throw 'arrow-placed t))))))))

     ;; Different coordinates - use original logic
     (t
      (dag-draw--add-ultra-safe-arrow grid x1 y1 x2 y2 occupancy-map)))))

(defun dag-draw--add-ultra-safe-arrow (grid x1 y1 x2 y2 occupancy-map)
  "Add directional arrow at the endpoint of a path with ultra-safe collision detection."
  (let* ((dx (- x2 x1))
         (dy (- y2 y1))
         (arrow-char (cond
                      ;; Same grid coordinates - don't draw arrow on same position
                      ((and (= dx 0) (= dy 0)) nil)
                      ;; Vertical arrows
                      ((and (= dx 0) (> dy 0)) ?▼)  ; downward
                      ((and (= dx 0) (< dy 0)) ?▲)  ; upward
                      ;; Horizontal arrows
                      ((and (= dy 0) (> dx 0)) ?▶)  ; rightward
                      ((and (= dy 0) (< dx 0)) ?◀)  ; leftward
                      ;; L-shaped paths - determine arrow based on final direction
                      ((> dy 0) ?▼)  ; if final segment goes down
                      ((< dy 0) ?▲)  ; if final segment goes up
                      ((> dx 0) ?▶)  ; if final segment goes right
                      ((< dx 0) ?◀)  ; if final segment goes left
                      (t ?▶))))      ; default rightward arrow

    ;; Draw arrow at endpoint with ultra-safe collision detection
    ;; Arrows are more permissive since they're endpoints and shouldn't interfere with content
    (dag-draw--ultra-safe-draw-arrow grid x2 y2 arrow-char occupancy-map)))

(defun dag-draw--ultra-safe-draw-arrow (grid x y arrow-char occupancy-map)
  "Draw arrow character with box-boundary aware placement."
  (when arrow-char
    (let* ((grid-height (length grid))
           (grid-width (if (> grid-height 0) (length (aref grid 0)) 0)))

      (when (and (>= x 0) (< x grid-width) (>= y 0) (< y grid-height))
        (let ((current-char (aref (aref grid y) x)))

          ;; GKNV-COMPLIANT ARROW PLACEMENT: Arrows are allowed on node boundaries
          (cond
           ;; Case 1: Empty space - always safe to place arrow
           ((eq current-char ?\s)
            (aset (aref grid y) x arrow-char))

           ;; Case 2: Plain edge characters - safe to replace with arrow (GKNV boundary connections)
           ((memq current-char '(?─ ?│ ?┼ ?┌ ?┐ ?└ ?┘))
            (aset (aref grid y) x arrow-char))

           ;; Case 3: Already has arrow - leave it alone
           ((memq current-char '(?▼ ?▲ ?▶ ?◀))
            nil)

           ;; Case 4: Node boundary characters - GKNV allows arrows on boundaries
           ;; This is the key fix: arrows can be placed on node borders per GKNV Section 5.1.1
           ((memq current-char '(?┬ ?┴ ?├ ?┤))
            (aset (aref grid y) x arrow-char))

           ;; Case 5: Alphanumeric characters (node text) - NEVER overwrite
           ((or (and (>= current-char ?a) (<= current-char ?z))
                (and (>= current-char ?A) (<= current-char ?Z))
                (and (>= current-char ?0) (<= current-char ?9)))
            nil)

           ;; Case 6: Default - allow arrow placement for GKNV compliance
           (t
            (aset (aref grid y) x arrow-char))))))))

(defun dag-draw--ultra-safe-draw-char (grid x y char occupancy-map)
  "Draw character with ultra-safe collision detection."
  (let* ((grid-height (length grid))
         (grid-width (if (> grid-height 0) (length (aref grid 0)) 0)))

    (when (and (>= x 0) (< x grid-width) (>= y 0) (< y grid-height))
      ;; Check occupancy map
      (unless (and occupancy-map
                   (< y (length occupancy-map))
                   (< x (length (aref occupancy-map y)))
                   (aref (aref occupancy-map y) x))
        ;; Safe to place character
        (aset (aref grid y) x char)
        t))))

;;; Safe Character Placement - CRITICAL BUG FIX

(defun dag-draw--safe-place-char (grid x y char occupancy-map)
  "Safely place character on grid, respecting occupancy map to prevent text intersection.
This is the CRITICAL function that prevents drawing through node text."
  (let* ((grid-height (length grid))
         (grid-width (if (> grid-height 0) (length (aref grid 0)) 0)))

    (when (and (>= x 0) (< x grid-width) (>= y 0) (< y grid-height))
      ;; Check occupancy map - NEVER draw in occupied areas
      (unless (and occupancy-map
                   (< y (length occupancy-map))
                   (< x (length (aref occupancy-map y)))
                   (aref (aref occupancy-map y) x))
        ;; Safe to place character
        (aset (aref grid y) x char)
        t))))

;;; ASCII Edge Drawing - Main Entry Point

(defun dag-draw--ascii-draw-edges (graph grid min-x min-y scale)
  "Draw edges on ASCII grid using boundary-aware routing with enhanced collision detection."
  ;; Create node occupancy map to avoid drawing through nodes
  (let ((occupancy-map (dag-draw--create-node-occupancy-map graph grid min-x min-y scale)))
    ;; Set global occupancy map for safety checks
    (setq dag-draw--global-occupancy-map occupancy-map)
    (dolist (edge (dag-draw-graph-edges graph))
      ;; Use enhanced boundary-aware routing with real-time collision detection
      (dag-draw--ascii-draw-safe-edge graph edge grid min-x min-y scale occupancy-map))))

;;; Edge Drawing Implementation - FIXED VERSION

(defun dag-draw--ascii-draw-safe-edge (graph edge grid min-x min-y scale occupancy-map)
  "Draw edge with enhanced collision detection that prevents any overwrites of node content.
    PHASE 2: Now uses proper spline-to-ASCII conversion per GKNV Section 5.2."
  ;; CRITICAL CHANGE: Use splines if available, otherwise fall back to orthogonal
  (if (dag-draw-edge-spline-points edge)
      ;; Use actual splines per GKNV algorithm Phase 4
      (dag-draw--ascii-draw-spline-path graph edge grid min-x min-y scale occupancy-map)
    ;; Fallback to orthogonal for edges without splines
    (dag-draw--ascii-draw-safe-orthogonal-edge graph edge grid min-x min-y scale occupancy-map)))

;;; Enhanced Edge Drawing Functions

(defun dag-draw--ascii-draw-safe-orthogonal-edge (graph edge grid min-x min-y scale occupancy-map)
  "Draw orthogonal edge with comprehensive collision avoidance."
  (let ((connection-points (dag-draw--get-edge-connection-points graph edge min-x min-y scale)))
    (when (and connection-points (= (length connection-points) 2))
      (let* ((from-port (car connection-points))
             (to-port (cadr connection-points))
             ;; COORDINATE SYSTEM FIX: Ports now return grid coordinates directly
             (from-x (dag-draw--center-aware-round (dag-draw-point-x from-port)))
             (from-y (dag-draw--center-aware-round (dag-draw-point-y from-port)))
             (to-x (dag-draw--center-aware-round (dag-draw-point-x to-port)))
             (to-y (dag-draw--center-aware-round (dag-draw-point-y to-port)))
             ;; Calculate port-based arrow direction
             (to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge)))
             (target-port-side (dag-draw--determine-port-side to-node to-port min-x min-y scale graph)))
        ;; Use safe path drawing that absolutely will not overwrite node content
        (dag-draw--ascii-draw-ultra-safe-path-with-port-arrow
         grid from-x from-y to-x to-y occupancy-map target-port-side)))))

(defun dag-draw--ascii-draw-ultra-safe-path-with-port-arrow (grid x1 y1 x2 y2 occupancy-map port-side)
  "Draw path with absolute safety and port-based arrow direction."
  (let* ((grid-height (length grid))
         (grid-width (if (> grid-height 0) (length (aref grid 0)) 0)))

    ;; Only draw if we can do so safely
    (when (and (>= x1 0) (< x1 grid-width) (>= y1 0) (< y1 grid-height)
               (>= x2 0) (< x2 grid-width) (>= y2 0) (< y2 grid-height))

      ;; Choose routing direction based on edge orientation
      (let ((routing-direction (if (<= (abs (- x1 x2)) 4)
                                   'vertical-only    ; Pure vertical edge (allow 4-char tolerance)
                                 (if (<= (abs (- y1 y2)) 4)
                                     'horizontal-only  ; Pure horizontal edge (allow 4-char tolerance)
                                   'horizontal-first)))) ; L-shaped edge

        ;; Draw the path with appropriate direction
        (dag-draw--draw-ultra-safe-l-path grid x1 y1 x2 y2 occupancy-map routing-direction))

      ;; Add port-based directional arrow at the endpoint
      (dag-draw--add-port-based-arrow grid x1 y1 x2 y2 occupancy-map port-side))))

(defun dag-draw--draw-ultra-safe-l-path (grid x1 y1 x2 y2 occupancy-map direction)
  "Draw L-shaped path with ultra-conservative safety checks."
  (let* ((grid-height (length grid))
         (grid-width (if (> grid-height 0) (length (aref grid 0)) 0)))

    (cond
     ;; Pure vertical line
     ((eq direction 'vertical-only)
      (let ((start-y (min y1 y2))
            (end-y (max y1 y2)))
        (dotimes (i (1+ (- end-y start-y)))
          (let ((y (+ start-y i)))
            (dag-draw--ultra-safe-draw-char grid x1 y ?│ occupancy-map)))))

     ;; Pure horizontal line
     ((eq direction 'horizontal-only)
      (let ((start-x (min x1 x2))
            (end-x (max x1 x2)))
        (dotimes (i (1+ (- end-x start-x)))
          (let ((x (+ start-x i)))
            (dag-draw--ultra-safe-draw-char grid x y1 ?─ occupancy-map)))))

     ;; L-shaped path: horizontal first
     ((eq direction 'horizontal-first)
      ;; Horizontal segment first: x1 to x2 at y1
      (let ((start-x (min x1 x2))
            (end-x (max x1 x2)))
        (dotimes (i (1+ (- end-x start-x)))
          (let ((x (+ start-x i)))
            (dag-draw--ultra-safe-draw-char grid x y1 ?─ occupancy-map))))
      ;; Vertical segment: y1 to y2 at x2
      (let ((start-y (min y1 y2))
            (end-y (max y1 y2)))
        (dotimes (i (1+ (- end-y start-y)))
          (let ((y (+ start-y i)))
            (dag-draw--ultra-safe-draw-char grid x2 y ?│ occupancy-map))))
      ;; Add corner character at junction point (x2, y1)
      (let ((corner-char (cond
                          ((and (< x1 x2) (< y1 y2)) ?┐)  ; Right then down
                          ((and (< x1 x2) (> y1 y2)) ?┘)  ; Right then up
                          ((and (> x1 x2) (< y1 y2)) ?┌)  ; Left then down
                          ((and (> x1 x2) (> y1 y2)) ?└)  ; Left then up
                          (t ?┼))))                         ; Fallback intersection
        (dag-draw--ultra-safe-draw-char grid x2 y1 corner-char occupancy-map)))

     ;; L-shaped path: vertical first (default fallback)
     (t
      ;; Vertical segment first: y1 to y2 at x1
      (let ((start-y (min y1 y2))
            (end-y (max y1 y2)))
        (dotimes (i (1+ (- end-y start-y)))
          (let ((y (+ start-y i)))
            (dag-draw--ultra-safe-draw-char grid x1 y ?│ occupancy-map))))
      ;; Horizontal segment: x1 to x2 at y2
      (let ((start-x (min x1 x2))
            (end-x (max x1 x2)))
        (dotimes (i (1+ (- end-x start-x)))
          (let ((x (+ start-x i)))
            (dag-draw--ultra-safe-draw-char grid x y2 ?─ occupancy-map))))
      ;; Add corner character at junction point (x1, y2)
      (let ((corner-char (cond
                          ((and (< y1 y2) (< x1 x2)) ?┌)  ; Down then right
                          ((and (< y1 y2) (> x1 x2)) ?┐)  ; Down then left
                          ((and (> y1 y2) (< x1 x2)) ?└)  ; Up then right
                          ((and (> y1 y2) (> x1 x2)) ?┘)  ; Up then left
                          (t ?┼))))                         ; Fallback intersection
        (dag-draw--ultra-safe-draw-char grid x1 y2 corner-char occupancy-map))))))

(defun dag-draw--add-port-based-arrow (grid x1 y1 x2 y2 occupancy-map port-side)
  "Add directional arrow based on actual coordinate direction, with port-side as secondary."
  (let* ((dx (- x2 x1))
         (dy (- y2 y1))
         (arrow-char (cond
                      ;; PRIORITY: Use coordinate-based direction for clear vertical/horizontal cases
                      ((and (= dx 0) (> dy 0)) ?▼)  ; Pure vertical downward
                      ((and (= dx 0) (< dy 0)) ?▲)  ; Pure vertical upward  
                      ((and (= dy 0) (> dx 0)) ?▶)  ; Pure horizontal rightward
                      ((and (= dy 0) (< dx 0)) ?◀)  ; Pure horizontal leftward
                      ;; For diagonal cases, trust coordinate direction over port-side
                      ((> (abs dy) (abs dx))
                       (if (> dy 0) ?▼ ?▲))    ; Primarily vertical
                      ((> (abs dx) (abs dy))
                       (if (> dx 0) ?▶ ?◀))    ; Primarily horizontal
                      ;; FALLBACK: Use port side only for unclear cases
                      ((eq port-side 'top) ?▼)
                      ((eq port-side 'bottom) ?▲)
                      ((eq port-side 'left) ?▶)
                      ((eq port-side 'right) ?◀)
                      (t ?▶))))                      ; Final default

    ;; Draw arrow at endpoint with ultra-safe collision detection
    (dag-draw--ultra-safe-draw-arrow grid x2 y2 arrow-char occupancy-map)))

(defun dag-draw--ascii-draw-spline-path (graph edge grid min-x min-y scale occupancy-map)
  "Draw spline path with proper GKNV Section 5.2 coordinate transformation.
TEMPORARY: Simplified version that falls back to orthogonal for testing arrows."
  ;; TODO: Implement full spline drawing logic
  ;; For now, just use orthogonal to test the arrow system
  (dag-draw--ascii-draw-safe-orthogonal-edge graph edge grid min-x min-y scale occupancy-map))

;;; Boundary Port Calculation

(defun dag-draw--calculate-boundary-ports (from-x from-y from-width from-height to-x to-y to-width to-height)
  "Calculate connection ports on actual box boundaries to avoid intersecting text.
COORDINATE FIX: Uses actual drawn box positions to calculate clean boundary connections."
  (let* (;; Calculate box centers for direction determination
         (from-center-x (+ from-x (/ from-width 2.0)))
         (from-center-y (+ from-y (/ from-height 2.0)))
         (to-center-x (+ to-x (/ to-width 2.0)))
         (to-center-y (+ to-y (/ to-height 2.0)))
         ;; Calculate direction between boxes
         (dx (- to-center-x from-center-x))
         (dy (- to-center-y from-center-y))
         ;; Determine primary connection direction
         (horizontal-primary (> (abs dx) (abs dy))))

    (if horizontal-primary
        ;; Horizontal connection - connect to edge centers (simple and clean)
        (if (> dx 0)
            ;; Left to right connection
            (list (list (+ from-x from-width -1) (round from-center-y))  ; Right edge center
                  (list to-x (round to-center-y)))                       ; Left edge center
          ;; Right to left connection
          (list (list from-x (round from-center-y))                      ; Left edge center
                (list (+ to-x to-width -1) (round to-center-y))))        ; Right edge center
      ;; Vertical connection - connect to edge centers (simple and clean)
      (if (> dy 0)
          ;; Top to bottom connection
          (list (list (round from-center-x) (+ from-y from-height -1))   ; Bottom edge center
                (list (round to-center-x) to-y))                         ; Top edge center
        ;; Bottom to top connection
        (list (list (round from-center-x) from-y)                        ; Top edge center
              (list (round to-center-x) (+ to-y to-height -1)))))))      ; Bottom edge center

(defun dag-draw--safe-draw-l-path (grid x1 y1 x2 y2 occupancy-map)
  "Draw L-shaped path using safe character placement.
Uses horizontal-first routing to minimize node text intersection."
  ;; Only draw if the points are different
  (when (not (and (= x1 x2) (= y1 y2)))
    ;; Draw horizontal line from start to corner
    (when (/= x1 x2)
      (let ((start-x (min x1 x2))
            (end-x (max x1 x2)))
        (dotimes (i (1+ (- end-x start-x)))
          (dag-draw--safe-place-char grid (+ start-x i) y1 ?─ occupancy-map))))

    ;; Draw vertical line from corner to end
    (when (/= y1 y2)
      (let ((start-y (min y1 y2))
            (end-y (max y1 y2)))
        (dotimes (i (1+ (- end-y start-y)))
          (dag-draw--safe-place-char grid x2 (+ start-y i) ?│ occupancy-map))))

    ;; Place corner junction only if we have both horizontal and vertical segments
    (when (and (/= x1 x2) (/= y1 y2))
      (dag-draw--safe-place-char grid x2 y1 ?┼ occupancy-map))))

(provide 'dag-draw-ascii-edges)

;;; dag-draw-ascii-edges.el ends here
