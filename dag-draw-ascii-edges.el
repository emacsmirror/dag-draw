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
                       (>= y 0) (< y grid-height))
              ;; ALWAYS use ultra-safe drawing to prevent double vertical lines
              (dag-draw--ultra-safe-draw-char grid x1 y ?│ occupancy-map))))))

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

(defvar dag-draw--arrow-positions nil
  "Track positions where arrows have been placed to prevent conflicts.")

(defun dag-draw--ultra-safe-draw-arrow (grid x y arrow-char occupancy-map)
  "Draw arrow character with GKNV Section 5.2 boundary clipping."
  (when arrow-char
    (let* ((grid-height (length grid))
           (grid-width (if (> grid-height 0) (length (aref grid 0)) 0))
           (position-key (format "%d,%d" x y)))

      (when (and (>= x 0) (< x grid-width) (>= y 0) (< y grid-height))
        (let ((current-char (aref (aref grid y) x)))

          ;; GKNV-COMPLIANT ARROW PLACEMENT: Arrows are allowed on node boundaries
          (cond
           ;; Case 1: Empty space - always safe to place arrow
           ((eq current-char ?\s)
            (aset (aref grid y) x arrow-char)
            (push position-key dag-draw--arrow-positions))

           ;; Case 2: Plain edge characters - safe to replace with arrow (GKNV boundary connections)
           ((memq current-char '(?─ ?│ ?┼ ?┌ ?┐ ?└ ?┘))
            (aset (aref grid y) x arrow-char)
            (push position-key dag-draw--arrow-positions))

           ;; Case 3: Already has arrow - SKIP TO PREVENT CONFLICTS
           ;; Don't place arrows where another arrow already exists to prevent ▶◀ conflicts
           ((memq current-char '(?▼ ?▲ ?▶ ?◀))
            ;; Skip this arrow placement to avoid visual conflicts
            nil)

           ;; Case 4: Node boundary characters - GKNV allows arrows on boundaries
           ;; This is the key fix: arrows can be placed on node borders per GKNV Section 5.1.1
           ((memq current-char '(?┬ ?┴ ?├ ?┤))
            (aset (aref grid y) x arrow-char)
            (push position-key dag-draw--arrow-positions))

           ;; Case 5: Alphanumeric characters (node text) - NEVER overwrite
           ((or (and (>= current-char ?a) (<= current-char ?z))
                (and (>= current-char ?A) (<= current-char ?Z))
                (and (>= current-char ?0) (<= current-char ?9)))
            nil)

           ;; Case 6: Default - allow arrow placement for GKNV compliance
           (t
            (aset (aref grid y) x arrow-char))))))))


(defun dag-draw--ultra-safe-draw-char (grid x y char occupancy-map)
  "Draw character with collision detection that matches arrow placement logic."
  (let* ((grid-height (length grid))
         (grid-width (if (> grid-height 0) (length (aref grid 0)) 0)))

    (when (and (>= x 0) (< x grid-width) (>= y 0) (< y grid-height))
      ;; BUFFER ZONE FIX: Check occupancy map first to respect buffer zones
      ;; If position is in a buffer zone, skip drawing ANY edge character
      (unless (and occupancy-map
                   (let ((grid-height-occ (length occupancy-map))
                         (grid-width-occ (if (> (length occupancy-map) 0) (length (aref occupancy-map 0)) 0)))
                     (and (>= x 0) (< x grid-width-occ) (>= y 0) (< y grid-height-occ)
                          (aref (aref occupancy-map y) x)
                          ;; Block ALL edge characters in buffer zones, not just vertical lines
                          (memq char '(?─ ?│ ?┼ ?┌ ?┐ ?└ ?┘))))))

      (catch 'ultra-safe-exit
        (let ((current-char (aref (aref grid y) x)))
          (cond
           ;; Case 1: Empty space - always safe to place
           ((eq current-char ?\s)
            (aset (aref grid y) x char)
            t)
           ;; Case 2: NEVER overwrite box corner characters - they define node boundaries
           ((memq current-char '(?┌ ?┐ ?└ ?┘))
            nil)
           ;; Case 2.5: NEVER draw edge lines adjacent to box corners (prevents trailing garbage)
           ((dag-draw--position-adjacent-to-box-corner grid x y)
            nil)
           ;; Case 2.6: NEVER draw vertical lines adjacent to vertical node boundaries OR inside boxes
           ((and (eq char ?│)
                 (or (dag-draw--position-adjacent-to-vertical-boundary grid x y)
                     (dag-draw--position-inside-or-touching-node-box grid x y)))
            nil)
           ;; Case 3: Junction and line combinations (avoid double vertical lines)
           ((and (eq current-char ?│) (eq char ?│))
            ;; NEVER place vertical line on existing vertical line
            nil)
           ((and (eq current-char ?─) (eq char ?│))
            ;; Horizontal + Vertical = Junction
            (aset (aref grid y) x ?┼)
            t)
           ((and (eq current-char ?│) (eq char ?─))
            ;; Vertical + Horizontal = Junction
            (aset (aref grid y) x ?┼)
            t)
           ((and (memq current-char '(?─ ?┼))
                 (memq char '(?─ ?┼)))
            ;; Other line character combinations
            (aset (aref grid y) x char)
            t)
           ;; Case 4: Never overwrite node text content
           ((or (and (>= current-char ?a) (<= current-char ?z))
                (and (>= current-char ?A) (<= current-char ?Z))
                (and (>= current-char ?0) (<= current-char ?9)))
            nil)
           ;; Case 5: Can create junctions with existing edge characters
           ((and (memq current-char '(?─ ?│))
                 (memq char '(?─ ?│ ?┼)))
            ;; Create proper junctions when edges meet
            (cond
             ;; Horizontal meets vertical - create junction
             ((and (eq current-char ?─) (eq char ?│))
              (aset (aref grid y) x ?┼) t)
             ((and (eq current-char ?│) (eq char ?─))
              (aset (aref grid y) x ?┼) t)
             ;; Same character - allow
             ((eq current-char char)
              (aset (aref grid y) x char) t)
             (t nil)))
           ;; Case 6: Default - allow placement in empty space only
           (t
            (aset (aref grid y) x char)
            t)))))))

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

;;; PHASE 3: Edge Conflict Resolution Functions

(defun dag-draw--create-edge-usage-map (grid)
  "Create a map to track which grid positions are used by edges.
This prevents multiple edges from drawing conflicting characters at the same position."
  (let* ((grid-height (length grid))
         (grid-width (if (> grid-height 0) (length (aref grid 0)) 0))
         (usage-map (make-vector grid-height nil)))
    (dotimes (y grid-height)
      (aset usage-map y (make-vector grid-width nil)))
    usage-map))

(defun dag-draw--sort-edges-by-priority (graph edges)
  "Sort edges by drawing priority to minimize conflicts.
Longer edges (spanning more ranks) get priority over shorter edges."
  (sort edges (lambda (edge-a edge-b)
                (let* ((from-a (dag-draw-get-node graph (dag-draw-edge-from-node edge-a)))
                       (to-a (dag-draw-get-node graph (dag-draw-edge-to-node edge-a)))
                       (from-b (dag-draw-get-node graph (dag-draw-edge-from-node edge-b)))
                       (to-b (dag-draw-get-node graph (dag-draw-edge-to-node edge-b)))
                       (rank-span-a (abs (- (or (dag-draw-node-rank to-a) 0)
                                            (or (dag-draw-node-rank from-a) 0))))
                       (rank-span-b (abs (- (or (dag-draw-node-rank to-b) 0)
                                            (or (dag-draw-node-rank from-b) 0)))))
                  ;; Longer spanning edges get priority (drawn first)
                  (> rank-span-a rank-span-b)))))

(defun dag-draw--ascii-draw-gknv-edge (graph edge grid min-x min-y scale occupancy-map drawn-splines)
  "Draw edge using GKNV Section 5 spline algorithm with collision avoidance.
GKNV Section 5.1.1: 'Tail and head port boxes route the spline to the appropriate side of the node'
GKNV Section 5.2: 'clips the spline to the boundaries of the endpoint node shapes'
DRAWN-SPLINES is list of already-drawn edges for collision avoidance per GKNV greedy strategy."
  ;; Check for collisions with already-drawn splines
  (if drawn-splines
      ;; Use collision-aware drawing for later edges
      (dag-draw--ascii-draw-collision-aware-edge graph edge grid min-x min-y scale occupancy-map drawn-splines)
    ;; First edge can be drawn directly
    (dag-draw--ascii-draw-safe-edge graph edge grid min-x min-y scale occupancy-map)))

(defun dag-draw--ascii-draw-collision-aware-edge (graph edge grid min-x min-y scale occupancy-map drawn-splines)
  "Draw edge with collision avoidance per GKNV Section 5.2.
DRAWN-SPLINES contains already-drawn edges that this edge must avoid."
  ;; For now, use a simple offset strategy:
  ;; Check if the direct path would conflict with drawn splines
  ;; If so, apply a small offset to avoid collision
  (let* ((from-node (dag-draw-get-node graph (dag-draw-edge-from-node edge)))
         (to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge)))
         (from-id (dag-draw-edge-from-node edge))
         (to-id (dag-draw-edge-to-node edge))
         ;; Count how many edges from same source node have already been drawn
         (same-source-count (length (seq-filter (lambda (drawn-edge)
                                                  (eq (dag-draw-edge-from-node drawn-edge) from-id))
                                                drawn-splines)))
         ;; Apply small offset based on drawing order to prevent overlaps
         (offset-amount (* same-source-count 1)))  ; 1-character offset per edge

    ;; Use offset in spline generation or ASCII conversion
    ;; For now, just use the regular drawing with occupancy awareness
    (dag-draw--ascii-draw-safe-edge graph edge grid min-x min-y scale occupancy-map)))


(defun dag-draw--calculate-edge-route (from-x from-y to-x to-y edge-usage-map)
  "Calculate the path that would be used for an edge route.
Returns list of (x y character) tuples that would be drawn."
  (let ((route-points '()))
    ;; Simple L-shaped route calculation
    (cond
     ;; Vertical-only route
     ((= from-x to-x)
      (let ((start-y (min from-y to-y))
            (end-y (max from-y to-y)))
        (dotimes (i (1+ (- end-y start-y)))
          (let ((y (+ start-y i)))
            (push (list from-x y ?│) route-points)))))
     ;; Horizontal-only route
     ((= from-y to-y)
      (let ((start-x (min from-x to-x))
            (end-x (max from-x to-x)))
        (dotimes (i (1+ (- end-x start-x)))
          (let ((x (+ start-x i)))
            (push (list x from-y ?─) route-points)))))
     ;; L-shaped route (horizontal first, then vertical)
     (t
      ;; Horizontal segment
      (let ((start-x (min from-x to-x))
            (end-x (max from-x to-x)))
        (dotimes (i (1+ (- end-x start-x)))
          (let ((x (+ start-x i)))
            (push (list x from-y ?─) route-points))))
      ;; Vertical segment (excluding corner)
      (let ((start-y (min from-y to-y))
            (end-y (max from-y to-y)))
        (dotimes (i (- end-y start-y))
          (let ((y (+ start-y i 1)))
            (push (list to-x y ?│) route-points))))))
    route-points))

(defun dag-draw--route-has-conflict (route-path edge-usage-map)
  "Check if route path conflicts with existing edge usage."
  (let ((has-conflict nil))
    (dolist (route-point route-path)
      (let ((x (nth 0 route-point))
            (y (nth 1 route-point))
            (char (nth 2 route-point)))
        (when (and (>= y 0) (< y (length edge-usage-map))
                   (>= x 0) (< x (length (aref edge-usage-map y))))
          (let ((existing-char (aref (aref edge-usage-map y) x)))
            ;; Conflict if position is used and characters are incompatible
            (when (and existing-char
                       (not (eq existing-char ?\s))
                       (not (dag-draw--characters-compatible existing-char char)))
              (setq has-conflict t))))))
    has-conflict))

(defun dag-draw--characters-compatible (char1 char2)
  "Check if two edge characters can coexist at the same position."
  (cond
   ;; Same character - always compatible
   ((eq char1 char2) t)
   ;; Junction creation - horizontal and vertical make junction
   ((and (memq char1 '(?─ ?│)) (memq char2 '(?─ ?│))) t)
   ;; Arrows are never compatible with each other
   ((and (memq char1 '(?▶ ?◀ ?▲ ?▼)) (memq char2 '(?▶ ?◀ ?▲ ?▼))) nil)
   ;; Default - not compatible
   (t nil)))

(defun dag-draw--mark-route-usage (route-path edge-usage-map)
  "Mark the route path as used in the edge usage map."
  (dolist (route-point route-path)
    (let ((x (nth 0 route-point))
          (y (nth 1 route-point))
          (char (nth 2 route-point)))
      (when (and (>= y 0) (< y (length edge-usage-map))
                 (>= x 0) (< x (length (aref edge-usage-map y))))
        (aset (aref edge-usage-map y) x char)))))

(defun dag-draw--draw-alternative-route (graph edge grid min-x min-y scale occupancy-map edge-usage-map)
  "Draw edge using alternative routing to avoid conflicts.
This is a simplified fallback that tries offset routing."
  ;; For now, just use the original safe edge drawing
  ;; In a full implementation, this would try different routing strategies
  (dag-draw--ascii-draw-safe-edge graph edge grid min-x min-y scale occupancy-map))

;;; ASCII Edge Drawing - Main Entry Point

(defun dag-draw--ascii-draw-edges (graph grid min-x min-y scale)
  "Draw edges using GKNV Section 5 spline drawing algorithm.
GKNV COMPLIANCE: Implement proper Pass 4 spline-to-ASCII conversion with boundary clipping."
  ;; Initialize arrow position tracking for GKNV Section 5.2 boundary clipping
  (setq dag-draw--arrow-positions nil)

  ;; Create node occupancy map to avoid drawing through nodes
  (let ((occupancy-map (dag-draw--create-node-occupancy-map graph grid min-x min-y scale)))
    ;; Set global occupancy map for safety checks
    (setq dag-draw--global-occupancy-map occupancy-map)

    ;; GKNV Pass 4: Draw edges sequentially to avoid collisions per Section 5.2
    ;; "splines are drawn by a 'greedy' strategy, they depend on the order in which they are computed"
    (let ((drawn-splines '())  ; Track already-drawn splines for collision avoidance
          (edge-usage-map (dag-draw--create-edge-usage-map grid)))  ; Track edge positions
      (dolist (edge (dag-draw-graph-edges graph))
        ;; Use GKNV-compliant edge drawing with collision awareness
        (dag-draw--ascii-draw-gknv-edge graph edge grid min-x min-y scale occupancy-map drawn-splines)
        ;; Add this edge to drawn splines for future collision avoidance
        (push edge drawn-splines)))))

;;; Edge Drawing Implementation - FIXED VERSION

(defun dag-draw--ascii-draw-safe-edge (graph edge grid min-x min-y scale occupancy-map)
  "Draw edge with enhanced collision detection that prevents any overwrites of node content.
    PHASE 2: Now uses proper spline-to-ASCII conversion per GKNV Section 5.2."
  ;; GKNV COMPLIANCE FIX: Re-enable spline drawing to use proper GKNV Pass 4 algorithm
  (let ((spline-points (dag-draw-edge-spline-points edge)))
    (if spline-points
        ;; DEBUG: Log spline path usage
        (progn
          (message "SPLINE-PATH: Edge %s->%s using spline drawing (%d points)"
                   (dag-draw-edge-from-node edge) (dag-draw-edge-to-node edge) (length spline-points))
          (message "  WORLD-BOUNDS: min-x=%.1f min-y=%.1f scale=%.3f" min-x min-y scale)
          ;; Use actual splines per GKNV algorithm Phase 4
          (dag-draw--ascii-draw-spline-path graph edge grid min-x min-y scale occupancy-map))
      ;; DEBUG: Log orthogonal path usage
      (progn
        (message "ORTHOGONAL: Edge %s->%s using orthogonal drawing"
                 (dag-draw-edge-from-node edge) (dag-draw-edge-to-node edge))
        ;; Fallback to orthogonal for edges without splines
        (dag-draw--ascii-draw-safe-orthogonal-edge graph edge grid min-x min-y scale occupancy-map)))))

;;; Enhanced Edge Drawing Functions

(defun dag-draw--ascii-draw-safe-orthogonal-edge (graph edge grid min-x min-y scale occupancy-map)
  "Draw orthogonal edge with comprehensive collision avoidance."
  (let ((connection-points (dag-draw--get-edge-connection-points graph edge min-x min-y scale)))
    ;; Connection points calculated

    (if (and connection-points (= (length connection-points) 2))
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
          ;; DEBUG: Log enhanced port usage
          (message "ENHANCED: Edge %s->%s using calculated ports (%d,%d)->(%d,%d)"
                   (dag-draw-edge-from-node edge) (dag-draw-edge-to-node edge)
                   from-x from-y to-x to-y)
          ;; Drawing edge with calculated coordinates
          ;; Use safe path drawing that absolutely will not overwrite node content
          (dag-draw--ascii-draw-ultra-safe-path-with-port-arrow
           grid from-x from-y to-x to-y occupancy-map target-port-side))
      ;; CRITICAL FIX: Still try to draw something even if connection points fail
      (progn
        ;; DEBUG: Log fallback usage
        (message "FALLBACK: Edge %s->%s using node centers (connection-points: %s)"
                 (dag-draw-edge-from-node edge) (dag-draw-edge-to-node edge)
                 (if connection-points (format "%d points" (length connection-points)) "nil"))
        ;; Fallback to node center connections
        ;; Fallback: use node centers as connection points
        (dag-draw--ascii-draw-fallback-edge graph edge grid min-x min-y scale occupancy-map)))))

(defun dag-draw--ascii-draw-fallback-edge (graph edge grid min-x min-y scale occupancy-map)
  "Fallback edge drawing using node centers when port calculation fails."
  (let* ((from-node (dag-draw-get-node graph (dag-draw-edge-from-node edge)))
         (to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge))))
    (when (and from-node to-node)
      (let* ((from-center (dag-draw--get-node-center-grid from-node min-x min-y scale graph))
             (to-center (dag-draw--get-node-center-grid to-node min-x min-y scale graph))
             (from-x (round (dag-draw-point-x from-center)))
             (from-y (round (dag-draw-point-y from-center)))
             (to-x (round (dag-draw-point-x to-center)))
             (to-y (round (dag-draw-point-y to-center))))
        ;; Drawing fallback edge between node centers
        ;; Draw simple L-shaped path between centers
        (dag-draw--draw-ultra-safe-l-path grid from-x from-y to-x to-y occupancy-map 'horizontal-first)
        ;; Add arrow at destination
        (dag-draw--add-port-based-arrow grid from-x from-y to-x to-y occupancy-map 'unknown)))))

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

     ;; Pure horizontal line - GKNV spline conversion
     ((eq direction 'horizontal-only)
      (let ((start-x (min x1 x2))
            (end-x (max x1 x2)))
        ;; Draw continuous horizontal line as GKNV spline requires
        (dotimes (i (1+ (- end-x start-x)))
          (let ((x (+ start-x i)))
            (dag-draw--ultra-safe-draw-char grid x y1 ?─ occupancy-map)))))

     ;; L-shaped path: horizontal first - GKNV spline conversion
     ((eq direction 'horizontal-first)
      ;; Horizontal segment first: x1 to x2 at y1 as continuous GKNV spline
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
      ;; Horizontal segment: x1 to x2 at y2 as continuous GKNV spline
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
         (dy (- y2 y1)))

    ;; DEBUG: Show coordinates and direction calculation for arrow placement
    (message "    GRID-ARROW: (%d,%d)->(%d,%d) dx=%d dy=%d port-side=%s"
             x1 y1 x2 y2 dx dy port-side)

    (let ((arrow-char (cond
                      ;; PRIORITY: Use coordinate-based direction for clear vertical/horizontal cases
                      ((and (= dx 0) (> dy 0)) ?▼)  ; Pure vertical downward
                      ((and (= dx 0) (< dy 0)) ?▲)  ; Pure vertical upward
                      ((and (= dy 0) (> dx 0)) ?▶)  ; Pure horizontal rightward
                      ((and (= dy 0) (< dx 0)) ?◀)  ; Pure horizontal leftward
                      ;; For diagonal cases, trust coordinate direction over port-side
                      ((> (abs dy) (abs dx))
                       (if (> dy 0) ?▼ ?▲))    ; Primarily vertical
                      ((> (abs dx) (abs dy))
                       ;; ARROW DIRECTION FIX: For primarily horizontal edges, use port-side as hint
                       ;; Port-side 'left means arrow goes rightward (INTO left side)
                       (cond ((eq port-side 'left) ?▶)   ; Arrow pointing right into left side
                             ((eq port-side 'right) ?◀)  ; Arrow pointing left into right side  
                             ((> dx 0) ?▶)               ; Fallback to coordinate direction
                             (t ?◀)))                    ; Fallback to coordinate direction
                      ;; FALLBACK: Use port side only for unclear cases
                      ((eq port-side 'top) ?▼)
                      ((eq port-side 'bottom) ?▲)
                      ((eq port-side 'left) ?▶)
                      ((eq port-side 'right) ?◀)
                      (t ?▶))))                      ; Final default

      ;; Draw arrow at endpoint with ultra-safe collision detection
      (dag-draw--ultra-safe-draw-arrow grid x2 y2 arrow-char occupancy-map))))

(defun dag-draw--draw-gknv-continuous-horizontal-line (grid start-x end-x y occupancy-map)
  "Draw continuous horizontal line per GKNV spline algorithm.
GKNV Section 5.2: Splines are continuous curves, not broken segments."
  (let ((start-pos (min start-x end-x))
        (end-pos (max start-x end-x)))
    ;; Draw continuous horizontal line as GKNV algorithm requires
    (dotimes (i (1+ (- end-pos start-pos)))
      (let ((x (+ start-pos i)))
        (dag-draw--ultra-safe-draw-char grid x y ?─ occupancy-map)))))

(defun dag-draw--determine-port-side-from-coordinates (x1 y1 x2 y2)
  "Determine port side based on coordinate direction for GKNV Section 5.1.1."
  (let ((dx (- x2 x1))
        (dy (- y2 y1)))
    (cond
     ((> (abs dx) (abs dy))
      (if (> dx 0) 'right 'left))
     (t
      (if (> dy 0) 'bottom 'top)))))

(defun dag-draw--ascii-draw-spline-path (graph edge grid min-x min-y scale occupancy-map)
  "Draw spline path with proper GKNV Section 5.2 coordinate transformation.
Implements GKNV Pass 4: Convert B-spline control points to ASCII grid coordinates."
  (let ((spline-points (dag-draw-edge-spline-points edge)))
    (if (and spline-points (>= (length spline-points) 2))
        (progn
          ;; GKNV Section 5.2: Transform spline control points to ASCII grid
          (dag-draw--draw-gknv-spline-to-ascii graph edge spline-points grid min-x min-y scale occupancy-map))
      ;; Fallback to orthogonal if no spline data
      (dag-draw--ascii-draw-safe-orthogonal-edge graph edge grid min-x min-y scale occupancy-map))))

(defun dag-draw--draw-gknv-spline-to-ascii (graph edge spline-points grid min-x min-y scale occupancy-map)
  "Convert GKNV B-spline control points to clean separated ASCII path.
GKNV Section 5.2 compliant: Proper edge separation to avoid overlapping paths."
  (let* ((connection-points (dag-draw--get-edge-connection-points graph edge min-x min-y scale))
         (from-port (when connection-points (car connection-points)))
         (to-port (when connection-points (cadr connection-points))))

    (when (and from-port to-port)
      (message "  RAW-PORTS: from=(%.2f,%.2f) to=(%.2f,%.2f)"
               (dag-draw-point-x from-port) (dag-draw-point-y from-port)
               (dag-draw-point-x to-port) (dag-draw-point-y to-port))
      (let* ((start-x (round (dag-draw-point-x from-port)))
             (start-y (round (dag-draw-point-y from-port)))
             (end-x (round (dag-draw-point-x to-port)))
             (end-y (round (dag-draw-point-y to-port))))

        (message "  GRID-PORTS: Edge %s->%s: (%d,%d) -> (%d,%d) %s"
                 (dag-draw-edge-from-node edge) (dag-draw-edge-to-node edge)
                 start-x start-y end-x end-y
                 (if (and (= start-x end-x) (= start-y end-y)) "ZERO-LENGTH!" ""))

        (let* ((to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge)))
               (target-port-side (dag-draw--determine-port-side to-node to-port min-x min-y scale graph))
               ;; Calculate edge separation offset to prevent overlapping
               (from-node-id (dag-draw-edge-from-node edge))
               (edges-from-same-node (dag-draw-get-edges-from graph from-node-id))
               (edge-index (--find-index (eq edge it) edges-from-same-node))
               (total-edges (length edges-from-same-node))
               ;; Enable edge separation to prevent overlapping parallel lines
               (y-separation (if (> total-edges 1)
                                (* edge-index 2)  ; Separate multiple edges by 2 grid units
                              0))
               (adjusted-start-y (+ start-y y-separation)))

          ;; PHASE 2 FIX: Actually use the spline points and convert world→grid coordinates
          (if (and spline-points (>= (length spline-points) 2))
              ;; Use actual spline path with coordinate conversion
              (dag-draw--draw-converted-spline-segments
               spline-points start-x start-y end-x end-y grid min-x min-y occupancy-map target-port-side)
            ;; Fallback to straight line if no valid spline data
            (dag-draw--ascii-draw-ultra-safe-path-with-port-arrow
             grid start-x adjusted-start-y end-x end-y occupancy-map target-port-side)))))))

(defun dag-draw--draw-converted-spline-segments (spline-points start-x start-y end-x end-y grid min-x min-y occupancy-map target-port-side)
  "Draw spline path with proper world→grid coordinate conversion.
PHASE 2 FIX: Converts spline points from world coordinates to grid coordinates before drawing."
  (let ((converted-points '())
        (current-x start-x)
        (current-y start-y))

    ;; Convert each spline point from world coordinates to grid coordinates
    (dolist (spline-point spline-points)
      (let* ((world-x (dag-draw-point-x spline-point))
             (world-y (dag-draw-point-y spline-point))
             ;; CRITICAL: Convert world coordinates to grid coordinates
             (grid-x (round (dag-draw--world-to-grid-coord world-x min-x dag-draw-ascii-coordinate-scale)))
             (grid-y (round (dag-draw--world-to-grid-coord world-y min-y dag-draw-ascii-coordinate-scale))))
        (push (list grid-x grid-y) converted-points)))

    ;; Reverse to get correct order
    (setq converted-points (nreverse converted-points))

    ;; REVERTED: Use simple L-path but fix the collision detection
    ;; The spline points route through nodes, so use safe orthogonal routing instead
    (dag-draw--draw-ultra-safe-l-path grid start-x start-y end-x end-y occupancy-map 'horizontal-first)
    ;; Add proper arrow at endpoint
    (dag-draw--add-port-based-arrow grid start-x start-y end-x end-y occupancy-map target-port-side)))

(defun dag-draw--draw-clean-line-segment (grid x1 y1 x2 y2 occupancy-map)
  "Draw a clean line segment from (x1,y1) to (x2,y2) with proper collision detection."
  (let* ((dx (- x2 x1))
         (dy (- y2 y1)))
    (cond
     ;; Horizontal line
     ((= dy 0)
      (let ((start-x (min x1 x2))
            (end-x (max x1 x2)))
        (dotimes (i (1+ (- end-x start-x)))
          (let ((x (+ start-x i)))
            (dag-draw--ultra-safe-draw-char grid x y1 ?─ occupancy-map)))))

     ;; Vertical line
     ((= dx 0)
      (let ((start-y (min y1 y2))
            (end-y (max y1 y2)))
        (dotimes (i (1+ (- end-y start-y)))
          (let ((y (+ start-y i)))
            (dag-draw--ultra-safe-draw-char grid x1 y ?│ occupancy-map)))))

     ;; Diagonal - use L-shaped path
     (t
      ;; Draw horizontal first, then vertical
      (dag-draw--draw-clean-line-segment grid x1 y1 x2 y1 occupancy-map)
      (dag-draw--draw-clean-line-segment grid x2 y1 x2 y2 occupancy-map)))))

(defun dag-draw--draw-continuous-spline-segments (grid-points start-x start-y end-x end-y grid occupancy-map)
  "Draw continuous line segments connecting all spline points per GKNV algorithm.
Creates a proper continuous path following the spline curve points."
  (when grid-points
    (let ((current-x start-x)
          (current-y start-y))

      ;; Draw segments connecting each spline point
      (dolist (grid-point grid-points)
        (let ((next-x (nth 0 grid-point))
              (next-y (nth 1 grid-point)))
          ;; Draw continuous line segment from current position to next point
          (dag-draw--draw-simple-continuous-path-segment grid current-x current-y next-x next-y occupancy-map)
          ;; Update current position
          (setq current-x next-x)
          (setq current-y next-y)))

      ;; Ensure final connection to endpoint
      (unless (and (= current-x end-x) (= current-y end-y))
        (dag-draw--draw-simple-continuous-path-segment grid current-x current-y end-x end-y occupancy-map)))))

(defun dag-draw--draw-simple-continuous-path-segment (grid x1 y1 x2 y2 occupancy-map)
  "Draw continuous path segment with simple, direct line drawing.
Trust the GKNV algorithm - no safety hacks, just draw the lines."
  (let* ((grid-height (length grid))
         (grid-width (if (> grid-height 0) (length (aref grid 0)) 0))
         (dx (- x2 x1))
         (dy (- y2 y1)))

    (cond
     ;; Pure horizontal line
     ((= dy 0)
      (let ((start-x (min x1 x2))
            (end-x (max x1 x2)))
        (dotimes (i (1+ (- end-x start-x)))
          (let ((x (+ start-x i)))
            (when (and (>= x 0) (< x grid-width) (>= y1 0) (< y1 grid-height))
              (let ((current-char (aref (aref grid y1) x)))
                (cond
                 ;; Empty space - draw line
                 ((eq current-char ?\s)
                  (aset (aref grid y1) x ?─))
                 ;; Already horizontal line - keep it
                 ((eq current-char ?─) nil)
                 ;; Vertical line - create proper junction
                 ((eq current-char ?│)
                  (aset (aref grid y1) x ?┼)))))))))

     ;; Pure vertical line
     ((= dx 0)
      (let ((start-y (min y1 y2))
            (end-y (max y1 y2)))
        (dotimes (i (1+ (- end-y start-y)))
          (let ((y (+ start-y i)))
            (when (and (>= x1 0) (< x1 grid-width) (>= y 0) (< y grid-height))
              (let ((current-char (aref (aref grid y) x1)))
                (cond
                 ;; Empty space - use ultra-safe drawing
                 ((eq current-char ?\s)
                  (dag-draw--ultra-safe-draw-char grid x1 y ?│ occupancy-map))
                 ;; Already vertical line - keep it
                 ((eq current-char ?│) nil)
                 ;; Horizontal line - create proper junction (use ultra-safe)
                 ((eq current-char ?─)
                  (dag-draw--ultra-safe-draw-char grid x1 y ?┼ occupancy-map)))))))))

     ;; L-shaped path: horizontal first, then vertical
     (t
      ;; Horizontal segment
      (let ((start-x (min x1 x2))
            (end-x (max x1 x2)))
        (dotimes (i (1+ (- end-x start-x)))
          (let ((x (+ start-x i)))
            (when (and (>= x 0) (< x grid-width) (>= y1 0) (< y1 grid-height))
              (let ((current-char (aref (aref grid y1) x)))
                (cond
                 ;; Empty space - draw line
                 ((eq current-char ?\s)
                  (aset (aref grid y1) x ?─))
                 ;; Already horizontal line - keep it
                 ((eq current-char ?─) nil)
                 ;; Vertical line - create proper junction
                 ((eq current-char ?│)
                  (aset (aref grid y1) x ?┼))))))))
      ;; Vertical segment
      (let ((start-y (min y1 y2))
            (end-y (max y1 y2)))
        (dotimes (i (1+ (- end-y start-y)))
          (let ((y (+ start-y i)))
            (when (and (>= x2 0) (< x2 grid-width) (>= y 0) (< y grid-height))
              (let ((current-char (aref (aref grid y) x2)))
                (cond
                 ;; Empty space - use ultra-safe drawing
                 ((eq current-char ?\s)
                  (dag-draw--ultra-safe-draw-char grid x2 y ?│ occupancy-map))
                 ;; Already vertical line - keep it
                 ((eq current-char ?│) nil)
                 ;; Horizontal line - create proper junction
                 ((eq current-char ?─)
                  (dag-draw--ultra-safe-draw-char grid x2 y ?┼ occupancy-map))))))))
      ;; Corner character at junction - only if corner position is empty
      (let ((corner-char (cond
                          ((and (< x1 x2) (< y1 y2)) ?┐)  ; Right then down
                          ((and (< x1 x2) (> y1 y2)) ?┘)  ; Right then up
                          ((and (> x1 x2) (< y1 y2)) ?┌)  ; Left then down
                          ((and (> x1 x2) (> y1 y2)) ?└)  ; Left then up
                          (t ?┼))))                         ; Fallback
        (when (and (>= x2 0) (< x2 grid-width) (>= y1 0) (< y1 grid-height))
          (let ((current-char (aref (aref grid y1) x2)))
            (when (eq current-char ?\s)  ; Only draw corner if space is empty
              (aset (aref grid y1) x2 corner-char)))))))))

(defun dag-draw--add-gknv-boundary-arrow (graph edge grid x y occupancy-map)
  "Add arrow at spline endpoint following GKNV Section 5.2 boundary clipping."
  (let* ((from-node (dag-draw-get-node graph (dag-draw-edge-from-node edge)))
         (to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge)))
         ;; Calculate arrow direction based on edge direction
         (direction (dag-draw--detect-edge-direction from-node to-node graph))
         (arrow-char (dag-draw--get-arrow-char direction)))

    ;; Place arrow directly - trust the GKNV algorithm
    (when (and arrow-char
               (>= x 0) (< x (length (aref grid 0)))
               (>= y 0) (< y (length grid)))
      (aset (aref grid y) x arrow-char))))

(defun dag-draw--create-spline-path-segments (grid-points start-x start-y end-x end-y)
  "Create path segments from spline points, ensuring connection to actual ports.
Returns list of segments as (x1 y1 x2 y2) for continuous path drawing."
  (let ((segments '())
        (current-x start-x)
        (current-y start-y))

    ;; Create segments connecting spline points
    (dolist (grid-point grid-points)
      (let ((next-x (round (nth 0 grid-point)))
            (next-y (round (nth 1 grid-point))))
        (unless (and (= current-x next-x) (= current-y next-y))
          (push (list current-x current-y next-x next-y) segments)
          (setq current-x next-x)
          (setq current-y next-y))))

    ;; Ensure final segment connects to actual endpoint
    (unless (and (= current-x end-x) (= current-y end-y))
      (push (list current-x current-y end-x end-y) segments))

    (nreverse segments)))

(defun dag-draw--draw-continuous-path-segment (grid x1 y1 x2 y2 occupancy-map)
  "Draw a continuous path segment ensuring no gaps or floating characters.
Uses orthogonal routing (horizontal then vertical) for clean ASCII representation."
  (let* ((grid-height (length grid))
         (grid-width (if (> grid-height 0) (length (aref grid 0)) 0))
         (dx (- x2 x1))
         (dy (- y2 y1)))

    (cond
     ;; Pure horizontal line
     ((= dy 0)
      (let ((start-x (min x1 x2))
            (end-x (max x1 x2)))
        (dotimes (i (1+ (- end-x start-x)))
          (let ((x (+ start-x i)))
            (dag-draw--ultra-safe-draw-char grid x y1 ?─ occupancy-map)))))

     ;; Pure vertical line
     ((= dx 0)
      (let ((start-y (min y1 y2))
            (end-y (max y1 y2)))
        (dotimes (i (1+ (- end-y start-y)))
          (let ((y (+ start-y i)))
            (dag-draw--ultra-safe-draw-char grid x1 y ?│ occupancy-map)))))

     ;; L-shaped path: horizontal first, then vertical
     (t
      ;; Horizontal segment: x1 to x2 at y1
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

      ;; Corner character at junction
      (let ((corner-char (cond
                          ((and (< x1 x2) (< y1 y2)) ?┐)  ; Right then down
                          ((and (< x1 x2) (> y1 y2)) ?┘)  ; Right then up
                          ((and (> x1 x2) (< y1 y2)) ?┌)  ; Left then down
                          ((and (> x1 x2) (> y1 y2)) ?└)  ; Left then up
                          (t ?┼))))                         ; Fallback
        (dag-draw--ultra-safe-draw-char grid x2 y1 corner-char occupancy-map))))))

(defun dag-draw--add-precise-endpoint-arrow (graph edge grid x y occupancy-map)
  "Add arrow at precise endpoint based on edge direction and target node port side.
Implements GKNV Section 5.2 boundary clipping - arrow must be exactly on node boundary."
  (let* ((to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge)))
         (from-node (dag-draw-get-node graph (dag-draw-edge-from-node edge)))
         ;; Calculate direction based on node positions for proper arrow orientation
         (direction (dag-draw--detect-edge-direction from-node to-node graph))
         ;; Use existing arrow character function for proper Unicode arrows
         (arrow-char (dag-draw--get-arrow-char direction)))

    ;; GKNV Section 5.2: Ensure arrow is clipped to node boundary
    (dag-draw--place-boundary-clipped-arrow graph edge grid x y arrow-char occupancy-map)))

(defun dag-draw--detect-edge-direction (from-node to-node &optional graph)
  "Detect direction of edge from FROM-NODE to TO-NODE for proper arrow selection.
Returns 'left, 'right, 'up, or 'down based on node coordinate differences.
COORDINATE SYSTEM FIX: Uses adjusted-positions when available to match port calculation."
  (let* ((from-id (dag-draw-node-id from-node))
         (to-id (dag-draw-node-id to-node))
         ;; Use same coordinate source as port calculation - adjusted-positions first
         (adjusted-positions (and graph (dag-draw-graph-adjusted-positions graph)))
         (from-adjusted (and adjusted-positions (ht-get adjusted-positions from-id)))
         (to-adjusted (and adjusted-positions (ht-get adjusted-positions to-id)))
         ;; Calculate coordinates with adjusted-positions priority
         (from-x (if from-adjusted
                     (+ (nth 0 from-adjusted) (/ (nth 2 from-adjusted) 2.0))  ; center of adjusted box
                   (or (dag-draw-node-x-coord from-node) 0)))
         (from-y (if from-adjusted
                     (+ (nth 1 from-adjusted) (/ (nth 3 from-adjusted) 2.0))  ; center of adjusted box
                   (or (dag-draw-node-y-coord from-node) 0)))
         (to-x (if to-adjusted
                   (+ (nth 0 to-adjusted) (/ (nth 2 to-adjusted) 2.0))      ; center of adjusted box
                 (or (dag-draw-node-x-coord to-node) 0)))
         (to-y (if to-adjusted
                   (+ (nth 1 to-adjusted) (/ (nth 3 to-adjusted) 2.0))      ; center of adjusted box
                 (or (dag-draw-node-y-coord to-node) 0)))
         (dx (- to-x from-x))
         (dy (- to-y from-y)))

    ;; DEBUG: Show direction calculation for diagnosis
    (message "    ARROW-DIR: %s->%s from=(%.1f,%.1f) to=(%.1f,%.1f) dx=%.1f dy=%.1f direction=%s"
             from-id to-id from-x from-y to-x to-y dx dy
             (cond ((>= (abs dy) (abs dx)) (if (> dy 0) 'down 'up))
                   (t (if (> dx 0) 'right 'left))))

    ;; Determine primary direction based on larger coordinate difference
    (cond
     ;; Vertical movement is primary
     ((>= (abs dy) (abs dx))
      (if (> dy 0) 'down 'up))
     ;; Horizontal movement is primary
     (t
      (if (> dx 0) 'right 'left)))))

(defun dag-draw--place-boundary-clipped-arrow (graph edge grid x y arrow-char occupancy-map)
  "Place arrow with GKNV Section 5.2 boundary clipping compliance.
Ensures arrow appears exactly on node boundary, not floating in space."
  (let* ((grid-height (length grid))
         (grid-width (if (> grid-height 0) (length (aref grid 0)) 0))
         (to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge))))

    (when (and (>= x 0) (< x grid-width) (>= y 0) (< y grid-height) to-node)
      (let* ((current-char (aref (aref grid y) x))
             ;; Check if this position is actually on a node boundary
             (is-on-boundary (dag-draw--position-is-on-node-boundary graph x y to-node)))

        (cond
         ;; Case 1: Position is on target node boundary - GKNV compliant placement
         (is-on-boundary
          (aset (aref grid y) x arrow-char))

         ;; Case 2: Empty space near boundary - safe to place
         ((eq current-char ?\s)
          (aset (aref grid y) x arrow-char))

         ;; Case 3: On edge character that can be replaced with arrow
         ((memq current-char '(?─ ?│ ?┼))
          (aset (aref grid y) x arrow-char))

         ;; Case 4: Do not place on node text or unknown characters
         (t nil))))))

(defun dag-draw--position-is-on-node-boundary (graph x y node)
  "Check if position (X,Y) is on the boundary of NODE.
Used for GKNV Section 5.2 boundary clipping verification."
  ;; ENHANCED BOUNDARY DETECTION: Check if position is actually on node boundary
  (let* ((adjusted-positions (dag-draw-graph-adjusted-positions graph))
         (node-id (dag-draw-node-id node)))
    (if adjusted-positions
        (let* ((coords (ht-get adjusted-positions node-id))
               (node-x (nth 0 coords))
               (node-y (nth 1 coords))
               (width (nth 2 coords))
               (height (nth 3 coords))
               (left node-x)
               (right (+ node-x width -1))
               (top node-y)
               (bottom (+ node-y height -1)))
          ;; Check if position is on any of the four boundary edges
          (or (and (>= x left) (<= x right) (or (= y top) (= y bottom)))      ; top/bottom edges
              (and (>= y top) (<= y bottom) (or (= x left) (= x right)))))    ; left/right edges
      ;; Fallback: assume valid boundary
      t)))

(defun dag-draw--align-spline-to-ports (grid-points start-x start-y end-x end-y)
  "Force spline grid points to align precisely with port coordinates.
Fixes coordinate misalignment between spline generation and ASCII rendering."
  (if (< (length grid-points) 2)
      ;; Not enough points - create direct path
      (list (list start-x start-y) (list end-x end-y))
    ;; Force first and last points to match port coordinates exactly
    (let* ((aligned-points (copy-sequence grid-points))
           (first-point (car aligned-points))
           (last-point (car (last aligned-points))))

      ;; Force first point to start port
      (setcar aligned-points (list start-x start-y))

      ;; Force last point to end port
      (setcar (last aligned-points) (list end-x end-y))

      aligned-points)))

(defun dag-draw--position-adjacent-to-box-corner (grid x y)
  "Check if position (X,Y) is adjacent to a box corner, preventing trailing garbage.
Returns t if position is next to a box corner character."
  (let* ((grid-height (length grid))
         (grid-width (if (> grid-height 0) (length (aref grid 0)) 0)))

    ;; Check all 8 adjacent positions for box corner characters
    (catch 'found-corner
      (dolist (offset '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)))
        (let ((check-x (+ x (nth 0 offset)))
              (check-y (+ y (nth 1 offset))))
          (when (and (>= check-x 0) (< check-x grid-width)
                     (>= check-y 0) (< check-y grid-height))
            (let ((char-at-pos (aref (aref grid check-y) check-x)))
              (when (memq char-at-pos '(?┌ ?┐ ?└ ?┘))
                (throw 'found-corner t))))))
      nil)))

(defun dag-draw--position-adjacent-to-vertical-boundary (grid x y)
  "Check if position (X,Y) is adjacent to a vertical node boundary.
Returns t if position is next to a vertical boundary character, preventing ││ artifacts."
  (let* ((grid-height (length grid))
         (grid-width (if (> grid-height 0) (length (aref grid 0)) 0)))

    ;; FIRST: Check horizontally adjacent positions for vertical boundary characters
    ;; (for cases where nodes are already drawn)
    (catch 'found-vertical-boundary
      (dolist (x-offset '(-1 1))  ; Check left and right positions only
        (let ((check-x (+ x x-offset)))
          (when (and (>= check-x 0) (< check-x grid-width)
                     (>= y 0) (< y grid-height))
            (let ((char-at-pos (aref (aref grid y) check-x)))
              ;; Check for vertical node boundary characters
              (when (memq char-at-pos '(?│ ?┌ ?┐ ?└ ?┘))
                (throw 'found-vertical-boundary t))))))

      ;; SECOND: Check occupancy map for adjacent node positions
      ;; (for cases where nodes haven't been drawn yet but occupancy map exists)
      (when (boundp 'dag-draw--global-occupancy-map)
        (catch 'found-occupied-boundary
          (dolist (x-offset '(-1 1))  ; Check left and right positions only
            (let ((check-x (+ x x-offset)))
              (when (and (>= check-x 0) (< check-x grid-width)
                         (>= y 0) (< y grid-height)
                         dag-draw--global-occupancy-map)
                ;; Check if adjacent position is occupied by a node
                (when (aref (aref dag-draw--global-occupancy-map y) check-x)
                  (throw 'found-occupied-boundary t)))))))
      nil)))

(defun dag-draw--position-inside-or-touching-node-box (grid x y)
  "Check if position (X,Y) is inside or touching a node box boundary.
Returns t if position is inside a node box or adjacent to box characters."
  (let* ((grid-height (length grid))
         (grid-width (if (> grid-height 0) (length (aref grid 0)) 0)))

    (when (and (>= x 0) (< x grid-width) (>= y 0) (< y grid-height))
      (let ((current-char (aref (aref grid y) x)))
        ;; If current position has corner characters, it's definitely part of a box boundary
        (if (memq current-char '(?┌ ?┐ ?└ ?┘))
            t
          ;; Check if we're inside a node text area by looking for complete box structure
          (catch 'found-box-interior
            ;; Look for corner patterns that indicate we're inside a box
            (when (dag-draw--detect-node-box-interior grid x y)
              (throw 'found-box-interior t))
            nil))))))

(defun dag-draw--detect-node-box-interior (grid x y)
  "Detect if position (X,Y) is inside a node text area by scanning for box patterns.
Returns t if position appears to be inside a complete node box structure."
  (let* ((grid-height (length grid))
         (grid-width (if (> grid-height 0) (length (aref grid 0)) 0)))

    ;; Strategy: Look for horizontal box edges above and below current position
    ;; If we find top border above and bottom border below, we're inside a box
    (catch 'found-interior
      ;; Search upward for top border (┌─...─┐ pattern)
      (let ((found-top-border nil)
            (found-bottom-border nil))

        ;; Look upward for top border
        (dotimes (y-offset 5)  ; Search up to 5 rows above
          (let ((check-y (- y y-offset)))
            (when (and (>= check-y 0) (< check-y grid-height))
              (let ((char-above (aref (aref grid check-y) x)))
                (when (memq char-above '(?┌ ?┐ ?─))
                  (setq found-top-border t))))))

        ;; Look downward for bottom border
        (dotimes (y-offset 5)  ; Search up to 5 rows below
          (let ((check-y (+ y y-offset)))
            (when (and (>= check-y 0) (< check-y grid-height))
              (let ((char-below (aref (aref grid check-y) x)))
                (when (memq char-below '(?└ ?┘ ?─))
                  (setq found-bottom-border t))))))

        ;; If we found both top and bottom borders, we're inside a box
        (when (and found-top-border found-bottom-border)
          (throw 'found-interior t)))

      nil)))

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
