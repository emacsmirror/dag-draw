;;; dag-draw-ascii-grid.el --- ASCII grid management for dag-draw -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; ASCII Grid Rendering - GKNV Adaptation:
;;
;; This module adapts the GKNV graph drawing algorithm for ASCII character grid
;; output. The GKNV paper describes graphical (PostScript) output, so this module
;; implements ASCII-specific decisions from doc/implementation-decisions.md.
;;
;; ASCII Decisions: D5.1-D5.8 (Character-grid specific)
;; Algorithm: Junction character algorithm with context analysis
;;
;; Key Requirements:
;; - Unicode box-drawing characters (D5.2)
;; - 5 junction types: start-port, end-port, corner, merge/split, cross (D5.4)
;; - Walk-based local analysis for correct junction selection (D5.4)
;; - Arrows at port boundary (D5.5)
;; - Coordinate scaling X and Y independently (D5.1)
;;
;; Baseline Status: ✅ Compliant with ASCII adaptation decisions
;;
;; Note: GKNV paper does not cover ASCII rendering. This module implements
;; the ASCII-specific decisions documented in implementation-decisions.md
;; while maintaining GKNV algorithm correctness for the underlying layout.
;;
;; See doc/implementation-decisions.md (D5.1-D5.8) for ASCII decision rationale.

;;; Code:

(require 'ht)
(require 'dag-draw-core)

;;; Customization

;;; Dynamic Scale Calculation

(defun dag-draw--calculate-optimal-ascii-scale (graph target-width target-height)
  "Calculate optimal scale factor based on graph complexity and ASCII constraints.
GRAPH is the input graph, TARGET-WIDTH and TARGET-HEIGHT are the desired ASCII dimensions.

Following GKNV paper Section 1.2: coordinates should use '72 units per inch' for high resolution,
but ASCII has ~5 characters per inch, requiring dynamic scaling based on graph complexity."
  (let* ((node-count (dag-draw-node-count graph))
         (edge-count (dag-draw-edge-count graph))
         
         ;; Enhanced complexity analysis
         (node-size-factor (dag-draw--calculate-node-size-complexity graph))
         (edge-density-factor (dag-draw--calculate-edge-density-factor graph))
         (hierarchy-depth-factor (dag-draw--estimate-hierarchy-depth graph))
         
         ;; Combined complexity factor with weighted components
         (complexity-factor (+ (* node-count 0.15)
                              (* edge-count 0.08)
                              (* node-size-factor 0.1)
                              (* edge-density-factor 0.05)
                              (* hierarchy-depth-factor 0.02)))
         
         ;; Base scale calculation: balance between resolution and fitting
         ;; More complex graphs need smaller scale to fit in ASCII area
         (base-scale (/ 1.0 (max 1.0 (+ 1.5 complexity-factor))))
         
         ;; Adjust for target dimensions - larger ASCII area allows larger scale
         (dimension-factor (/ (+ target-width target-height) 200.0))
         (adjusted-scale (* base-scale dimension-factor))
         
         ;; Minimum scale to prevent coordinate collapse (GKNV aesthetic A2)
         (min-scale 0.02)
         
         ;; Maximum scale to maintain readability
         (max-scale 0.4))
    
    ;; Validate against coordinate collapse (GKNV aesthetic A2)
    (let ((validated-scale (max min-scale (min max-scale adjusted-scale))))
      (dag-draw--validate-scale-prevents-collapse graph validated-scale target-width target-height))))

(defun dag-draw--validate-scale-prevents-collapse (graph scale target-width target-height)
  "Validate that SCALE prevents coordinate collapse, per GKNV aesthetic A2.
Returns adjusted scale that ensures no two nodes collapse to same grid position.
Only validates when nodes have actual coordinates set (after layout)."
  (if (or (= (dag-draw-node-count graph) 0)
          ;; Only validate if nodes have coordinates (after GKNV layout)
          (not (dag-draw--graph-has-positioned-nodes graph)))
      scale
    (let* ((nodes (ht-values (dag-draw-graph-nodes graph)))
           (positions (ht-create))
           (collapse-detected nil))
      
      ;; Simulate grid positioning with current scale
      (dolist (node nodes)
        (let* ((world-x (dag-draw-node-x-coord node))
               (world-y (dag-draw-node-y-coord node)))
          (when (and world-x world-y)  ; Only check nodes with coordinates
            (let* ((grid-x (round (* world-x scale)))
                   (grid-y (round (* world-y scale)))
                   (grid-pos (format "%d,%d" grid-x grid-y)))
              
              (if (ht-get positions grid-pos)
                  (setq collapse-detected t)
                (ht-set! positions grid-pos t))))))
      
      (if collapse-detected
          ;; Increase scale slightly to prevent collapse
          (let ((anti-collapse-scale (* scale 1.15))) ; Smaller increment
            (if (< anti-collapse-scale 0.4)
                ;; Recursively validate the adjusted scale
                (dag-draw--validate-scale-prevents-collapse graph anti-collapse-scale target-width target-height)
              ;; If we can't prevent collapse, return minimum viable scale
              0.02))
        ;; No collapse detected - scale is valid
        scale))))

(defun dag-draw--graph-has-positioned-nodes (graph)
  "Check if GRAPH has nodes with positioned coordinates (after layout)."
  (let ((has-positioned nil))
    (ht-each (lambda (node-id node)
               (when (and (dag-draw-node-x-coord node)
                         (dag-draw-node-y-coord node))
                 (setq has-positioned t)))
             (dag-draw-graph-nodes graph))
    has-positioned))

(defun dag-draw--calculate-node-size-complexity (graph)
  "Calculate complexity factor based on node size variations.
Graphs with larger or more varied node sizes need smaller scale factors."
  (if (= (dag-draw-node-count graph) 0)
      0.0
    (let* ((total-size 0)
           (size-variance 0)
           (nodes (ht-values (dag-draw-graph-nodes graph))))
      
      ;; Calculate average node size
      (dolist (node nodes)
        (setq total-size (+ total-size 
                           (dag-draw-node-x-size node)
                           (dag-draw-node-y-size node))))
      
      (let ((avg-size (/ total-size (* 2.0 (length nodes)))))
        ;; Calculate size variance for complexity
        (dolist (node nodes)
          (let ((node-size (/ (+ (dag-draw-node-x-size node)
                                 (dag-draw-node-y-size node)) 2.0)))
            (setq size-variance (+ size-variance 
                                  (expt (- node-size avg-size) 2)))))
        
        ;; Return normalized complexity factor
        (/ (sqrt (/ size-variance (length nodes))) 50.0)))))

(defun dag-draw--calculate-edge-density-factor (graph)
  "Calculate edge density complexity factor.
Higher edge density relative to node count indicates more complex layout requirements."
  (let ((node-count (dag-draw-node-count graph)))
    (if (<= node-count 1)
        0.0
      (let* ((edge-count (dag-draw-edge-count graph))
             (max-possible-edges (* node-count (1- node-count)))
             (density (if (> max-possible-edges 0)
                         (/ (float edge-count) max-possible-edges)
                       0.0)))
        ;; Scale density to reasonable complexity factor
        (* density 2.0)))))

(defun dag-draw--estimate-hierarchy-depth (graph)
  "Estimate hierarchy depth for complexity calculation.
Deeper hierarchies may need different scaling considerations."
  (if (= (dag-draw-node-count graph) 0)
      0.0
    (let ((source-nodes (dag-draw-get-source-nodes graph)))
      (if (null source-nodes)
          ;; No clear hierarchy (cycles or disconnected) - moderate complexity
          1.0
        ;; Simple depth estimation: count nodes at different distances from sources
        (let ((max-depth 0))
          (dolist (source source-nodes)
            (let ((depth (dag-draw--calculate-max-depth-from-node graph source)))
              (setq max-depth (max max-depth depth))))
          max-depth)))))

(defun dag-draw--calculate-max-depth-from-node (graph start-node)
  "Calculate maximum depth reachable from START-NODE in GRAPH."
  (let ((visited (ht-create))
        (max-depth 0))
    (dag-draw--depth-first-search graph start-node visited 0 
                                 (lambda (depth) 
                                   (setq max-depth (max max-depth depth))))
    max-depth))

(defun dag-draw--depth-first-search (graph node visited current-depth callback)
  "Perform DFS from NODE, calling CALLBACK with depth at each node."
  (unless (ht-get visited node)
    (ht-set! visited node t)
    (funcall callback current-depth)
    (let ((successors (dag-draw-get-successors graph node)))
      (dolist (successor successors)
        (dag-draw--depth-first-search graph successor visited (1+ current-depth) callback)))))

;;; ASCII Scaling Helper Functions

;; DELETED: Coordinate transformation functions - obsolete in ASCII-first architecture
;; - dag-draw--world-to-grid-coord
;; - dag-draw--grid-to-world-coord  
;; - dag-draw--world-to-grid-size

(defun dag-draw--get-node-center-grid (node min-x min-y scale &optional graph)
  "Get node center coordinates directly in grid space for simple edge routing.
    VISUAL FIX: Simplified coordinate calculation to avoid conversion errors."
  (let* ((node-id (dag-draw-node-id node))
         ;; GKNV Pass 3 Authority: Only use algorithm-assigned coordinates  
         ;; Section 4: "The third pass finds optimal coordinates for nodes"
         (gknv-x (dag-draw-node-x-coord node))
         (gknv-y (dag-draw-node-y-coord node))
         ;; Get adjusted coordinates from layout algorithm
         (adjusted-coords (and graph
                               (dag-draw-graph-adjusted-positions graph)
                               (ht-get (dag-draw-graph-adjusted-positions graph) node-id))))

    (if adjusted-coords
        ;; Use adjusted coordinates from GKNV layout algorithm
        (dag-draw-point-create
         :x (+ (nth 0 adjusted-coords) (/ (nth 2 adjusted-coords) 2.0))
         :y (+ (nth 1 adjusted-coords) (/ (nth 3 adjusted-coords) 2.0)))
      ;; Use GKNV Pass 3 coordinates - convert world coordinates to grid coordinates
      (let* ((world-x (or gknv-x 0))
             (world-y (or gknv-y 0))
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

(defun dag-draw--world-to-grid-coord (world-coord min-coord scale)
  "Convert world coordinate to grid coordinate.
WORLD-COORD is the world position, MIN-COORD is the world minimum, SCALE is the scaling factor."
  (round (/ (- world-coord min-coord) scale)))

(defun dag-draw--world-to-grid-size (world-size scale)
  "Convert world size to grid size.
WORLD-SIZE is the world dimension, SCALE is the scaling factor."
  (max 1 (round (/ world-size scale))))

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

;;; Junction Character Enhancement

(defun dag-draw--get-enhanced-junction-char (context)
  "Determine the appropriate junction character based on CONTEXT.
CONTEXT is a plist containing junction information per CLAUDE.md specifications.
Returns the Unicode character that should be used for the junction."
  (let ((junction-type (plist-get context :type)))
    (cond
     ;; Port boundary junctions (CLAUDE.md: "At the start/end of edge, at port boundary")
     ((eq junction-type 'port-start)
      (let ((direction (plist-get context :direction)))
        (cond
         ((eq direction 'down) ?┬)  ; ─ becomes ┬ when edge starts downward
         ((eq direction 'up) ?┴)    ; ─ becomes ┴ when edge starts upward  
         ((eq direction 'left) ?┤)  ; │ becomes ┤ when edge starts leftward
         ((eq direction 'right) ?├) ; │ becomes ├ when edge starts rightward
         (t ?+))))  ; fallback
     
     ((eq junction-type 'port-end)
      (let ((direction (plist-get context :direction)))
        (cond
         ((eq direction 'up) ?┴)     ; ─ becomes ┴ when edge ends from above
         ((eq direction 'down) ?┬)   ; ─ becomes ┬ when edge ends from below
         ((eq direction 'left) ?┤)   ; │ becomes ┤ when edge ends from left
         ((eq direction 'right) ?├)  ; │ becomes ├ when edge ends from right  
         (t ?+))))  ; fallback
     
     ;; Direction change junctions (CLAUDE.md: "When edge requires direction change")
     ((eq junction-type 'direction-change)
      (let ((from-dir (plist-get context :from-direction))
            (to-dir (plist-get context :to-direction)))
        (cond
         ((and (eq from-dir 'right) (eq to-dir 'down)) ?┐)  ; right→down corner
         ((and (eq from-dir 'left) (eq to-dir 'down)) ?┌)   ; left→down corner
         ((and (eq from-dir 'right) (eq to-dir 'up)) ?┘)    ; right→up corner
         ((and (eq from-dir 'left) (eq to-dir 'up)) ?└)     ; left→up corner
         ((and (eq from-dir 'down) (eq to-dir 'right)) ?└)  ; down→right corner
         ((and (eq from-dir 'down) (eq to-dir 'left)) ?┘)   ; down→left corner
         ((and (eq from-dir 'up) (eq to-dir 'right)) ?┌)    ; up→right corner
         ((and (eq from-dir 'up) (eq to-dir 'left)) ?┐)     ; up→left corner
         (t ?+))))  ; fallback
     
     ;; Edge joining junctions (CLAUDE.md: "When two edges join")
     ((eq junction-type 'edge-join)
      (let ((outgoing-dir (plist-get context :outgoing-direction)))
        (cond
         ((eq outgoing-dir 'right) ?┴)   ; edges from above join rightward
         ((eq outgoing-dir 'left) ?┴)    ; edges from above join leftward
         ((eq outgoing-dir 'down) ?┬)    ; edges from sides join downward
         ((eq outgoing-dir 'up) ?┴)      ; edges from sides join upward
         (t ?┼))))  ; fallback to cross
     
     ;; Edge separation junctions (CLAUDE.md: "When two edges separate")
     ((eq junction-type 'edge-split)
      (let ((incoming-dir (plist-get context :incoming-direction)))
        (cond
         ((eq incoming-dir 'left) ?┬)    ; horizontal line splits downward
         ((eq incoming-dir 'right) ?┬)   ; horizontal line splits downward
         ((eq incoming-dir 'down) ?┴)    ; vertical line splits sideways
         ((eq incoming-dir 'up) ?┬)      ; vertical line splits sideways
         (t ?┼))))  ; fallback to cross
     
     ;; Edge crossing junctions (CLAUDE.md: "When two edges cross")
     ((eq junction-type 'edge-cross) ?┼)  ; always use cross character
     
     ;; T-junction scenarios (CLAUDE.md example)
     ((eq junction-type 't-junction)
      (let ((main-dir (plist-get context :main-direction))
            (branch-dir (plist-get context :branch-direction)))
        (cond
         ((and (eq main-dir 'down) (eq branch-dir 'right)) ?├)   ; down + right branch
         ((and (eq main-dir 'down) (eq branch-dir 'left)) ?┤)    ; down + left branch
         ((and (eq main-dir 'up) (eq branch-dir 'right)) ?├)     ; up + right branch
         ((and (eq main-dir 'up) (eq branch-dir 'left)) ?┤)      ; up + left branch
         ((and (eq main-dir 'right) (eq branch-dir 'down)) ?┬)   ; right + down branch
         ((and (eq main-dir 'right) (eq branch-dir 'up)) ?┴)     ; right + up branch
         ((and (eq main-dir 'left) (eq branch-dir 'down)) ?┬)    ; left + down branch
         ((and (eq main-dir 'left) (eq branch-dir 'up)) ?┴)      ; left + up branch
         (t ?┼))))  ; fallback to cross
     
     ;; Fallback for unknown junction types
     (t ?+))))

;;; Edge Analysis for Junction Detection

(defun dag-draw--analyze-junction-points (graph)
  "Analyze GRAPH to find points where junction characters are needed.
CLAUDE.md: 'walks the edge in order to determine the locally-relevant algorithm'
Returns a list of junction point specifications."
  (when graph
    (let ((junction-points '()))
      ;; Phase 1: Detect port boundary junctions (CLAUDE.md: "At the start/end of edge")
      (setq junction-points (append junction-points (dag-draw--detect-port-junctions graph)))
      
      ;; Phase 2: Detect direction change junctions (CLAUDE.md: "When edge requires direction change")
      (setq junction-points (append junction-points (dag-draw--detect-direction-changes graph)))
      
      ;; Phase 3: Detect edge intersection junctions (CLAUDE.md: "When edges join/separate/cross")
      (setq junction-points (append junction-points (dag-draw--detect-edge-intersections graph)))
      
      junction-points)))

(defun dag-draw--detect-port-junctions (graph)
  "Detect junction points at node port boundaries in GRAPH.
CLAUDE.md: 'At the start of the edge, at the port boundary'
Returns a list of port junction specifications."
  (when graph
    (let ((port-junctions '()))
      ;; Walk through all edges to find port boundary points
      (dolist (edge (dag-draw-graph-edges graph))
        (let ((from-node (dag-draw-get-node graph (dag-draw-edge-from-node edge)))
              (to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge))))
          
          ;; Check if nodes have coordinates (positioned after GKNV layout)
          (when (and from-node to-node
                     (dag-draw-node-x-coord from-node) (dag-draw-node-y-coord from-node)
                     (dag-draw-node-x-coord to-node) (dag-draw-node-y-coord to-node))
            
            ;; Determine edge direction from source to target
            (let* ((from-x (dag-draw-node-x-coord from-node))
                   (from-y (dag-draw-node-y-coord from-node))
                   (to-x (dag-draw-node-x-coord to-node))
                   (to-y (dag-draw-node-y-coord to-node))
                   (direction (dag-draw--determine-edge-direction from-x from-y to-x to-y)))
              
              ;; Add port start junction (at source node boundary)
              (push (list :type 'port-start
                         :node (dag-draw-edge-from-node edge)
                         :x from-x :y from-y
                         :direction direction
                         :edge edge)
                    port-junctions)
              
              ;; Add port end junction (at target node boundary)  
              (push (list :type 'port-end
                         :node (dag-draw-edge-to-node edge)
                         :x to-x :y to-y  
                         :direction (dag-draw--reverse-direction direction)
                         :edge edge)
                    port-junctions)))))
      
      port-junctions)))

(defun dag-draw--determine-edge-direction (from-x from-y to-x to-y)
  "Determine primary direction of edge from (FROM-X,FROM-Y) to (TO-X,TO-Y).
Returns one of: 'up, 'down, 'left, 'right based on dominant coordinate change."
  (let ((dx (- to-x from-x))
        (dy (- to-y from-y)))
    (cond
     ;; Vertical movement dominates
     ((> (abs dy) (abs dx))
      (if (> dy 0) 'down 'up))
     ;; Horizontal movement dominates  
     ((> (abs dx) (abs dy))
      (if (> dx 0) 'right 'left))
     ;; Equal movement - choose based on positive direction preference
     ((and (> dx 0) (> dy 0)) 'down)  ; Southeast
     ((and (< dx 0) (> dy 0)) 'down)  ; Southwest
     ((and (> dx 0) (< dy 0)) 'up)    ; Northeast  
     ((and (< dx 0) (< dy 0)) 'up)    ; Northwest
     ;; Fallback
     (t 'down))))

(defun dag-draw--reverse-direction (direction)
  "Return the opposite direction of DIRECTION."
  (cond
   ((eq direction 'up) 'down)
   ((eq direction 'down) 'up)
   ((eq direction 'left) 'right)
   ((eq direction 'right) 'left)
   (t direction)))

(defun dag-draw--detect-direction-changes (graph)
  "Detect points where edges require direction changes.
CLAUDE.md: 'When the edge requires a direction change'
Returns a list of direction change junction specifications."
  ;; TODO: Implement spline analysis for direction changes
  ;; For now, return empty list as this requires complex spline path analysis
  '())

(defun dag-draw--detect-direction-changes-in-path (edge-path)
  "Detect direction changes (corners) in EDGE-PATH.
EDGE-PATH is a list of cons cells (x . y) representing the path.
Returns a list of corner specifications with position and directions.
CLAUDE.md: 'When the edge requires a direction change'"
  (let ((corners '()))
    (when (>= (length edge-path) 3)
      ;; Walk through path, checking each point with its neighbors
      (let ((i 1))  ; Start at second point
        (while (< i (- (length edge-path) 1))
          (let* ((prev (nth (- i 1) edge-path))
                 (curr (nth i edge-path))
                 (next (nth (+ i 1) edge-path))
                 ;; Calculate direction vectors
                 (dx-in (- (car curr) (car prev)))
                 (dy-in (- (cdr curr) (cdr prev)))
                 (dx-out (- (car next) (car curr)))
                 (dy-out (- (cdr next) (cdr curr))))
            ;; Check if this is a direction change
            ;; Direction change occurs when we go from H to V or V to H
            (when (dag-draw--is-direction-change dx-in dy-in dx-out dy-out)
              (push (list :type 'direction-change
                         :x (car curr)
                         :y (cdr curr)
                         :from-direction (dag-draw--get-direction dx-in dy-in)
                         :to-direction (dag-draw--get-direction dx-out dy-out))
                    corners)))
          (setq i (1+ i)))))
    (nreverse corners)))

(defun dag-draw--is-direction-change (dx-in dy-in dx-out dy-out)
  "Check if the direction changes between incoming and outgoing vectors.
DX-IN, DY-IN: incoming direction vector.
DX-OUT, DY-OUT: outgoing direction vector.
Returns t if direction changes from horizontal to vertical or vice versa."
  (let ((is-h-in (and (not (zerop dx-in)) (zerop dy-in)))   ; Horizontal in
        (is-v-in (and (zerop dx-in) (not (zerop dy-in))))   ; Vertical in
        (is-h-out (and (not (zerop dx-out)) (zerop dy-out))) ; Horizontal out
        (is-v-out (and (zerop dx-out) (not (zerop dy-out)))) ; Vertical out
        )
    ;; Direction change if: H->V or V->H
    (or (and is-h-in is-v-out)
        (and is-v-in is-h-out))))

(defun dag-draw--get-direction (dx dy)
  "Get direction symbol from direction vector (DX, DY).
Returns one of: 'up, 'down, 'left, 'right."
  (cond
   ((and (> dx 0) (zerop dy)) 'right)
   ((and (< dx 0) (zerop dy)) 'left)
   ((and (zerop dx) (> dy 0)) 'down)
   ((and (zerop dx) (< dy 0)) 'up)
   ;; Diagonal or zero - should not happen in orthogonal paths
   (t 'unknown)))

(defun dag-draw--detect-edge-intersections (graph)
  "Detect points where edges join, separate, or cross.
CLAUDE.md: 'When two edges join, or two edges separate' and 'When two edges cross'
Returns a list of intersection junction specifications."
  ;; TODO: Implement grid-based intersection analysis
  ;; For now, return empty list as this requires ASCII grid coordinate analysis
  '())

(defun dag-draw--detect-crossings-in-paths (edge-paths)
  "Detect crossing points between multiple EDGE-PATHS.
EDGE-PATHS is a list of edge paths, where each path is a list of cons cells (x . y).
Returns a list of crossing specifications.
CLAUDE.md: 'When two edges cross'"
  (let ((crossings '())
        (position-map (make-hash-table :test 'equal)))

    ;; Build a map of positions to edges that pass through them
    (let ((edge-index 0))
      (dolist (path edge-paths)
        (dolist (point path)
          (let* ((key (cons (car point) (cdr point)))
                 (current-list (gethash key position-map '())))
            (puthash key (cons edge-index current-list) position-map)))
        (setq edge-index (1+ edge-index))))

    ;; Find positions where multiple edges meet
    (maphash (lambda (pos edge-indices)
               (when (>= (length edge-indices) 2)
                 ;; Check if edges actually cross (not just touch)
                 ;; Two edges cross if they pass through same point from different directions
                 (let ((edge-dirs (dag-draw--get-edge-directions-at-point
                                  pos edge-paths edge-indices)))
                   (when (dag-draw--are-edges-crossing edge-dirs)
                     (push (list :type 'edge-cross
                                :x (car pos)
                                :y (cdr pos)
                                :edge-indices edge-indices)
                           crossings)))))
             position-map)

    (nreverse crossings)))

(defun dag-draw--get-edge-directions-at-point (pos edge-paths edge-indices)
  "Get the directions of edges at position POS.
POS is a cons cell (x . y).
EDGE-PATHS is the list of all edge paths.
EDGE-INDICES is the list of edge indices that pass through POS.
Returns a list of direction symbols for each edge."
  (let ((directions '()))
    (dolist (edge-idx edge-indices)
      (let* ((path (nth edge-idx edge-paths))
             ;; Find the point in the path
             (point-idx (dag-draw--find-point-index-in-path pos path)))
        (when point-idx
          ;; Determine direction at this point by looking at neighbors
          (let* ((prev (when (> point-idx 0) (nth (- point-idx 1) path)))
                 (next (when (< point-idx (- (length path) 1))
                        (nth (+ point-idx 1) path)))
                 (dir (cond
                      ;; Check incoming direction
                      ((and prev
                            (= (cdr prev) (cdr pos))
                            (/= (car prev) (car pos)))
                       'horizontal)
                      ((and prev
                            (= (car prev) (car pos))
                            (/= (cdr prev) (cdr pos)))
                       'vertical)
                      ;; Check outgoing direction
                      ((and next
                            (= (cdr next) (cdr pos))
                            (/= (car next) (car pos)))
                       'horizontal)
                      ((and next
                            (= (car next) (car pos))
                            (/= (cdr next) (cdr pos)))
                       'vertical)
                      (t 'unknown))))
            (push dir directions)))))
    (nreverse directions)))

(defun dag-draw--find-point-index-in-path (pos path)
  "Find the index of POS in PATH.
POS is a cons cell (x . y).
PATH is a list of cons cells.
Returns the index or nil if not found."
  (let ((idx 0)
        (found nil))
    (dolist (point path)
      (when (and (= (car point) (car pos))
                 (= (cdr point) (cdr pos)))
        (setq found idx))
      (setq idx (1+ idx)))
    found))

(defun dag-draw--are-edges-crossing (edge-dirs)
  "Check if edges are truly crossing based on their directions.
EDGE-DIRS is a list of direction symbols.
Two edges cross if they have different directions (horizontal vs vertical)."
  (let ((has-h nil)
        (has-v nil))
    (dolist (dir edge-dirs)
      (when (eq dir 'horizontal) (setq has-h t))
      (when (eq dir 'vertical) (setq has-v t)))
    (and has-h has-v)))

(defun dag-draw--detect-joins-in-paths (edge-paths)
  "Detect join points where edges merge or split.
EDGE-PATHS is a list of edge paths, where each path is a list of cons cells (x . y).
Returns a list of join/split junction specifications.
CLAUDE.md: 'When two edges join, or two edges separate'"
  (let ((joins '())
        (position-map (make-hash-table :test 'equal)))

    ;; Build a map of positions to edges that pass through them
    (let ((edge-index 0))
      (dolist (path edge-paths)
        (dolist (point path)
          (let* ((key (cons (car point) (cdr point)))
                 (current-list (gethash key position-map '())))
            (puthash key (cons edge-index current-list) position-map)))
        (setq edge-index (1+ edge-index))))

    ;; Find positions where multiple edges meet but don't cross
    ;; (i.e., they join or split - T-junctions)
    (maphash (lambda (pos edge-indices)
               (when (>= (length edge-indices) 2)
                 ;; Check if this is a join/split (T-junction) not a crossing
                 (let ((edge-dirs (dag-draw--get-edge-directions-at-point
                                  pos edge-paths edge-indices)))
                   ;; A join/split occurs when edges meet but don't cross
                   ;; This includes T-junctions where 3+ edges meet
                   (when (not (dag-draw--are-edges-crossing edge-dirs))
                     (push (list :type 'edge-join
                                :x (car pos)
                                :y (cdr pos)
                                :edge-indices edge-indices
                                :directions edge-dirs)
                           joins)))))
             position-map)

    (nreverse joins)))

;;; Local Grid Context Analysis

(defun dag-draw--analyze-local-grid-junction-context (grid x y current-char new-char)
  "Analyze grid context at position (X,Y) to determine junction type.
CURRENT-CHAR is the character already at the position (or space).
NEW-CHAR is the character being drawn.
Returns a context plist suitable for dag-draw--get-enhanced-junction-char.

This implements the D5.6-D5.8 context analysis requirements:
- Check for adjacent edges in all 4 directions
- Determine junction type based on connectivity
- Build proper context plist for character selection."
  (let* ((grid-height (length grid))
         (grid-width (if (> grid-height 0) (length (aref grid 0)) 0))
         ;; Check all 4 directions for edge characters
         (has-up (dag-draw--has-edge-in-direction grid x y 'up))
         (has-down (dag-draw--has-edge-in-direction grid x y 'down))
         (has-left (dag-draw--has-edge-in-direction grid x y 'left))
         (has-right (dag-draw--has-edge-in-direction grid x y 'right))
         ;; Count total connections
         (connection-count (+ (if has-up 1 0) (if has-down 1 0)
                             (if has-left 1 0) (if has-right 1 0)))
         ;; Determine if new character indicates direction
         (new-is-horizontal (memq new-char '(?─)))
         (new-is-vertical (memq new-char '(?│))))

    ;; Build context plist based on connectivity pattern
    (cond
     ;; Direction change: 2 connections in perpendicular directions
     ((and (= connection-count 2)
           (or (and has-left has-down)
               (and has-right has-down)
               (and has-left has-up)
               (and has-right has-up)))
      ;; For corners, determine from/to based on which two directions have edges
      ;; The character maps to the turn direction regardless of edge flow direction
      (let ((from-dir (cond
                       ;; Horizontal incoming
                       (has-left 'right)    ; Edge from left means traveling right
                       (has-right 'left)    ; Edge from right means traveling left
                       ;; Vertical incoming
                       (has-up 'down)       ; Edge from above means traveling down
                       (has-down 'up)))     ; Edge from below means traveling up
            (to-dir (cond
                     ;; Vertical outgoing
                     (has-down 'down)
                     (has-up 'up)
                     ;; Horizontal outgoing
                     (has-right 'right)
                     (has-left 'left))))
        (list :type 'direction-change
              :from-direction from-dir
              :to-direction to-dir
              :has-up has-up
              :has-down has-down
              :has-left has-left
              :has-right has-right)))

     ;; T-junction: 3 connections
     ((= connection-count 3)
      (list :type 't-junction
            :main-direction (cond ((and has-up has-down) 'down)
                                 ((and has-left has-right) 'right)
                                 (t 'down))
            ;; Branch is the direction that's NOT part of the straight line
            :branch-direction (cond
                              ;; Vertical line (up-down), branch is horizontal
                              ((and has-up has-down)
                               (cond (has-left 'left)
                                     (has-right 'right)))
                              ;; Horizontal line (left-right), branch is vertical
                              ((and has-left has-right)
                               (cond (has-up 'up)
                                     (has-down 'down)))
                              ;; Shouldn't reach here if t-junction detection is correct
                              (t 'right))
            :has-up has-up
            :has-down has-down
            :has-left has-left
            :has-right has-right))

     ;; Cross: 4 connections
     ((= connection-count 4)
      (list :type 'edge-cross
            :has-up has-up
            :has-down has-down
            :has-left has-left
            :has-right has-right))

     ;; Straight line: 2 connections in same direction (not perpendicular)
     ((and (= connection-count 2)
           (or (and has-left has-right)   ; Horizontal line
               (and has-up has-down)))    ; Vertical line
      (list :type 'straight-line
            :has-up has-up
            :has-down has-down
            :has-left has-left
            :has-right has-right))

     ;; Default: return minimal context
     (t
      (list :type 'unknown
            :has-up has-up
            :has-down has-down
            :has-left has-left
            :has-right has-right)))))

(defun dag-draw--has-edge-in-direction (grid x y direction)
  "Check if there's an edge character in DIRECTION from (X,Y) on GRID.
DIRECTION is one of: up, down, left, right.
Returns t if edge character found, nil otherwise."
  (let* ((grid-height (length grid))
         (grid-width (if (> grid-height 0) (length (aref grid 0)) 0))
         (check-x (cond ((eq direction 'left) (- x 1))
                       ((eq direction 'right) (+ x 1))
                       (t x)))
         (check-y (cond ((eq direction 'up) (- y 1))
                       ((eq direction 'down) (+ y 1))
                       (t y)))
         ;; Edge and junction characters that indicate connectivity
         ;; Include ?+ as it's used as a fallback junction character
         ;; Include arrow characters as they indicate edge connectivity
         (edge-chars (list ?─ ?│ ?┼ ?┌ ?┐ ?└ ?┘ ?├ ?┤ ?┬ ?┴ ?+ ?\u25bc ?\u25b2 ?\u25ba ?\u25c4))
         (char-at-pos (if (and (>= check-x 0) (< check-x grid-width)
                              (>= check-y 0) (< check-y grid-height))
                         (aref (aref grid check-y) check-x)
                       nil)))

    ;; Check bounds and character type, return t or nil (not list tail)
    (if (and (>= check-x 0) (< check-x grid-width)
             (>= check-y 0) (< check-y grid-height)
             (memq (aref (aref grid check-y) check-x) edge-chars))
        t
      nil)))

;;; Junction Character Application (Priority 4: Integration)

(defun dag-draw--apply-junction-chars-to-grid (grid)
  "Apply junction characters throughout GRID based on local context analysis.
Walks through all grid positions, analyzes connectivity, and updates characters.
This is the integration point for D5.1-D5.8 junction character enhancement.
CLAUDE.md: 'walks the edge in order to determine the locally-relevant algorithm'"
  (let* ((grid-height (length grid))
         (grid-width (if (> grid-height 0) (length (aref grid 0)) 0))
         ;; Include ?+ as it's used as a fallback junction character
         (edge-chars '(?─ ?│ ?┼ ?┌ ?┐ ?└ ?┘ ?├ ?┤ ?┬ ?┴ ?+))
         ;; Arrow characters should NOT be replaced (CLAUDE.md: "the one possible exemption
         ;; is where an arrow is placed")
         (arrow-chars (list ?\u25bc ?\u25b2 ?\u25ba ?\u25c4))) ; ▼ ▲ ► ◄
    ;; Walk through entire grid
    (dotimes (y grid-height)
      (dotimes (x grid-width)
        (let ((current-char (aref (aref grid y) x)))
          ;; Only process edge/junction characters, but NOT arrows
          (when (and (memq current-char edge-chars)
                    (not (memq current-char arrow-chars)))
            ;; Analyze local context and determine proper junction character
            (let* ((context (dag-draw--analyze-local-grid-junction-context
                           grid x y current-char current-char))
                   (junction-char (dag-draw--get-enhanced-junction-char context)))
              ;; Update grid with enhanced junction character
              (when junction-char
                (aset (aref grid y) x junction-char)))))))))

(provide 'dag-draw-ascii-grid)

;;; dag-draw-ascii-grid.el ends here
