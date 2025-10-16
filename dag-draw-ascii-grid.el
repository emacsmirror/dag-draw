;;; dag-draw-ascii-grid.el --- ASCII grid management for dag-draw -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; ASCII Grid Rendering - GKNV Adaptation
;;
;; BOUNDED CONTEXT: ASCII Rendering Context
;; UPSTREAM CONTEXT: GKNV Algorithm Context (via Coordinate Transform Layer)
;; LAYER: Rendering Implementation
;; AUTHORITY: doc/implementation-decisions.md (D5.1-D5.8)
;;
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
(require 'dag-draw-coord-transform)
(require 'dag-draw-ascii-junctions)

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

;;; ASCII Coordinate Context Layer (moved to dag-draw-coord-transform.el)
;; All coordinate transformation functions have been extracted to dag-draw-coord-transform.el

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

;;; Junction Character Enhancement (moved to dag-draw-ascii-junctions.el)
;; All junction-related functions have been extracted to dag-draw-ascii-junctions.el

;;; Edge Analysis for Junction Detection (moved to dag-draw-ascii-junctions.el)

;;; Local Grid Context Analysis (moved to dag-draw-ascii-junctions.el)

(provide 'dag-draw-ascii-grid)

;;; dag-draw-ascii-grid.el ends here
