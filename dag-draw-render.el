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
  ;; GKNV COMPLIANCE: Ensure positioning has completed
  ;; ASCII-first mode doesn't require splines, just positioned nodes
  (unless (and (> (ht-size (dag-draw-graph-nodes graph)) 0)
               ;; Check if ALL nodes have coordinates (not just first one)
               (cl-every (lambda (node) (dag-draw-node-x-coord node))
                         (ht-values (dag-draw-graph-nodes graph))))
    ;; Run GKNV algorithm ONCE if not already done
    (dag-draw-layout-graph graph))

  ;; Handle empty graphs
  (if (= (ht-size (dag-draw-graph-nodes graph)) 0)
      "(Empty Graph)"
    
    ;; ASCII-first mode: coordinates are already in grid units, no conversion needed
    (dag-draw--render-ascii-native graph)))

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
         
         ;; Create grid with padding (ensure integers for make-vector)
         (grid-width (round (max 20 (+ (- max-x min-x) 10))))
         (grid-height (round (max 10 (+ (- max-y min-y) 5))))
         (grid (dag-draw--create-ascii-grid grid-width grid-height)))
    
    (message "\n=== ASCII-NATIVE RENDERING ===")
    (message "Grid bounds: (%d,%d) to (%d,%d)" min-x min-y max-x max-y)
    (message "Grid size: %dx%d" grid-width grid-height)
    
    ;; Draw nodes directly using ASCII coordinates (ensure integers)
    (dolist (node nodes)
      (let ((x (round (- (or (dag-draw-node-x-coord node) 0) min-x)))
            (y (round (- (or (dag-draw-node-y-coord node) 0) min-y)))
            (label (dag-draw-node-label node))
            (width (+ (length (dag-draw-node-label node)) 4))  ; Label + padding
            (height 3))  ; Standard node height
        (dag-draw--draw-node-box grid x y width height label)))
    
    ;; Draw edges directly using ASCII coordinates (ensure integers)
    (dolist (edge (dag-draw-graph-edges graph))
      (let* ((from-node (dag-draw-get-node graph (dag-draw-edge-from-node edge)))
             (to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge)))
             (from-x (round (- (or (dag-draw-node-x-coord from-node) 0) min-x)))
             (from-y (round (- (or (dag-draw-node-y-coord from-node) 0) min-y)))
             (to-x (round (- (or (dag-draw-node-x-coord to-node) 0) min-x)))
             (to-y (round (- (or (dag-draw-node-y-coord to-node) 0) min-y))))
        (dag-draw--draw-simple-edge grid from-x from-y to-x to-y from-node to-node)))
    
    ;; Convert grid to string
    (dag-draw--ascii-grid-to-string grid)))

;; DELETED: dag-draw--convert-gknv-to-ascii-grid - obsolete in ASCII-first architecture

;; DELETED: dag-draw--draw-nodes-gknv-compliant - obsolete in ASCII-first architecture

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


;; DELETED: dag-draw--draw-edges-gknv-compliant - obsolete in ASCII-first architecture

;; DELETED: dag-draw--draw-edge-with-proper-ports - obsolete in ASCII-first architecture

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

(defun dag-draw--draw-simple-edge (grid from-x from-y to-x to-y from-node to-node)
  "Draw a simple edge from (FROM-X,FROM-Y) to (TO-X,TO-Y) in GRID.
This is a basic ASCII edge drawing for ASCII-native coordinate mode.
FROM-NODE and TO-NODE are used to calculate proper port positions per GKNV Section 4.2."
  
  ;; Calculate port positions per GKNV Section 4.2: Node Port as X-direction offset from node center
  (let* ((from-node-width (+ (length (dag-draw-node-label from-node)) 4))  ; Actual from-node width
         (to-node-width (+ (length (dag-draw-node-label to-node)) 4))       ; Actual to-node width
         (from-port-x (+ from-x (/ from-node-width 2))) ; Center X of from-node
         (from-port-y (+ from-y 3))                     ; Bottom Y of from-node  
         (to-port-x (+ to-x (/ to-node-width 2)))       ; Center X of to-node
         (to-port-y to-y)                               ; Top Y of to-node
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
