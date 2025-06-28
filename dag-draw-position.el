;;; dag-draw-position.el --- Node positioning for dag-draw -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Implementation of the node positioning pass of the GKNV algorithm.
;; This module assigns X and Y coordinates to nodes after rank assignment
;; and vertex ordering. It uses auxiliary graph construction and network
;; simplex to find optimal X coordinates, as described in section 4 of
;; the research paper.

;;; Code:

(require 'dash)
(require 'ht)
(require 'dag-draw)
(require 'dag-draw-core)

;;; Y-coordinate assignment (straightforward)

(defun dag-draw--assign-y-coordinates (graph)
  "Assign Y coordinates to nodes based on their ranks.
This is straightforward - nodes in the same rank get the same Y coordinate."
  (let ((rank-separation (dag-draw-graph-rank-separation graph))
        (max-rank (or (dag-draw-graph-max-rank graph) 0)))
    
    ;; Assign Y coordinates from top to bottom
    (ht-each (lambda (node-id node)
               (let ((rank (or (dag-draw-node-rank node) 0)))
                 (setf (dag-draw-node-y-coord node)
                       (* rank rank-separation))))
             (dag-draw-graph-nodes graph))))

;;; X-coordinate assignment using auxiliary graph

(defun dag-draw--create-auxiliary-graph (graph)
  "Create auxiliary graph for X-coordinate optimization.
Returns a new graph where each original edge (u,v) is replaced by
two edges (n_e, u) and (n_e, v) through an auxiliary node n_e."
  (let ((aux-graph (dag-draw-create-graph))
        (edge-counter 0))
    
    ;; Add all original nodes to auxiliary graph
    (ht-each (lambda (node-id node)
               (dag-draw-add-node aux-graph node-id 
                                 (dag-draw-node-label node)))
             (dag-draw-graph-nodes graph))
    
    ;; For each original edge, create auxiliary node and edges
    (dolist (edge (dag-draw-graph-edges graph))
      (let* ((edge-id (intern (format "edge_%d" (cl-incf edge-counter))))
             (from-node (dag-draw-edge-from-node edge))
             (to-node (dag-draw-edge-to-node edge))
             (edge-weight (dag-draw-edge-weight edge)))
        
        ;; Add auxiliary node for this edge
        (dag-draw-add-node aux-graph edge-id "")
        
        ;; Add two edges: (edge_node, from) and (edge_node, to)
        ;; with weight = original_weight * omega_factor
        (let ((omega-factor (dag-draw--get-omega-factor graph from-node to-node)))
          (dag-draw-add-edge aux-graph edge-id from-node
                           (* edge-weight omega-factor))
          (dag-draw-add-edge aux-graph edge-id to-node
                           (* edge-weight omega-factor)))))
    
    ;; Add separation constraints between adjacent nodes in same rank
    (dag-draw--add-separation-edges aux-graph graph)
    
    aux-graph))

(defun dag-draw--get-omega-factor (graph from-node to-node)
  "Get omega factor for edge cost based on node types.
Real-real edges: 1, real-virtual: 2, virtual-virtual: 8"
  (let ((from-virtual (dag-draw--is-virtual-node-p from-node))
        (to-virtual (dag-draw--is-virtual-node-p to-node)))
    (cond
     ((and (not from-virtual) (not to-virtual)) 1)   ; both real
     ((and from-virtual to-virtual) 8)               ; both virtual
     ((or from-virtual to-virtual) 2)                ; one virtual
     (t 1))))                                        ; fallback

(defun dag-draw--is-virtual-node-p (node-id)
  "Check if node is a virtual node (starts with 'virtual_')."
  (string-match "^virtual_" (symbol-name node-id)))

(defun dag-draw--add-separation-edges (aux-graph original-graph)
  "Add separation constraint edges between adjacent nodes in same rank."
  (let ((rank-to-nodes (ht-create)))
    
    ;; Group nodes by rank
    (ht-each (lambda (node-id node)
               (let ((rank (or (dag-draw-node-rank node) 0)))
                 (ht-set! rank-to-nodes rank
                          (cons node-id (ht-get rank-to-nodes rank '())))))
             (dag-draw-graph-nodes original-graph))
    
    ;; Add separation edges within each rank
    (ht-each (lambda (rank node-list)
               (let ((ordered-nodes (dag-draw--get-ordered-nodes-in-rank 
                                   original-graph node-list)))
                 (dotimes (i (1- (length ordered-nodes)))
                   (let ((left-node (nth i ordered-nodes))
                         (right-node (nth (1+ i) ordered-nodes)))
                     
                     ;; Add separation edge with minimum distance constraint
                     (let ((sep-distance (dag-draw--calculate-separation 
                                        original-graph left-node right-node)))
                       (let ((sep-edge (dag-draw-edge-create
                                      :from-node left-node
                                      :to-node right-node
                                      :weight 0  ; No cost, just constraint
                                      :min-length sep-distance)))
                         (push sep-edge (dag-draw-graph-edges aux-graph))))))))
             rank-to-nodes)))

(defun dag-draw--get-ordered-nodes-in-rank (graph node-list)
  "Get nodes in rank ordered by their assigned order."
  (sort node-list
        (lambda (a b)
          (let ((order-a (or (dag-draw-node-order (dag-draw-get-node graph a)) 0))
                (order-b (or (dag-draw-node-order (dag-draw-get-node graph b)) 0)))
            (< order-a order-b)))))

(defun dag-draw--calculate-separation (graph left-node right-node)
  "Calculate minimum separation between two adjacent nodes."
  (let* ((left (dag-draw-get-node graph left-node))
         (right (dag-draw-get-node graph right-node))
         (left-width (dag-draw-node-x-size left))
         (right-width (dag-draw-node-x-size right))
         (node-sep (dag-draw-graph-node-separation graph)))
    
    ;; Minimum separation = (left_width + right_width)/2 + node_separation
    (+ (/ (+ left-width right-width) 2.0) node-sep)))

;;; Simple heuristic approach (fallback)

(defun dag-draw--position-nodes-heuristic (graph)
  "Simple heuristic approach for X-coordinate assignment.
This is a fallback when the auxiliary graph approach is too complex."
  (let ((rank-to-nodes (ht-create))
        (max-rank-width 0))
    
    ;; Group nodes by rank
    (ht-each (lambda (node-id node)
               (let ((rank (or (dag-draw-node-rank node) 0)))
                 (ht-set! rank-to-nodes rank
                          (cons node-id (ht-get rank-to-nodes rank '())))))
             (dag-draw-graph-nodes graph))
    
    ;; Calculate the width needed for each rank and find maximum
    (ht-each (lambda (rank node-list)
               (let ((ordered-nodes (dag-draw--get-ordered-nodes-in-rank graph node-list))
                     (rank-width 0))
                 (dolist (node-id ordered-nodes)
                   (let* ((node (dag-draw-get-node graph node-id))
                          (node-width (dag-draw-node-x-size node))
                          (node-sep (dag-draw-graph-node-separation graph)))
                     (setq rank-width (+ rank-width node-width node-sep))))
                 (setq max-rank-width (max max-rank-width rank-width))))
             rank-to-nodes)
    
    ;; Position nodes within each rank, centering smaller ranks
    (ht-each (lambda (rank node-list)
               (let ((ordered-nodes (dag-draw--get-ordered-nodes-in-rank graph node-list))
                     (rank-width 0))
                 
                 ;; Calculate this rank's total width
                 (dolist (node-id ordered-nodes)
                   (let* ((node (dag-draw-get-node graph node-id))
                          (node-width (dag-draw-node-x-size node))
                          (node-sep (dag-draw-graph-node-separation graph)))
                     (setq rank-width (+ rank-width node-width node-sep))))
                 
                 ;; Center this rank within the maximum width
                 (let ((start-x (/ (- max-rank-width rank-width) 2.0))
                       (current-x 0))
                   (setq current-x start-x)
                   
                   (dolist (node-id ordered-nodes)
                     (let ((node (dag-draw-get-node graph node-id)))
                       ;; Set X coordinate
                       (setf (dag-draw-node-x-coord node) current-x)
                       
                       ;; Update position for next node
                       (let ((node-width (dag-draw-node-x-size node))
                             (node-sep (dag-draw-graph-node-separation graph)))
                         (setq current-x (+ current-x node-width node-sep))))))))
             rank-to-nodes)))

;;; Network simplex solver (simplified)

(defun dag-draw--solve-auxiliary-graph (aux-graph)
  "Solve auxiliary graph using simplified network simplex.
This is a simplified version - a full implementation would be more complex."
  
  ;; For now, use a simple greedy approach
  ;; In a full implementation, this would use proper network simplex
  (dag-draw--position-nodes-heuristic aux-graph)
  
  ;; TODO: Implement full network simplex algorithm
  ;; This would involve:
  ;; 1. Creating initial feasible spanning tree
  ;; 2. Computing cut values
  ;; 3. Finding entering and leaving edges
  ;; 4. Updating the tree until optimal
  
  aux-graph)

(defun dag-draw--extract-coordinates (aux-graph original-graph)
  "Extract X coordinates from auxiliary graph solution back to original graph."
  (ht-each (lambda (node-id node)
             (let ((aux-node (dag-draw-get-node aux-graph node-id)))
               (when aux-node
                 (setf (dag-draw-node-x-coord node)
                       (or (dag-draw-node-x-coord aux-node) 0)))))
           (dag-draw-graph-nodes original-graph)))

;;; Main positioning function

(defun dag-draw-position-nodes (graph)
  "Assign X and Y coordinates to nodes (Pass 3 of GKNV algorithm).
This implements the node positioning pass using auxiliary graph construction
for optimal X-coordinate assignment."
  
  ;; First assign Y coordinates (straightforward)
  (dag-draw--assign-y-coordinates graph)
  
  ;; Then assign X coordinates using auxiliary graph approach
  (let ((use-auxiliary-graph nil))  ; Switch to control approach - use heuristic for now
    
    (if use-auxiliary-graph
        ;; Optimal approach using auxiliary graph
        (let ((aux-graph (dag-draw--create-auxiliary-graph graph)))
          (dag-draw--solve-auxiliary-graph aux-graph)
          (dag-draw--extract-coordinates aux-graph graph))
      
      ;; Simple heuristic approach
      (dag-draw--position-nodes-heuristic graph)))
  
  ;; Ensure all nodes have valid coordinates (fallback for missing coordinates)
  (dag-draw--ensure-all-nodes-have-coordinates graph)
  
  graph)

(defun dag-draw--ensure-all-nodes-have-coordinates (graph)
  "Ensure all nodes have valid X and Y coordinates.
This is a fallback to prevent nil coordinate errors in rendering."
  (let ((default-x 100)
        (default-y 100)
        (x-offset 0)
        (y-offset 0))
    
    (ht-each (lambda (node-id node)
               ;; Assign default coordinates if missing
               (unless (dag-draw-node-x-coord node)
                 (setf (dag-draw-node-x-coord node) (+ default-x x-offset))
                 (setq x-offset (+ x-offset 100)))  ; Space out nodes horizontally
               
               (unless (dag-draw-node-y-coord node)
                 (setf (dag-draw-node-y-coord node) (+ default-y y-offset))
                 (setq y-offset (+ y-offset 60))))  ; Space out nodes vertically
             (dag-draw-graph-nodes graph))))

;;; Coordinate normalization and adjustment

(defun dag-draw-normalize-coordinates (graph)
  "Normalize coordinates to start from (0,0) and be non-negative."
  (let ((min-x most-positive-fixnum)
        (min-y most-positive-fixnum))
    
    ;; Find minimum coordinates
    (ht-each (lambda (node-id node)
               (let ((x (or (dag-draw-node-x-coord node) 0))
                     (y (or (dag-draw-node-y-coord node) 0)))
                 (setq min-x (min min-x x))
                 (setq min-y (min min-y y))))
             (dag-draw-graph-nodes graph))
    
    ;; Adjust all coordinates to be non-negative
    (ht-each (lambda (node-id node)
               (setf (dag-draw-node-x-coord node)
                     (- (or (dag-draw-node-x-coord node) 0) min-x))
               (setf (dag-draw-node-y-coord node)
                     (- (or (dag-draw-node-y-coord node) 0) min-y)))
             (dag-draw-graph-nodes graph)))
  
  graph)

(defun dag-draw-get-graph-bounds (graph)
  "Get bounding box of the positioned graph.
Returns (min-x min-y max-x max-y)."
  ;; Handle empty graphs explicitly
  (if (= (ht-size (dag-draw-graph-nodes graph)) 0)
      '(0 0 100 100)  ; Return default bounds for empty graph
    (let ((min-x most-positive-fixnum)
          (min-y most-positive-fixnum)
          (max-x most-negative-fixnum)
          (max-y most-negative-fixnum))
      
      (ht-each (lambda (node-id node)
                 (let* ((x (or (dag-draw-node-x-coord node) 0))
                        (y (or (dag-draw-node-y-coord node) 0))
                        (width (dag-draw-node-x-size node))
                        (height (dag-draw-node-y-size node))
                        (left (- x (/ width 2.0)))
                        (right (+ x (/ width 2.0)))
                        (top (- y (/ height 2.0)))
                        (bottom (+ y (/ height 2.0))))
                   
                   (setq min-x (min min-x left))
                   (setq max-x (max max-x right))
                   (setq min-y (min min-y top))
                   (setq max-y (max max-y bottom))))
               (dag-draw-graph-nodes graph))
      
      (list min-x min-y max-x max-y))))

(provide 'dag-draw-position)

;;; dag-draw-position.el ends here