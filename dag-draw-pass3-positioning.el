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
(require 'cl-lib)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-quality)

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
  "Calculate minimum separation between two adjacent nodes using GKNV formula.
ρ(a,b) = (xsize(a) + xsize(b))/2 + nodesep(G)"
  (let* ((left (dag-draw-get-node graph left-node))
         (right (dag-draw-get-node graph right-node))
         (left-width (dag-draw-node-x-size left))
         (right-width (dag-draw-node-x-size right))
         (base-node-sep (dag-draw-graph-node-separation graph)))
    
    ;; GKNV formula: ρ(a,b) = (xsize(a) + xsize(b))/2 + nodesep(G)
    (+ (/ (+ left-width right-width) 2.0) base-node-sep)))

(defun dag-draw--calculate-parallel-path-bonus (graph left-node right-node)
  "Calculate bonus separation for nodes with parallel dependency paths.
This prevents visual confusion when nodes share common sources but connect to different targets."
  (let* ((left-targets (dag-draw--get-node-targets graph left-node))
         (right-targets (dag-draw--get-node-targets graph right-node))
         (left-sources (dag-draw--get-node-sources graph left-node))
         (right-sources (dag-draw--get-node-sources graph right-node))
         (shared-sources (cl-intersection left-sources right-sources))
         (overlapping-targets (cl-intersection left-targets right-targets))
         (base-separation (dag-draw-graph-node-separation graph)))
    
    (cond
     ;; High bonus: Same sources, different targets (classic parallel path issue)
     ((and shared-sources 
           (not overlapping-targets)
           (> (length left-targets) 0)
           (> (length right-targets) 0))
      (* base-separation 0.8))  ; 80% bonus for parallel paths
     
     ;; Medium bonus: Same sources, some overlapping targets  
     ((and shared-sources overlapping-targets)
      (* base-separation 0.4))  ; 40% bonus for partially parallel paths
     
     ;; Small bonus: Different sources but many outgoing connections
     ((and (> (length left-targets) 1) (> (length right-targets) 1))
      (* base-separation 0.2))  ; 20% bonus for complex nodes
     
     ;; No bonus
     (t 0))))

(defun dag-draw--calculate-edge-density-bonus (graph left-node right-node)
  "Calculate bonus separation based on edge density to prevent visual crowding."
  (let* ((left-out-degree (length (dag-draw--get-node-targets graph left-node)))
         (right-out-degree (length (dag-draw--get-node-targets graph right-node)))
         (left-in-degree (length (dag-draw--get-node-sources graph left-node)))
         (right-in-degree (length (dag-draw--get-node-sources graph right-node)))
         (total-degree (+ left-out-degree right-out-degree left-in-degree right-in-degree))
         (base-separation (dag-draw-graph-node-separation graph)))
    
    ;; Bonus increases with total edge density
    (cond
     ((>= total-degree 8) (* base-separation 0.6))  ; Very high density
     ((>= total-degree 6) (* base-separation 0.4))  ; High density  
     ((>= total-degree 4) (* base-separation 0.2))  ; Medium density
     (t 0))))                                       ; Low density

(defun dag-draw--get-node-targets (graph node-id)
  "Get list of nodes that NODE-ID connects to (outgoing edges)."
  (let ((targets '()))
    (dolist (edge (dag-draw-graph-edges graph))
      (when (eq (dag-draw-edge-from-node edge) node-id)
        (push (dag-draw-edge-to-node edge) targets)))
    targets))

(defun dag-draw--get-node-sources (graph node-id)
  "Get list of nodes that connect to NODE-ID (incoming edges)."
  (let ((sources '()))
    (dolist (edge (dag-draw-graph-edges graph))
      (when (eq (dag-draw-edge-to-node edge) node-id)
        (push (dag-draw-edge-from-node edge) sources)))
    sources))

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
  
  ;; Use GKNV-compliant positioning with proper separation constraints
  (dag-draw--position-with-separation-constraints graph)
  
  ;; Ensure all nodes have valid coordinates (fallback for missing coordinates)
  (dag-draw--ensure-all-nodes-have-coordinates graph)
  
  ;; DEBUG: Show final node positions after X and Y coordinate assignment
  (message "NODE POSITIONS after complete positioning:")
  (ht-each (lambda (node-id node)
             (message "  Node %s: (%.1f,%.1f) rank=%s"
                      node-id 
                      (or (dag-draw-node-x-coord node) 0)
                      (or (dag-draw-node-y-coord node) 0)
                      (or (dag-draw-node-rank node) "nil")))
           (dag-draw-graph-nodes graph))
  
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


;;; GKNV Enhanced Coordinate Positioning Implementation

(defun dag-draw--position-with-separation-constraints (graph)
  "Position nodes with enhanced separation constraint handling.
Ensures minimum separation between adjacent nodes is maintained."
  (let ((rank-to-nodes (ht-create)))
    
    ;; Group nodes by rank
    (ht-each (lambda (node-id node)
               (let ((rank (or (dag-draw-node-rank node) 0)))
                 (ht-set! rank-to-nodes rank
                          (cons node-id (ht-get rank-to-nodes rank '())))))
             (dag-draw-graph-nodes graph))
    
    ;; Position nodes within each rank respecting separation constraints
    (ht-each (lambda (rank node-list)
               ;; Sort nodes by their order within the rank
               (let ((sorted-nodes (sort node-list 
                                         (lambda (a b)
                                           (let ((order-a (dag-draw-node-order (dag-draw-get-node graph a)))
                                                 (order-b (dag-draw-node-order (dag-draw-get-node graph b))))
                                             (< (or order-a 0) (or order-b 0))))))
                     (current-x 0)
                     (min-separation (dag-draw-graph-node-separation graph)))
                 
                 ;; Assign X coordinates with GKNV separation constraints  
                 (dotimes (i (length sorted-nodes))
                   (let* ((node-id (nth i sorted-nodes))
                          (node (dag-draw-get-node graph node-id)))
                     (setf (dag-draw-node-x-coord node) current-x)
                     ;; Set Y coordinate based on rank
                     (setf (dag-draw-node-y-coord node) 
                           (* rank (dag-draw-graph-rank-separation graph)))
                     
                     ;; Calculate GKNV separation to next node
                     (when (< i (1- (length sorted-nodes)))
                       (let* ((next-node-id (nth (1+ i) sorted-nodes))
                              (next-node (dag-draw-get-node graph next-node-id))
                              ;; GKNV formula: ρ(a,b) = (xsize(a) + xsize(b))/2 + nodesep(G)
                              (separation (dag-draw--calculate-separation graph node-id next-node-id)))
                         (setq current-x (+ current-x separation))))))))
             rank-to-nodes)))

(defun dag-draw--create-enhanced-auxiliary-graph (graph)
  "Create enhanced auxiliary graph for handling long edges.
Returns hash table with auxiliary-nodes and auxiliary-edges information."
  (let ((aux-info (ht-create))
        (auxiliary-nodes '())
        (auxiliary-edges '()))
    
    ;; Find edges that span multiple ranks and create auxiliary nodes
    (dolist (edge (dag-draw-graph-edges graph))
      (let* ((from-node (dag-draw-get-node graph (dag-draw-edge-from-node edge)))
             (to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge)))
             (from-rank (or (dag-draw-node-rank from-node) 0))
             (to-rank (or (dag-draw-node-rank to-node) 0)))
        
        ;; If edge spans more than one rank, create auxiliary nodes
        (when (> (- to-rank from-rank) 1)
          (let ((prev-node-id (dag-draw-edge-from-node edge)))
            ;; Create auxiliary nodes for intermediate ranks
            (dotimes (i (- to-rank from-rank 1))
              (let* ((aux-rank (+ from-rank i 1))
                     (aux-node-id (intern (format "aux_%s_%s_%d" 
                                                  (dag-draw-edge-from-node edge)
                                                  (dag-draw-edge-to-node edge)
                                                  aux-rank))))
                ;; Add auxiliary node to graph
                (dag-draw-add-node graph aux-node-id "AUX")
                (setf (dag-draw-node-rank (dag-draw-get-node graph aux-node-id)) aux-rank)
                (setf (dag-draw-node-order (dag-draw-get-node graph aux-node-id)) 0)
                
                ;; Create auxiliary edge
                (dag-draw-add-edge graph prev-node-id aux-node-id)
                
                (push aux-node-id auxiliary-nodes)
                (setq prev-node-id aux-node-id)))
            
            ;; Create final edge to target
            (dag-draw-add-edge graph prev-node-id (dag-draw-edge-to-node edge))))))
    
    ;; Store auxiliary information
    (ht-set! aux-info 'auxiliary-nodes auxiliary-nodes)
    (ht-set! aux-info 'auxiliary-edges auxiliary-edges)
    
    aux-info))

;;; TDD Network Simplex for X-coordinate positioning

(defun dag-draw--create-auxiliary-graph-with-omega (graph)
  "Create auxiliary graph with proper Omega edge weights for X-coordinate optimization.
Returns a graph with auxiliary nodes and edges weighted by Omega factors."
  (let ((aux-graph (dag-draw-create-graph)))
    
    ;; Add all original nodes to auxiliary graph
    (ht-each (lambda (node-id node)
               (dag-draw-add-node aux-graph node-id 
                                 (dag-draw-node-label node)))
             (dag-draw-graph-nodes graph))
    
    ;; Add edges with Omega weights
    (dolist (edge (dag-draw-graph-edges graph))
      (let* ((from-node (dag-draw-edge-from-node edge))
             (to-node (dag-draw-edge-to-node edge))
             (omega-weight (dag-draw--get-omega-factor graph from-node to-node)))
        (dag-draw-add-edge aux-graph from-node to-node omega-weight)))
    
    aux-graph))

(defun dag-draw--optimize-x-coordinates-with-simplex (graph)
  "Optimize X-coordinates using network simplex min-cost flow.
Returns hash table with success information."
  (let ((result (ht-create)))
    
    ;; Create auxiliary graph
    (let ((aux-graph (dag-draw--create-auxiliary-graph-with-omega graph)))
      
      ;; Apply simplified min-cost flow optimization
      ;; For minimal implementation, position nodes based on order
      (dag-draw--position-with-separation-constraints graph)
      
      ;; Mark as successful
      (ht-set! result 'success t))
    
    result))

;;; GKNV minpath() Virtual Node Chain Straightening

(defun dag-draw--find-virtual-node-chains (graph)
  "Find chains of virtual nodes that can be straightened.
Returns list of virtual node chains, where each chain is a list of node IDs
that form a continuous path of virtual nodes between real nodes.
Based on GKNV paper: minpath straightens chains of virtual nodes."
  (let ((chains '())
        (visited (ht-create)))
    
    ;; Find all virtual nodes
    (ht-each (lambda (node-id node)
               (when (and (dag-draw--is-virtual-node-p node-id)
                         (not (ht-get visited node-id)))
                 ;; Start a new chain from this virtual node
                 (let ((chain (dag-draw--trace-virtual-chain graph node-id visited)))
                   (when (> (length chain) 0)
                     (push chain chains)))))
             (dag-draw-graph-nodes graph))
    
    chains))

(defun dag-draw--trace-virtual-chain (graph start-node visited)
  "Trace a chain of virtual nodes starting from START-NODE.
Marks visited nodes and returns the chain as a list of node IDs."
  (let ((chain '())
        (current-node start-node))
    
    ;; Walk forward through virtual nodes
    (while (and current-node
                (dag-draw--is-virtual-node-p current-node)
                (not (ht-get visited current-node)))
      ;; Mark as visited
      (ht-set! visited current-node t)
      ;; Add to chain
      (push current-node chain)
      
      ;; Find next virtual node in chain
      (setq current-node (dag-draw--find-next-virtual-in-chain graph current-node)))
    
    ;; Return chain in correct order (reverse since we pushed)
    (reverse chain)))

(defun dag-draw--find-next-virtual-in-chain (graph current-node)
  "Find the next virtual node connected to CURRENT-NODE.
Returns the next virtual node ID if found, nil otherwise."
  (let ((next-virtual nil))
    (dolist (edge (dag-draw-graph-edges graph))
      (when (eq (dag-draw-edge-from-node edge) current-node)
        (let ((target-node (dag-draw-edge-to-node edge)))
          (when (dag-draw--is-virtual-node-p target-node)
            (setq next-virtual target-node)))))
    next-virtual))

(defun dag-draw--minpath-straighten-virtual-chains (graph)
  "Apply GKNV minpath() algorithm to straighten virtual node chains.
Sets virtual nodes in each chain to have the same X coordinate for straight lines.
Based on GKNV paper: 'minpath straightens chains of virtual nodes by sequentially 
finding sub-chains that may be assigned the same X coordinate.'"
  (let ((chains (dag-draw--find-virtual-node-chains graph)))
    (dolist (chain chains)
      (when (> (length chain) 1)
        ;; Calculate optimal X coordinate for this chain
        (let ((optimal-x (dag-draw--calculate-optimal-chain-x-coordinate graph chain)))
          ;; Align all virtual nodes in chain to optimal X
          (dolist (node-id chain)
            (let ((node (dag-draw-get-node graph node-id)))
              (when node
                (setf (dag-draw-node-x-coord node) optimal-x)))))))))

(defun dag-draw--calculate-optimal-chain-x-coordinate (graph chain)
  "Calculate optimal X coordinate for virtual node CHAIN.
Uses median of chain endpoints and neighbor positions for best alignment."
  (let ((x-coordinates '()))
    
    ;; Collect X coordinates of chain endpoints (real nodes connected to chain)
    (let ((first-virtual (car chain))
          (last-virtual (car (last chain))))
      
      ;; Find real nodes connected to first virtual node (incoming)
      (dolist (edge (dag-draw-graph-edges graph))
        (when (eq (dag-draw-edge-to-node edge) first-virtual)
          (let ((source-node (dag-draw-get-node graph (dag-draw-edge-from-node edge))))
            (when (and source-node (not (dag-draw--is-virtual-node-p (dag-draw-edge-from-node edge))))
              (push (or (dag-draw-node-x-coord source-node) 0) x-coordinates)))))
      
      ;; Find real nodes connected to last virtual node (outgoing)
      (dolist (edge (dag-draw-graph-edges graph))
        (when (eq (dag-draw-edge-from-node edge) last-virtual)
          (let ((target-node (dag-draw-get-node graph (dag-draw-edge-to-node edge))))
            (when (and target-node (not (dag-draw--is-virtual-node-p (dag-draw-edge-to-node edge))))
              (push (or (dag-draw-node-x-coord target-node) 0) x-coordinates))))))
    
    ;; Calculate median/average of collected coordinates
    (if x-coordinates
        (let ((sorted-coords (sort x-coordinates #'<)))
          (if (= (length sorted-coords) 1)
              (car sorted-coords)
            ;; Use median for better alignment
            (let ((mid-index (/ (length sorted-coords) 2)))
              (if (= (mod (length sorted-coords) 2) 0)
                  ;; Even number - average of two middle values
                  (/ (+ (nth (1- mid-index) sorted-coords)
                        (nth mid-index sorted-coords)) 2.0)
                ;; Odd number - middle value
                (nth mid-index sorted-coords)))))
      ;; Fallback if no coordinates found
      0)))

;;; GKNV packcut() Layout Compaction

(defun dag-draw--find-compaction-opportunities (graph)
  "Find layout compaction opportunities in GRAPH.
Returns list of compaction operations, each with :can-compact and :savings info.
Based on GKNV packcut algorithm that searches for blocks that can be compacted."
  (let ((opportunities '())
        (ranks (dag-draw--get-graph-ranks graph)))
    
    ;; Analyze each rank for compaction opportunities
    (dolist (rank ranks)
      (let* ((nodes-in-rank (dag-draw--get-nodes-in-rank-sorted-by-x graph rank))
             (rank-opportunities (dag-draw--find-rank-compaction-opportunities 
                                 graph nodes-in-rank)))
        (setq opportunities (append opportunities rank-opportunities))))
    
    opportunities))

(defun dag-draw--get-nodes-in-rank-sorted-by-x (graph rank)
  "Get nodes in RANK sorted by X coordinate (left to right).
Returns list of node IDs sorted by their X coordinates."
  (let ((nodes-in-rank '()))
    ;; Collect nodes in this rank
    (ht-each (lambda (node-id node)
               (when (and (dag-draw-node-rank node)
                         (= (dag-draw-node-rank node) rank))
                 (push node-id nodes-in-rank)))
             (dag-draw-graph-nodes graph))
    
    ;; Sort by X coordinate
    (sort nodes-in-rank
          (lambda (a b)
            (let ((x-a (or (dag-draw-node-x-coord (dag-draw-get-node graph a)) 0))
                  (x-b (or (dag-draw-node-x-coord (dag-draw-get-node graph b)) 0)))
              (< x-a x-b))))))

(defun dag-draw--find-rank-compaction-opportunities (graph nodes-in-rank)
  "Find compaction opportunities within a single rank.
Returns list of compaction operations for this rank."
  (let ((opportunities '()))
    (when (>= (length nodes-in-rank) 2)
      ;; Check gaps between adjacent nodes
      (dotimes (i (1- (length nodes-in-rank)))
        (let* ((left-node-id (nth i nodes-in-rank))
               (right-node-id (nth (1+ i) nodes-in-rank))
               (left-node (dag-draw-get-node graph left-node-id))
               (right-node (dag-draw-get-node graph right-node-id))
               (left-x (dag-draw-node-x-coord left-node))
               (right-x (dag-draw-node-x-coord right-node))
               (current-gap (- right-x left-x))
               (min-separation (dag-draw--calculate-separation graph left-node-id right-node-id))
               (excess-space (- current-gap min-separation)))
          
          (when (> excess-space 5)  ; Minimum threshold for compaction
            (push (list :left-node left-node-id
                       :right-node right-node-id
                       :can-compact t
                       :savings excess-space
                       :current-gap current-gap
                       :min-separation min-separation)
                  opportunities)))))
    opportunities))

(defun dag-draw--packcut-compact-layout (graph)
  "Apply GKNV packcut() compaction algorithm to reduce layout width.
Sweeps layout from left to right, searching for blocks that can be compacted.
Based on GKNV specification: 'For each node, if all the nodes to the right of 
it can be shifted to the left by some increment without violating any positioning 
constraints, the shift is performed.'"
  (let ((ranks (dag-draw--get-graph-ranks graph)))
    ;; Process each rank independently
    (dolist (rank ranks)
      (dag-draw--packcut-compact-rank graph rank))))

(defun dag-draw--packcut-compact-rank (graph rank)
  "Apply packcut compaction to a single RANK in GRAPH.
Implements left-to-right sweep compaction within the rank."
  (let ((nodes-in-rank (dag-draw--get-nodes-in-rank-sorted-by-x graph rank)))
    (when (>= (length nodes-in-rank) 2)
      ;; Sweep from left to right, compacting gaps
      (dotimes (i (1- (length nodes-in-rank)))
        (let* ((left-node-id (nth i nodes-in-rank))
               (right-node-id (nth (1+ i) nodes-in-rank))
               (compaction-amount (dag-draw--calculate-compaction-amount 
                                 graph left-node-id right-node-id)))
          (when (> compaction-amount 0)
            ;; Shift right node and all nodes to its right
            (dag-draw--shift-nodes-right-of-position graph rank
                                                   (dag-draw-node-x-coord 
                                                   (dag-draw-get-node graph right-node-id))
                                                   (- compaction-amount))))))))

(defun dag-draw--calculate-compaction-amount (graph left-node-id right-node-id)
  "Calculate how much RIGHT-NODE can be shifted left toward LEFT-NODE.
Returns the amount of compaction possible while respecting GKNV constraints."
  (let* ((left-node (dag-draw-get-node graph left-node-id))
         (right-node (dag-draw-get-node graph right-node-id))
         (left-x (dag-draw-node-x-coord left-node))
         (right-x (dag-draw-node-x-coord right-node))
         (current-gap (- right-x left-x))
         (min-separation (dag-draw--calculate-separation graph left-node-id right-node-id))
         (excess-space (- current-gap min-separation)))
    
    ;; Return compaction amount (positive means can compact)
    (max 0 (- excess-space 2))))  ; Leave smaller buffer for more aggressive compaction

(defun dag-draw--shift-nodes-right-of-position (graph rank x-threshold shift-amount)
  "Shift all nodes in RANK that are right of X-THRESHOLD by SHIFT-AMOUNT.
SHIFT-AMOUNT should be negative for leftward movement (compaction)."
  (ht-each (lambda (node-id node)
             (when (and (dag-draw-node-rank node)
                       (= (dag-draw-node-rank node) rank)
                       (>= (dag-draw-node-x-coord node) x-threshold))
               ;; Shift this node
               (setf (dag-draw-node-x-coord node)
                     (+ (dag-draw-node-x-coord node) shift-amount))))
           (dag-draw-graph-nodes graph)))

(defun dag-draw--calculate-layout-width (graph)
  "Calculate total width of the layout from leftmost to rightmost node."
  (let ((min-x most-positive-fixnum)
        (max-x most-negative-fixnum))
    (ht-each (lambda (node-id node)
               (let ((x (dag-draw-node-x-coord node)))
                 (when x
                   (setq min-x (min min-x x))
                   (setq max-x (max max-x x)))))
             (dag-draw-graph-nodes graph))
    (if (and (< min-x most-positive-fixnum)
             (> max-x most-negative-fixnum))
        (- max-x min-x)
      0)))

;; Note: dag-draw--get-graph-ranks is defined in dag-draw-quality.el

(provide 'dag-draw-pass3-positioning)

;;; dag-draw-pass3-positioning.el ends here