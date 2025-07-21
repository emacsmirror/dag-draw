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



;;; Main positioning function

(defun dag-draw-position-nodes (graph)
  "Assign X and Y coordinates to nodes (Pass 3 of GKNV algorithm).
This implements the node positioning pass using auxiliary graph construction
for optimal X-coordinate assignment."

  ;; First assign Y coordinates (straightforward)
  (dag-draw--assign-y-coordinates graph)

  ;; Use GKNV auxiliary graph method for X-coordinate positioning (Section 4.2)
  (dag-draw--position-with-auxiliary-graph graph)

  ;; Apply GKNV positioning enhancements
  (dag-draw--apply-gknv-positioning-enhancements graph)

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



;;; GKNV Positioning Enhancements Integration

(defun dag-draw--apply-gknv-positioning-enhancements (graph)
  "Apply GKNV positioning enhancements: minpath() and packcut().
Based on GKNV Figure 4-1 algorithm steps 8-9 within the iterative optimization loop."
  (let ((max-iterations 3)  ; GKNV recommends iterative improvement
        (best-layout-width most-positive-fixnum)
        (best-coordinates (ht-create)))

    ;; Store initial coordinates as baseline
    (dag-draw--store-coordinates graph best-coordinates)
    (setq best-layout-width (dag-draw--calculate-layout-width graph))

    ;; Iterative GKNV positioning enhancement
    (dotimes (iteration max-iterations)
      (message "GKNV positioning enhancement iteration %d" (1+ iteration))

      ;; Step 8: minpath() - straighten virtual node chains
      (dag-draw--minpath-straighten-virtual-chains graph)

      ;; Step 9: packcut() - compact layout by removing excess spacing
      (dag-draw--packcut-compact-layout graph)

      ;; Check if this iteration improved the layout
      (let ((current-width (dag-draw--calculate-layout-width graph)))
        (when (< current-width best-layout-width)
          (message "GKNV enhancement: Improved layout width from %s to %s"
                   best-layout-width current-width)
          (setq best-layout-width current-width)
          (dag-draw--store-coordinates graph best-coordinates))))

    ;; Restore best coordinates found
    (dag-draw--restore-coordinates graph best-coordinates)
    (message "GKNV positioning enhancements completed. Final width: %s" best-layout-width)))

(defun dag-draw--store-coordinates (graph coordinate-store)
  "Store current node coordinates in COORDINATE-STORE hash table."
  (ht-clear! coordinate-store)
  (ht-each (lambda (node-id node)
             (ht-set! coordinate-store node-id
                      (cons (dag-draw-node-x-coord node)
                            (dag-draw-node-y-coord node))))
           (dag-draw-graph-nodes graph)))

(defun dag-draw--restore-coordinates (graph coordinate-store)
  "Restore node coordinates from COORDINATE-STORE hash table."
  (ht-each (lambda (node-id coords)
             (let ((node (dag-draw-get-node graph node-id)))
               (when node
                 (setf (dag-draw-node-x-coord node) (car coords))
                 (setf (dag-draw-node-y-coord node) (cdr coords)))))
           coordinate-store))

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
      (when (> (length chain) 0)  ; Handle both single nodes and multi-node chains
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

;;; GKNV Section 4.2: Network Simplex Constraint Solver

(defun dag-draw--build-constraint-auxiliary-graph (graph)
  "Build auxiliary graph for GKNV Section 4.2 network simplex constraint solver.
Per GKNV specification: 'Every edge e=(u,v) in G is replaced by two edges
(n_e,u) and (n_e,v) with δ=0 and ω=ω(e)×Ω(e). If v is the left neighbor
of w, then G' has an edge f=(v,w) with δ(f)=ρ(v,w) and ω(f)=0.'"
  (let ((aux-graph (dag-draw-create-graph)))

    ;; Copy all original nodes to auxiliary graph
    (ht-each (lambda (node-id node)
               (dag-draw-add-node aux-graph node-id (dag-draw-node-label node))
               ;; Copy node properties
               (let ((aux-node (dag-draw-get-node aux-graph node-id)))
                 (setf (dag-draw-node-rank aux-node) (dag-draw-node-rank node))
                 (setf (dag-draw-node-x-coord aux-node) (dag-draw-node-x-coord node))
                 (setf (dag-draw-node-y-coord aux-node) (dag-draw-node-y-coord node))))
             (dag-draw-graph-nodes graph))

    ;; Create auxiliary nodes and cost edges for each original edge
    (dolist (edge (dag-draw-graph-edges graph))
      (let* ((from-node (dag-draw-edge-from-node edge))
             (to-node (dag-draw-edge-to-node edge))
             (edge-weight (or (dag-draw-edge-weight edge) 1))
             (aux-node-id (intern (format "aux_edge_%s_%s" from-node to-node)))
             (omega-factor (dag-draw--calculate-omega-factor from-node to-node))
             (cost-weight (* edge-weight omega-factor)))

        ;; Create auxiliary node for this edge
        (dag-draw-add-node aux-graph aux-node-id (format "Aux_%s_%s" from-node to-node))

        ;; Create cost edges: (aux_node, from) and (aux_node, to)
        ;; With δ=0 and ω=ω(e)×Ω(e) per GKNV specification
        (let ((attrs1 (ht-create))
              (attrs2 (ht-create)))
          (ht-set attrs1 'min-length 0)  ; δ=0 for cost edges
          (ht-set attrs2 'min-length 0)  ; δ=0 for cost edges
          (dag-draw-add-edge aux-graph aux-node-id from-node cost-weight nil attrs1)
          (dag-draw-add-edge aux-graph aux-node-id to-node cost-weight nil attrs2))))

    ;; Create separation edges for adjacent nodes in same rank
    (ht-each (lambda (rank nodes-in-rank)
               (when (> (length nodes-in-rank) 1)
                 ;; Sort nodes by X coordinate to establish left-right order
                 (let ((sorted-nodes (sort (copy-sequence nodes-in-rank)
                                           (lambda (a b)
                                             (< (or (dag-draw-node-x-coord
                                                     (dag-draw-get-node aux-graph a)) 0)
                                                (or (dag-draw-node-x-coord
                                                     (dag-draw-get-node aux-graph b)) 0))))))
                   ;; Create separation edges between adjacent nodes
                   (dotimes (i (1- (length sorted-nodes)))
                     (let* ((left-node (nth i sorted-nodes))
                            (right-node (nth (1+ i) sorted-nodes))
                            (separation (dag-draw--calculate-separation aux-graph left-node right-node)))
                       ;; Create separation edge: (left, right) with δ=ρ(left,right), ω=0
                       (let ((sep-attrs (ht-create)))
                         (ht-set sep-attrs 'min-length separation)  ; δ=ρ(left,right) for separation
                         (dag-draw-add-edge aux-graph left-node right-node 0 nil sep-attrs)))))))
             (dag-draw--group-nodes-by-rank aux-graph))

    aux-graph))

(defun dag-draw--calculate-omega-factor (from-node to-node)
  "Calculate Ω factor for edge cost per GKNV specification.
Edges between virtual nodes get higher weights to favor straightening."
  (let ((from-virtual (dag-draw--is-virtual-node-p from-node))
        (to-virtual (dag-draw--is-virtual-node-p to-node)))
    (cond
     ;; Both virtual nodes - highest priority for straightening
     ((and from-virtual to-virtual) 8)
     ;; One virtual, one real - medium priority
     ((or from-virtual to-virtual) 2)
     ;; Both real nodes - lowest priority (already straight)
     (t 1))))

(defun dag-draw--calculate-separation (graph left-node right-node)
  "Calculate minimum separation ρ(left,right) between adjacent nodes.
Based on GKNV: ρ(a,b) = (xsize(a) + xsize(b))/2 + nodesep(G)"
  (let* ((left (dag-draw-get-node graph left-node))
         (right (dag-draw-get-node graph right-node))
         (left-width (if left (dag-draw-node-x-size left) 60))    ; Use actual node width
         (right-width (if right (dag-draw-node-x-size right) 60)) ; Use actual node width
         (nodesep (or (dag-draw-graph-node-separation graph) 2)))
    (+ (/ (+ left-width right-width) 2.0) nodesep)))

(defun dag-draw--group-nodes-by-rank (graph)
  "Group nodes by their rank for separation edge creation."
  (let ((rank-groups (ht-create)))
    (ht-each (lambda (node-id node)
               (let ((rank (or (dag-draw-node-rank node) 0)))
                 (ht-set rank-groups rank
                         (cons node-id (or (ht-get rank-groups rank) '())))))
             (dag-draw-graph-nodes graph))
    rank-groups))


(defun dag-draw--network-simplex-x-coordinates (aux-graph)
  "Apply network simplex algorithm to auxiliary graph for X coordinate optimization.
Reuses existing network simplex infrastructure but optimizes X coordinates instead of ranks."
  ;; For now, use a simplified approach that respects separation constraints
  ;; TODO: Full network simplex implementation for X coordinates
  (dag-draw--simple-x-coordinate-solver aux-graph))

(defun dag-draw--simple-x-coordinate-solver (aux-graph)
  "Simplified constraint solver that minimizes GKNV cost function.
Implements iterative cost minimization with separation constraint compliance."
  ;; Initialize all nodes to reasonable starting positions
  (dag-draw--initialize-x-coordinates aux-graph)
  
  ;; Multiple iterations to converge on optimal positioning
  (dotimes (iteration 10)  ; Increased iterations for better convergence
    (dag-draw--gknv-cost-minimization-iteration aux-graph))

  ;; Final cleanup to ensure separation constraints
  (dag-draw--enforce-separation-constraints aux-graph))

(defun dag-draw--initialize-x-coordinates (aux-graph)
  "Initialize X coordinates for all nodes in auxiliary graph."
  (let ((current-x 0)
        (node-spacing 100))  ; Default spacing
    (ht-each (lambda (node-id node)
               (unless (dag-draw-node-x-coord node)
                 (setf (dag-draw-node-x-coord node) current-x)
                 (setq current-x (+ current-x node-spacing))))
             (dag-draw-graph-nodes aux-graph))))

(defun dag-draw--gknv-cost-minimization-iteration (aux-graph)
  "One iteration of GKNV cost minimization across all nodes.
Updates each node's X coordinate to minimize the GKNV cost function."
  ;; Update each node's position based on cost minimization
  (ht-each (lambda (node-id node)
             (let ((new-x (dag-draw--calculate-cost-minimizing-position aux-graph node-id)))
               (setf (dag-draw-node-x-coord node) new-x)))
           (dag-draw-graph-nodes aux-graph)))


(defun dag-draw--calculate-cost-minimizing-position (aux-graph node-id)
  "Calculate X position that minimizes GKNV cost function for NODE-ID.
Per GKNV Section 4.2: minimize Σ Ω(e)×ω(e)×|x_w - x_v| for L1 norm optimization.
The optimal solution is the weighted median of connected node positions."
  (let ((weighted-positions '()))

    ;; Collect all cost edges (weight > 0) connected to this node
    (dolist (edge (dag-draw-graph-edges aux-graph))
      (let ((weight (dag-draw-edge-weight edge)))
        (when (> weight 0)  ; Only cost edges, not separation edges (weight=0)
          (cond
           ;; Node is source of edge - minimize distance to target
           ((eq (dag-draw-edge-from-node edge) node-id)
            (let* ((target-id (dag-draw-edge-to-node edge))
                   (target-node (dag-draw-get-node aux-graph target-id))
                   (target-x (or (dag-draw-node-x-coord target-node) 0)))
              (push (cons target-x weight) weighted-positions)))
           ;; Node is target of edge - minimize distance to source  
           ((eq (dag-draw-edge-to-node edge) node-id)
            (let* ((source-id (dag-draw-edge-from-node edge))
                   (source-node (dag-draw-get-node aux-graph source-id))
                   (source-x (or (dag-draw-node-x-coord source-node) 0)))
              (push (cons source-x weight) weighted-positions)))))))

    ;; Calculate weighted median for L1 norm minimization
    (if weighted-positions
        (dag-draw--calculate-cost-weighted-median weighted-positions)
      0)))

(defun dag-draw--calculate-cost-weighted-median (weighted-positions)
  "Calculate weighted median from list of (position . weight) pairs.
This minimizes the L1 norm cost function Σ weight×|x - position|."
  (when weighted-positions
    ;; Sort positions by X coordinate
    (let* ((sorted-positions (sort (copy-sequence weighted-positions)
                                   (lambda (a b) (< (car a) (car b)))))
           (total-weight (apply #'+ (mapcar #'cdr sorted-positions)))
           (half-weight (/ total-weight 2.0))
           (cumulative-weight 0))
      
      ;; Find the weighted median
      (catch 'found
        (dolist (pos-weight sorted-positions)
          (let ((position (car pos-weight))
                (weight (cdr pos-weight)))
            (setq cumulative-weight (+ cumulative-weight weight))
            (when (>= cumulative-weight half-weight)
              (throw 'found position))))
        ;; Fallback: return first position if no median found
        (car (car sorted-positions))))))

(defun dag-draw--enforce-separation-constraints (aux-graph)
  "Final pass to ensure all separation constraints are met."
  (let ((rank-groups (dag-draw--group-nodes-by-rank aux-graph)))

    (ht-each (lambda (rank nodes-in-rank)
               (when (> (length nodes-in-rank) 1)
                 ;; Sort nodes by current X position
                 (let ((sorted-nodes (sort (copy-sequence nodes-in-rank)
                                           (lambda (a b)
                                             (< (dag-draw-node-x-coord (dag-draw-get-node aux-graph a))
                                                (dag-draw-node-x-coord (dag-draw-get-node aux-graph b)))))))

                   ;; Adjust positions to meet separation constraints
                   (let ((current-x 0))
                     (dolist (node-id sorted-nodes)
                       (let ((node (dag-draw-get-node aux-graph node-id)))
                         (when node
                           (let ((required-x (max current-x (dag-draw-node-x-coord node))))
                             (setf (dag-draw-node-x-coord node) required-x)
                             (setq current-x (+ required-x (dag-draw--calculate-node-spacing aux-graph node-id)))))))))))
             rank-groups)))


(defun dag-draw--calculate-preferred-x-position (aux-graph node-id)
  "Calculate preferred X position for NODE-ID based on GKNV cost minimization.
According to GKNV paper: minimize Σ Ω(e)×ω(e)×|x_w - x_v| for all edges."
  (let ((cost-edges '())
        (total-weight 0))

    ;; Find cost edges (weight > 0) connected to this node
    (dolist (edge (dag-draw-graph-edges aux-graph))
      (let ((weight (dag-draw-edge-weight edge)))
        (when (and (> weight 0)  ; Only cost edges, not separation edges
                   (or (eq (dag-draw-edge-from-node edge) node-id)
                       (eq (dag-draw-edge-to-node edge) node-id)))
          (let ((other-node-id (if (eq (dag-draw-edge-from-node edge) node-id)
                                   (dag-draw-edge-to-node edge)
                                 (dag-draw-edge-from-node edge))))
            (push (cons other-node-id weight) cost-edges)
            (setq total-weight (+ total-weight weight))))))

    ;; Calculate weighted average position based on GKNV cost function
    ;; Higher weights mean stronger attraction (nodes should be closer)
    (if (> total-weight 0)
        (let ((weighted-sum 0))
          (dolist (cost-edge cost-edges)
            (let* ((other-node-id (car cost-edge))
                   (edge-weight (cdr cost-edge))
                   (other-node (dag-draw-get-node aux-graph other-node-id))
                   (other-x (or (dag-draw-node-x-coord other-node) 0)))
              ;; Weight by edge importance - higher weight = stronger pull
              (setq weighted-sum (+ weighted-sum (* edge-weight other-x)))))
          (/ weighted-sum total-weight))
      0)))

(defun dag-draw--calculate-node-spacing (graph node-id)
  "Calculate spacing needed after NODE-ID for next node."
  (let* ((node (dag-draw-get-node graph node-id))
         (node-width (if node (dag-draw-node-x-size node) 60))
         (nodesep (or (dag-draw-graph-node-separation graph) 2)))
    (+ node-width nodesep)))

;;; Helper functions for TDD tests




(defun dag-draw--get-nodes-in-rank (graph rank)
  "Get all nodes in specified RANK."
  (let ((nodes-in-rank '()))
    (ht-each (lambda (node-id node)
               (when (= (or (dag-draw-node-rank node) 0) rank)
                 (push node-id nodes-in-rank)))
             (dag-draw-graph-nodes graph))
    nodes-in-rank))

(defun dag-draw--position-with-auxiliary-graph (graph)
  "Position nodes using GKNV auxiliary graph method (Section 4.2).
Creates constraint auxiliary graph and applies network simplex for optimal X-coordinates."
  (let ((aux-graph (dag-draw--build-constraint-auxiliary-graph graph)))
    ;; Apply network simplex to auxiliary graph for X-coordinate optimization
    (dag-draw--network-simplex-x-coordinates aux-graph)
    
    ;; Extract X-coordinates from auxiliary graph back to original graph
    (dag-draw--extract-x-coordinates-from-auxiliary aux-graph graph)))

(defun dag-draw--extract-x-coordinates-from-auxiliary (aux-graph original-graph)
  "Extract X-coordinates from auxiliary graph back to original graph.
Only copies coordinates for original nodes, ignoring auxiliary edge nodes."
  (ht-each (lambda (node-id node)
             ;; Only copy coordinates for original nodes (not auxiliary edge nodes)
             (unless (string-match "^aux_edge_" (symbol-name node-id))
               (let ((original-node (dag-draw-get-node original-graph node-id)))
                 (when original-node
                   (setf (dag-draw-node-x-coord original-node) 
                         (dag-draw-node-x-coord node))))))
           (dag-draw-graph-nodes aux-graph)))

(provide 'dag-draw-pass3-positioning)

;;; dag-draw-pass3-positioning.el ends here
