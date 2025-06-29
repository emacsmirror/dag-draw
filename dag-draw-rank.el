;;; dag-draw-rank.el --- Rank assignment for dag-draw -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Implementation of the rank assignment pass of the GKNV algorithm.
;; This module assigns nodes to discrete ranks using a simplified version
;; of the network simplex approach described in the paper.

;;; Code:

(require 'dash)
(require 'ht)
(require 'dag-draw)
(require 'dag-draw-core)

;;; Simple Cycle Breaking

(defun dag-draw--simple-has-cycle (graph visited rec-stack node)
  "Check if graph has cycle starting from NODE using DFS.
VISITED tracks visited nodes, REC-STACK tracks recursion stack."
  (ht-set! visited node t)
  (ht-set! rec-stack node t)
  
  (let ((has-cycle nil))
    (dolist (successor (dag-draw-get-successors graph node))
      (cond
       ;; Not visited yet - recurse
       ((not (ht-get visited successor))
        (when (dag-draw--simple-has-cycle graph visited rec-stack successor)
          (setq has-cycle t)))
       ;; In recursion stack - cycle found
       ((ht-get rec-stack successor)
        (setq has-cycle t))))
    
    (ht-set! rec-stack node nil)
    has-cycle))

(defun dag-draw-simple-has-cycles (graph)
  "Simple cycle detection using DFS."
  (let ((visited (ht-create))
        (rec-stack (ht-create))
        (has-cycle nil))
    
    ;; Initialize tracking tables
    (dolist (node-id (dag-draw-get-node-ids graph))
      (ht-set! visited node-id nil)
      (ht-set! rec-stack node-id nil))
    
    ;; Check from each unvisited node
    (dolist (node-id (dag-draw-get-node-ids graph))
      (when (and (not (ht-get visited node-id))
                 (not has-cycle))
        (setq has-cycle (dag-draw--simple-has-cycle graph visited rec-stack node-id))))
    
    has-cycle))

(defun dag-draw-simple-break-cycles (graph)
  "Simple cycle breaking by removing arbitrary back edges.
Returns a new graph with cycles broken."
  (let ((acyclic (dag-draw-copy-graph graph)))
    
    ;; Keep removing edges until no cycles remain
    (while (dag-draw-simple-has-cycles acyclic)
      (let ((edges (dag-draw-graph-edges acyclic))
            (edge-removed nil))
        ;; Try removing each edge until we find one that breaks a cycle
        (dolist (edge edges)
          (unless edge-removed
            (let ((from (dag-draw-edge-from-node edge))
                  (to (dag-draw-edge-to-node edge)))
              ;; Remove edge temporarily
              (dag-draw-remove-edge acyclic from to)
              ;; If this breaks cycles, we're done with this iteration
              (if (not (dag-draw-simple-has-cycles acyclic))
                  (setq edge-removed t)
                ;; Otherwise, add it back and try next edge
                (dag-draw-add-edge acyclic from to 
                                   (dag-draw-edge-weight edge)
                                   (dag-draw-edge-label edge)
                                   (dag-draw-edge-attributes edge))))))))
    
    acyclic))

;;; Simple Rank Assignment

(defun dag-draw-assign-ranks (graph)
  "Assign ranks to nodes in GRAPH using a simplified algorithm.
This is the first pass of the GKNV algorithm."
  ;; First, break any cycles to make the graph acyclic
  (let ((acyclic (if (dag-draw-simple-has-cycles graph)
                     (dag-draw-simple-break-cycles graph)
                   graph)))
    
    ;; Copy the acyclic structure back to the original graph
    (unless (eq acyclic graph)
      (setf (dag-draw-graph-edges graph) (dag-draw-graph-edges acyclic)))
    
    ;; Now assign ranks using topological ordering
    (dag-draw--assign-ranks-topological graph)
    
    graph))

(defun dag-draw--assign-ranks-topological (graph)
  "Assign ranks using a simple topological approach."
  (let ((in-degree (ht-create))
        (queue '())
        (current-rank 0))
    
    ;; Calculate in-degrees
    (dolist (node-id (dag-draw-get-node-ids graph))
      (ht-set! in-degree node-id 0))
    
    (dolist (edge (dag-draw-graph-edges graph))
      (let ((to-node (dag-draw-edge-to-node edge)))
        (ht-set! in-degree to-node (1+ (ht-get in-degree to-node 0)))))
    
    ;; Find nodes with in-degree 0
    (dolist (node-id (dag-draw-get-node-ids graph))
      (when (zerop (ht-get in-degree node-id))
        (push node-id queue)))
    
    ;; Process nodes level by level
    (while queue
      (let ((current-level queue))
        (setq queue '())
        
        ;; Assign current rank to all nodes in this level
        (dolist (node-id current-level)
          (let ((node (dag-draw-get-node graph node-id)))
            (setf (dag-draw-node-rank node) current-rank)))
        
        ;; Update in-degrees and find next level
        (dolist (node-id current-level)
          (dolist (successor (dag-draw-get-successors graph node-id))
            (ht-set! in-degree successor (1- (ht-get in-degree successor)))
            (when (zerop (ht-get in-degree successor))
              (push successor queue))))
        
        (setq current-rank (1+ current-rank))))
    
    ;; Set max rank in graph
    (setf (dag-draw-graph-max-rank graph) (1- current-rank))
    
    graph))

;;; Rank Adjustment and Balancing

(defun dag-draw-normalize-ranks (graph)
  "Normalize ranks so the minimum rank is 0."
  (let ((min-rank most-positive-fixnum))
    
    ;; Find minimum rank
    (ht-each (lambda (node-id node)
               (when (dag-draw-node-rank node)
                 (setq min-rank (min min-rank (dag-draw-node-rank node)))))
             (dag-draw-graph-nodes graph))
    
    ;; Adjust all ranks
    (when (< min-rank most-positive-fixnum)
      (ht-each (lambda (node-id node)
                 (when (dag-draw-node-rank node)
                   (setf (dag-draw-node-rank node) 
                         (- (dag-draw-node-rank node) min-rank))))
               (dag-draw-graph-nodes graph))
      
      ;; Update max rank
      (when (dag-draw-graph-max-rank graph)
        (setf (dag-draw-graph-max-rank graph)
              (- (dag-draw-graph-max-rank graph) min-rank))))
    
    graph))

(defun dag-draw-balance-ranks (graph)
  "Balance rank assignment to improve layout quality.
This is a simplified version of the balancing described in the paper."
  (when (dag-draw-graph-max-rank graph)
    (let ((rank-counts (make-vector (1+ (dag-draw-graph-max-rank graph)) 0)))
      
      ;; Count nodes per rank
      (ht-each (lambda (node-id node)
                 (when (dag-draw-node-rank node)
                   (let ((rank (dag-draw-node-rank node)))
                     (aset rank-counts rank (1+ (aref rank-counts rank))))))
               (dag-draw-graph-nodes graph))
      
      ;; Try to move nodes from crowded ranks to less crowded ones
      ;; (This is a simplified heuristic)
      (ht-each (lambda (node-id node)
                 (when (dag-draw-node-rank node)
                   (dag-draw--try-balance-node graph node rank-counts)))
               (dag-draw-graph-nodes graph))))
  
  graph)

(defun dag-draw--try-balance-node (graph node rank-counts)
  "Try to balance a single node's rank assignment."
  (let ((current-rank (dag-draw-node-rank node))
        (node-id (dag-draw-node-id node)))
    
    ;; Check if node can be moved to adjacent ranks
    (dolist (delta '(-1 1))
      (let ((new-rank (+ current-rank delta)))
        (when (and (>= new-rank 0)
                   (<= new-rank (dag-draw-graph-max-rank graph))
                   (< (aref rank-counts new-rank) 
                      (aref rank-counts current-rank)))
          
          ;; Check if move preserves edge constraints
          (when (dag-draw--rank-move-valid-p graph node-id new-rank)
            ;; Update rank counts
            (aset rank-counts current-rank (1- (aref rank-counts current-rank)))
            (aset rank-counts new-rank (1+ (aref rank-counts new-rank)))
            ;; Move the node
            (setf (dag-draw-node-rank node) new-rank)))))))

(defun dag-draw--rank-move-valid-p (graph node-id new-rank)
  "Check if moving NODE-ID to NEW-RANK preserves edge direction constraints."
  (let ((valid t))
    
    ;; Check all incoming edges
    (dolist (predecessor (dag-draw-get-predecessors graph node-id))
      (let ((pred-node (dag-draw-get-node graph predecessor)))
        (when (and (dag-draw-node-rank pred-node)
                   (>= (dag-draw-node-rank pred-node) new-rank))
          (setq valid nil))))
    
    ;; Check all outgoing edges
    (dolist (successor (dag-draw-get-successors graph node-id))
      (let ((succ-node (dag-draw-get-node graph successor)))
        (when (and (dag-draw-node-rank succ-node)
                   (<= (dag-draw-node-rank succ-node) new-rank))
          (setq valid nil))))
    
    valid))

;;; GKNV Network Simplex Implementation

(defun dag-draw--construct-feasible-tree (graph)
  "Construct initial feasible spanning tree with auxiliary nodes.
Returns hash table with tree-edges, non-tree-edges, aux-source, and aux-sink."
  (let ((tree-info (ht-create))
        (aux-source-id 'aux-source)
        (aux-sink-id 'aux-sink))
    
    ;; Add auxiliary source and sink nodes to graph temporarily
    (dag-draw-add-node graph aux-source-id "AUX-SOURCE")
    (dag-draw-add-node graph aux-sink-id "AUX-SINK")
    
    ;; Create auxiliary edges to make spanning tree
    (let ((tree-edges '())
          (non-tree-edges (copy-sequence (dag-draw-graph-edges graph))))
      
      ;; Connect aux-source to all source nodes (nodes with no incoming edges)
      (dolist (node-id (dag-draw-get-node-ids graph))
        (when (and (not (eq node-id aux-source-id))
                   (not (eq node-id aux-sink-id))
                   (null (dag-draw-get-predecessors graph node-id)))
          (let ((aux-edge (dag-draw-add-edge graph aux-source-id node-id)))
            (push aux-edge tree-edges))))
      
      ;; Connect all sink nodes to aux-sink
      (dolist (node-id (dag-draw-get-node-ids graph))
        (when (and (not (eq node-id aux-source-id))
                   (not (eq node-id aux-sink-id))
                   (null (dag-draw-get-successors graph node-id)))
          (let ((aux-edge (dag-draw-add-edge graph node-id aux-sink-id)))
            (push aux-edge tree-edges))))
      
      ;; Store results
      (ht-set! tree-info 'tree-edges tree-edges)
      (ht-set! tree-info 'non-tree-edges non-tree-edges)
      (ht-set! tree-info 'aux-source aux-source-id)
      (ht-set! tree-info 'aux-sink aux-sink-id))
    
    tree-info))

(defun dag-draw--compute-cut-values (graph tree-info)
  "Compute cut values for all tree edges.
Returns hash table mapping tree edges to their cut values."
  (let ((cut-values (ht-create))
        (tree-edges (ht-get tree-info 'tree-edges)))
    
    ;; For each tree edge, compute its cut value
    ;; (This is a simplified implementation - the full algorithm is more complex)
    (dolist (edge tree-edges)
      (let ((from-node (dag-draw-edge-from-node edge))
            (to-node (dag-draw-edge-to-node edge)))
        ;; Simple cut value calculation (weight of edge)
        (ht-set! cut-values edge (dag-draw-edge-weight edge))))
    
    cut-values))

;;; Public Interface

(defun dag-draw-rank-graph (graph)
  "Complete rank assignment process for GRAPH.
This includes cycle breaking, rank assignment, normalization, and balancing."
  (dag-draw-assign-ranks graph)
  (dag-draw-normalize-ranks graph)
  (dag-draw-balance-ranks graph)
  graph)

(provide 'dag-draw-rank)

;;; dag-draw-rank.el ends here