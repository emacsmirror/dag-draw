;;; dag-draw-quality.el --- Dynamic graph quality analysis for dag-draw -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This module provides dynamic analysis of graph structure to calculate optimal
;; rank separation for hollow routing. It implements the GKNV paper's recommendation
;; to "optionally increase separation between adjacent ranks to improve the slope
;; of nearly horizontal edges to make them more readable."
;;
;; The analysis considers:
;; - Edge convergence patterns (multiple edges to same destination)
;; - Edge density between ranks
;; - Horizontal routing distances
;; - ASCII grid constraints

;;; Code:

(require 'dash)
(require 'ht)
(require 'dag-draw-core)

;;; Edge Analysis Functions

(defun dag-draw--count-edges-between-ranks (graph rank1 rank2)
  "Count edges between RANK1 and RANK2 in GRAPH.
Returns the number of edges that cross from nodes in RANK1 to nodes in RANK2."
  (let ((edge-count 0))
    (dolist (edge (dag-draw-graph-edges graph))
      (let* ((from-node (dag-draw-get-node graph (dag-draw-edge-from-node edge)))
             (to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge)))
             (from-rank (when from-node (dag-draw-node-rank from-node)))
             (to-rank (when to-node (dag-draw-node-rank to-node))))
        (when (and from-rank to-rank
                   (= from-rank rank1)
                   (= to-rank rank2))
          (setq edge-count (1+ edge-count)))))
    edge-count))

(defun dag-draw--max-edges-to-same-destination (graph from-rank to-rank)
  "Find maximum number of edges converging on any single destination node.
Analyzes edges from FROM-RANK to TO-RANK and returns the highest convergence count."
  (let ((destination-counts (ht-create)))
    
    ;; Count edges to each destination node
    (dolist (edge (dag-draw-graph-edges graph))
      (let* ((from-node (dag-draw-get-node graph (dag-draw-edge-from-node edge)))
             (to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge)))
             (from-node-rank (when from-node (dag-draw-node-rank from-node)))
             (to-node-rank (when to-node (dag-draw-node-rank to-node))))
        (when (and from-node-rank to-node-rank
                   (= from-node-rank from-rank)
                   (= to-node-rank to-rank))
          (let ((dest-id (dag-draw-edge-to-node edge)))
            (ht-set! destination-counts dest-id 
                     (1+ (or (ht-get destination-counts dest-id) 0)))))))
    
    ;; Find maximum convergence
    (let ((max-convergence 0))
      (ht-each (lambda (dest-id count)
                 (setq max-convergence (max max-convergence count)))
               destination-counts)
      max-convergence)))

(defun dag-draw--max-horizontal-edge-distance (graph from-rank to-rank)
  "Calculate maximum horizontal distance for edges between FROM-RANK and TO-RANK.
Returns the maximum horizontal distance any edge needs to travel, which affects
routing complexity and space requirements."
  (let ((max-distance 0))
    (dolist (edge (dag-draw-graph-edges graph))
      (let* ((from-node (dag-draw-get-node graph (dag-draw-edge-from-node edge)))
             (to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge)))
             (from-node-rank (when from-node (dag-draw-node-rank from-node)))
             (to-node-rank (when to-node (dag-draw-node-rank to-node))))
        (when (and from-node-rank to-node-rank
                   (= from-node-rank from-rank)
                   (= to-node-rank to-rank))
          (let* ((from-x (or (dag-draw-node-x-coord from-node) 0))
                 (to-x (or (dag-draw-node-x-coord to-node) 0))
                 (horizontal-distance (abs (- to-x from-x))))
            (setq max-distance (max max-distance horizontal-distance))))))
    max-distance))

;;; Rank Analysis Functions

(defun dag-draw--get-graph-ranks (graph)
  "Get list of all ranks present in GRAPH.
Returns sorted list of rank numbers."
  (let ((ranks '()))
    (ht-each (lambda (node-id node)
               (let ((rank (dag-draw-node-rank node)))
                 (when (and rank (not (member rank ranks)))
                   (push rank ranks))))
             (dag-draw-graph-nodes graph))
    (sort ranks #'<)))

(defun dag-draw--get-nodes-in-rank (graph rank)
  "Get list of all nodes in RANK of GRAPH.
Returns list of node objects."
  (let ((nodes-in-rank '()))
    (ht-each (lambda (node-id node)
               (when (and (dag-draw-node-rank node)
                          (= (dag-draw-node-rank node) rank))
                 (push node nodes-in-rank)))
             (dag-draw-graph-nodes graph))
    nodes-in-rank))

;;; Dynamic Spacing Calculation

(defun dag-draw--calculate-dynamic-rank-separation (graph from-rank to-rank)
  "Calculate required ASCII rows between FROM-RANK and TO-RANK based on edge analysis.
Implements dynamic spacing following GKNV paper recommendation to increase separation
for better edge readability."
  (let* ((edges-between (dag-draw--count-edges-between-ranks graph from-rank to-rank))
         (max-convergence (dag-draw--max-edges-to-same-destination graph from-rank to-rank))
         (max-horizontal-distance (dag-draw--max-horizontal-edge-distance graph from-rank to-rank))
         
         ;; Base spacing: standard GKNV minimum per paper specification
         (base-spacing 2)
         
         ;; Additional spacing for convergence: +1 row for each additional converging edge
         (convergence-spacing (max 0 (1- max-convergence)))
         
         ;; Additional spacing for edge density: +1 row for every 3 edges crossing between ranks
         (density-spacing (/ edges-between 3))
         
         ;; Additional spacing for long horizontal routes: +1 row if routes are very long
         (distance-spacing (if (> max-horizontal-distance 300) 1 0)))
    
    (+ base-spacing convergence-spacing density-spacing distance-spacing)))

(defun dag-draw--calculate-max-required-rank-separation (graph)
  "Calculate maximum rank separation needed for any pair of adjacent ranks in GRAPH.
This ensures sufficient space for the most complex rank transition."
  (let ((ranks (dag-draw--get-graph-ranks graph)))
    (if (< (length ranks) 2)
        ;; Default spacing for graphs with 0 or 1 ranks
        2
      ;; Calculate spacing for each adjacent rank pair and take maximum
      (let ((max-spacing 2))
        (dotimes (i (1- (length ranks)))
          (let* ((from-rank (nth i ranks))
                 (to-rank (nth (1+ i) ranks))
                 (required-spacing (dag-draw--calculate-dynamic-rank-separation 
                                   graph from-rank to-rank)))
            (setq max-spacing (max max-spacing required-spacing))))
        max-spacing))))

;;; Graph Structure Analysis

(defun dag-draw--analyze-graph-complexity (graph)
  "Analyze graph complexity and return summary statistics.
Returns a plist with complexity metrics for debugging and optimization."
  (let* ((ranks (dag-draw--get-graph-ranks graph))
         (total-nodes (dag-draw-node-count graph))
         (total-edges (dag-draw-edge-count graph))
         (max-convergence 0)
         (total-rank-transitions 0))
    
    ;; Analyze convergence across all rank pairs
    (when (>= (length ranks) 2)
      (dotimes (i (1- (length ranks)))
        (let* ((from-rank (nth i ranks))
               (to-rank (nth (1+ i) ranks))
               (convergence (dag-draw--max-edges-to-same-destination graph from-rank to-rank))
               (edge-count (dag-draw--count-edges-between-ranks graph from-rank to-rank)))
          (setq max-convergence (max max-convergence convergence))
          (setq total-rank-transitions (+ total-rank-transitions edge-count)))))
    
    (list :total-nodes total-nodes
          :total-edges total-edges
          :rank-count (length ranks)
          :max-convergence max-convergence
          :total-rank-transitions total-rank-transitions
          :avg-edges-per-transition (if (> (length ranks) 1)
                                       (/ (float total-rank-transitions) (1- (length ranks)))
                                     0))))

(defun dag-draw--debug-spacing-calculation (graph)
  "Debug helper to show spacing calculation details.
Prints analysis of graph structure and spacing requirements."
  (let* ((complexity (dag-draw--analyze-graph-complexity graph))
         (max-spacing (dag-draw--calculate-max-required-rank-separation graph))
         (ranks (dag-draw--get-graph-ranks graph)))
    
    (message "=== DYNAMIC SPACING ANALYSIS ===")
    (message "Graph complexity: %s" complexity)
    (message "Calculated max rank separation: %d ASCII rows" max-spacing)
    
    (when (>= (length ranks) 2)
      (dotimes (i (1- (length ranks)))
        (let* ((from-rank (nth i ranks))
               (to-rank (nth (1+ i) ranks))
               (spacing (dag-draw--calculate-dynamic-rank-separation graph from-rank to-rank))
               (edges (dag-draw--count-edges-between-ranks graph from-rank to-rank))
               (convergence (dag-draw--max-edges-to-same-destination graph from-rank to-rank)))
          (message "Rank %d→%d: %d rows (edges: %d, max convergence: %d)" 
                   from-rank to-rank spacing edges convergence))))
    
    (message "=== END SPACING ANALYSIS ===")
    max-spacing))

(provide 'dag-draw-quality)

;;; dag-draw-quality.el ends here