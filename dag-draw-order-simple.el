;;; dag-draw-order-simple.el --- Simple vertex ordering for dag-draw -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Simplified implementation of vertex ordering within ranks.
;; This provides basic ordering functionality without the full complexity
;; of the weighted median heuristic.

;;; Code:

(require 'dash)
(require 'ht)
(require 'dag-draw)
(require 'dag-draw-core)

(defun dag-draw-order-vertices (graph)
  "Order vertices within ranks with enhanced convergence detection.
This is the second pass of the GKNV algorithm."
  
  ;; Enhanced approach with convergence detection
  (let ((convergence-result (dag-draw--simple-crossing-reduction-with-convergence graph)))
    
    ;; Log convergence information
    (message "Crossing reduction completed: iterations=%s converged=%s"
             (ht-get convergence-result 'iterations)
             (ht-get convergence-result 'converged))
    
    graph))

(defun dag-draw--simple-crossing-reduction-with-convergence (graph)
  "Simple crossing reduction with convergence detection."
  (let ((max-iterations 12)
        (convergence-threshold 3)
        (iterations-without-improvement 0)
        (best-ordering-cost most-positive-fixnum)
        (result (ht-create)))
    
    ;; Iterative crossing reduction with convergence detection
    (let ((iteration 0)
          (converged nil))
      
      ;; Initial ordering - group nodes by rank
      (let ((rank-to-nodes (ht-create)))
        (ht-each (lambda (node-id node)
                   (let ((rank (or (dag-draw-node-rank node) 0)))
                     (ht-set! rank-to-nodes rank
                              (cons node-id (ht-get rank-to-nodes rank '())))))
                 (dag-draw-graph-nodes graph))
        
        ;; Assign initial alphabetical order within each rank
        (ht-each (lambda (rank node-list)
                   (let ((sorted-nodes (sort node-list (lambda (a b) 
                                                         (string< (symbol-name a) 
                                                                 (symbol-name b)))))
                         (order 0))
                     (dolist (node-id sorted-nodes)
                       (let ((node (dag-draw-get-node graph node-id)))
                         (when node
                           (setf (dag-draw-node-order node) order)
                           (setq order (1+ order)))))))
                 rank-to-nodes))
      
      ;; Iterative improvement loop
      (while (and (< iteration max-iterations) (not converged))
        (let ((current-cost (dag-draw--simple-calculate-ordering-cost graph)))
          
          ;; Try some simple improvements (placeholder for more sophisticated algorithms)
          (dag-draw--simple-try-swaps graph)
          
          ;; Check for improvement
          (let ((new-cost (dag-draw--simple-calculate-ordering-cost graph)))
            (if (< new-cost best-ordering-cost)
                (progn
                  (setq best-ordering-cost new-cost)
                  (setq iterations-without-improvement 0))
              (setq iterations-without-improvement (1+ iterations-without-improvement))))
          
          ;; Check convergence
          (when (>= iterations-without-improvement convergence-threshold)
            (setq converged t))
          
          (setq iteration (1+ iteration))))
      
      ;; Store results
      (ht-set! result 'iterations iteration)
      (ht-set! result 'converged converged)
      (ht-set! result 'final-cost best-ordering-cost)
      
      result)))

(defun dag-draw--simple-calculate-ordering-cost (graph)
  "Calculate a simple cost metric for the current node ordering."
  ;; Simple cost: sum of differences in order between connected nodes
  (let ((total-cost 0))
    (dolist (edge (dag-draw-graph-edges graph))
      (let* ((from-node (dag-draw-get-node graph (dag-draw-edge-from-node edge)))
             (to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge)))
             (from-order (or (dag-draw-node-order from-node) 0))
             (to-order (or (dag-draw-node-order to-node) 0)))
        (setq total-cost (+ total-cost (abs (- from-order to-order))))))
    total-cost))

(defun dag-draw--simple-try-swaps (graph)
  "Try simple adjacent swaps to improve ordering."
  ;; Very simple: try swapping adjacent nodes in same rank
  ;; This is a placeholder for more sophisticated optimization
  (let ((nodes-by-rank (ht-create)))
    ;; Group nodes by rank
    (ht-each (lambda (node-id node)
               (let ((rank (or (dag-draw-node-rank node) 0)))
                 (ht-set! nodes-by-rank rank
                          (cons node-id (ht-get nodes-by-rank rank '())))))
             (dag-draw-graph-nodes graph))
    
    ;; For each rank, try swapping adjacent pairs
    (ht-each (lambda (rank node-list)
               (when (> (length node-list) 1)
                 (let ((sorted-nodes (sort node-list 
                                          (lambda (a b)
                                            (< (or (dag-draw-node-order (dag-draw-get-node graph a)) 0)
                                               (or (dag-draw-node-order (dag-draw-get-node graph b)) 0))))))
                   ;; Try one swap per rank per iteration (very conservative)
                   (when (>= (length sorted-nodes) 2)
                     (let* ((node1 (dag-draw-get-node graph (car sorted-nodes)))
                            (node2 (dag-draw-get-node graph (cadr sorted-nodes)))
                            (order1 (dag-draw-node-order node1))
                            (order2 (dag-draw-node-order node2)))
                       ;; Swap orders
                       (setf (dag-draw-node-order node1) order2)
                       (setf (dag-draw-node-order node2) order1))))))
             nodes-by-rank)))

;;; GKNV Crossing Reduction Implementation

(defun dag-draw--count-crossings (graph rank1 rank2)
  "Count edge crossings between RANK1 and RANK2.
Returns number of edge crossings between adjacent ranks."
  (let ((rank1-nodes (dag-draw--get-nodes-in-rank graph rank1))
        (rank2-nodes (dag-draw--get-nodes-in-rank graph rank2))
        (crossings 0))
    
    ;; For each pair of edges between the ranks, check if they cross
    (dolist (node1 rank1-nodes)
      (dolist (node2 rank1-nodes)
        (when (not (eq node1 node2))
          (let ((node1-successors (dag-draw--get-successors-in-rank graph node1 rank2))
                (node2-successors (dag-draw--get-successors-in-rank graph node2 rank2)))
            ;; Check all pairs of edges for crossings
            (dolist (succ1 node1-successors)
              (dolist (succ2 node2-successors)
                (when (dag-draw--edges-cross-p graph node1 succ1 node2 succ2)
                  (setq crossings (1+ crossings)))))))))
    
    ;; Each crossing is counted twice, so divide by 2
    (/ crossings 2)))

(defun dag-draw--get-nodes-in-rank (graph rank)
  "Get all nodes in the specified RANK."
  (let ((nodes '()))
    (ht-each (lambda (node-id node)
               (when (and (dag-draw-node-rank node)
                          (= (dag-draw-node-rank node) rank))
                 (push node-id nodes)))
             (dag-draw-graph-nodes graph))
    nodes))

(defun dag-draw--get-successors-in-rank (graph node-id target-rank)
  "Get successors of NODE-ID that are in TARGET-RANK."
  (let ((successors '()))
    (dolist (succ (dag-draw-get-successors graph node-id))
      (let ((succ-node (dag-draw-get-node graph succ)))
        (when (and (dag-draw-node-rank succ-node)
                   (= (dag-draw-node-rank succ-node) target-rank))
          (push succ successors))))
    successors))

(defun dag-draw--edges-cross-p (graph node1 target1 node2 target2)
  "Check if edges (NODE1->TARGET1) and (NODE2->TARGET2) cross."
  (let ((node1-order (dag-draw-node-order (dag-draw-get-node graph node1)))
        (node2-order (dag-draw-node-order (dag-draw-get-node graph node2)))
        (target1-order (dag-draw-node-order (dag-draw-get-node graph target1)))
        (target2-order (dag-draw-node-order (dag-draw-get-node graph target2))))
    
    ;; Edges cross if their relative order reverses between ranks
    (and node1-order node2-order target1-order target2-order
         (or (and (< node1-order node2-order) (> target1-order target2-order))
             (and (> node1-order node2-order) (< target1-order target2-order))))))

(defun dag-draw--median-order (graph rank)
  "Apply median heuristic to order nodes in RANK based on connected nodes."
  (let ((rank-nodes (dag-draw--get-nodes-in-rank graph rank)))
    ;; For each node in the rank, calculate its median position
    (dolist (node-id rank-nodes)
      (let ((median-pos (dag-draw--calculate-median-position graph node-id)))
        ;; Set order based on median position (simplified)
        (setf (dag-draw-node-order (dag-draw-get-node graph node-id)) median-pos)))))

(defun dag-draw--calculate-median-position (graph node-id)
  "Calculate median position for NODE-ID based on connected nodes."
  (let ((connected-positions '()))
    ;; Get positions of connected nodes in adjacent ranks
    (dolist (pred (dag-draw-get-predecessors graph node-id))
      (let ((pred-order (dag-draw-node-order (dag-draw-get-node graph pred))))
        (when pred-order
          (push pred-order connected-positions))))
    
    (dolist (succ (dag-draw-get-successors graph node-id))
      (let ((succ-order (dag-draw-node-order (dag-draw-get-node graph succ))))
        (when succ-order
          (push succ-order connected-positions))))
    
    ;; Return median of connected positions, or 0 if no connections
    (if connected-positions
        (let ((sorted-positions (sort connected-positions '<)))
          (nth (/ (length sorted-positions) 2) sorted-positions))
      0)))

;;; TDD Advanced Crossing Reduction Implementation

(defun dag-draw--calculate-weighted-median (graph node-id)
  "Calculate weighted median position for NODE-ID based on edge weights.
Returns the weighted median position considering connected node positions and edge weights."
  (let ((weighted-positions '())
        (total-weight 0))
    
    ;; Collect weighted positions from connected nodes
    (dolist (pred (dag-draw-get-predecessors graph node-id))
      (let* ((pred-node (dag-draw-get-node graph pred))
             (pred-order (or (dag-draw-node-order pred-node) 0))
             (edge-weight (dag-draw--get-edge-weight graph pred node-id)))
        (push (cons pred-order edge-weight) weighted-positions)
        (setq total-weight (+ total-weight edge-weight))))
    
    (dolist (succ (dag-draw-get-successors graph node-id))
      (let* ((succ-node (dag-draw-get-node graph succ))
             (succ-order (or (dag-draw-node-order succ-node) 0))
             (edge-weight (dag-draw--get-edge-weight graph node-id succ)))
        (push (cons succ-order edge-weight) weighted-positions)
        (setq total-weight (+ total-weight edge-weight))))
    
    ;; Calculate weighted median
    (if weighted-positions
        (let ((sorted-positions (sort weighted-positions (lambda (a b) (< (car a) (car b)))))
              (target-weight (/ total-weight 2.0))
              (cumulative-weight 0))
          (catch 'found
            (dolist (pos-weight sorted-positions)
              (setq cumulative-weight (+ cumulative-weight (cdr pos-weight)))
              (when (>= cumulative-weight target-weight)
                (throw 'found (car pos-weight))))
            0))  ; fallback
      0)))  ; no connections

(defun dag-draw--get-edge-weight (graph from-node to-node)
  "Get weight of edge between FROM-NODE and TO-NODE."
  (let ((edge (dag-draw--find-edge graph from-node to-node)))
    (if edge
        (dag-draw-edge-weight edge)
      1)))  ; default weight

(defun dag-draw--find-edge (graph from-node to-node)
  "Find edge between FROM-NODE and TO-NODE."
  (catch 'found
    (dolist (edge (dag-draw-graph-edges graph))
      (when (and (eq (dag-draw-edge-from-node edge) from-node)
                 (eq (dag-draw-edge-to-node edge) to-node))
        (throw 'found edge)))
    nil))

(defun dag-draw--iterative-crossing-reduction (graph)
  "Apply iterative crossing reduction until convergence.
Returns hash table with convergence information and final crossing count."
  (let ((result (ht-create))
        (max-iterations 10)
        (iterations 0)
        (converged nil)
        (prev-crossings most-positive-fixnum))
    
    ;; Main iteration loop
    (while (and (< iterations max-iterations) (not converged))
      (setq iterations (1+ iterations))
      
      ;; Apply crossing reduction to all rank pairs
      (let ((max-rank (or (dag-draw-graph-max-rank graph) 0)))
        (dotimes (rank max-rank)
          (dag-draw--median-order graph (1+ rank))))
      
      ;; Calculate total crossings
      (let ((total-crossings (dag-draw--calculate-total-crossings graph)))
        (if (>= total-crossings prev-crossings)
            (setq converged t)  ; No improvement - converged
          (setq prev-crossings total-crossings))))
    
    ;; Store results
    (ht-set! result 'converged converged)
    (ht-set! result 'iterations iterations)
    (ht-set! result 'final-crossings prev-crossings)
    
    result))

(defun dag-draw--calculate-total-crossings (graph)
  "Calculate total number of crossings in the graph."
  (let ((total-crossings 0)
        (max-rank (or (dag-draw-graph-max-rank graph) 0)))
    
    ;; Sum crossings between all adjacent rank pairs
    (dotimes (rank max-rank)
      (setq total-crossings (+ total-crossings 
                               (dag-draw--count-crossings graph rank (1+ rank)))))
    
    total-crossings))

(defun dag-draw--optimize-two-layer-crossings (graph rank1 rank2)
  "Optimize crossings between two specific layers.
Returns hash table with optimization results."
  (let ((result (ht-create)))
    
    ;; Count initial crossings
    (let ((initial-crossings (dag-draw--count-crossings graph rank1 rank2)))
      
      ;; Apply weighted median heuristic to the second layer
      (let ((rank2-nodes (dag-draw--get-nodes-in-rank graph rank2)))
        (dolist (node-id rank2-nodes)
          (let ((weighted-median (dag-draw--calculate-weighted-median graph node-id)))
            (setf (dag-draw-node-order (dag-draw-get-node graph node-id)) weighted-median))))
      
      ;; Count final crossings
      (let ((final-crossings (dag-draw--count-crossings graph rank1 rank2)))
        (ht-set! result 'optimization-applied t)
        (ht-set! result 'initial-crossings initial-crossings)
        (ht-set! result 'final-crossings final-crossings)))
    
    result))

(provide 'dag-draw-order-simple)

;;; dag-draw-order-simple.el ends here