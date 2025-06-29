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
  "Order vertices within ranks using a simplified approach.
This is the second pass of the GKNV algorithm."
  
  ;; Simple approach: assign order based on alphabetical/id ordering for now
  ;; This can be enhanced later with the full weighted median heuristic
  (let ((rank-to-nodes (ht-create)))
    
    ;; Group nodes by rank
    (ht-each (lambda (node-id node)
               (let ((rank (or (dag-draw-node-rank node) 0)))
                 (ht-set! rank-to-nodes rank
                          (cons node-id (ht-get rank-to-nodes rank '())))))
             (dag-draw-graph-nodes graph))
    
    ;; Assign order within each rank
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
  
  graph)

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

(provide 'dag-draw-order-simple)

;;; dag-draw-order-simple.el ends here