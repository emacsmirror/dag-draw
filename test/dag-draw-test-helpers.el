;;; dag-draw-test-helpers.el --- Helper functions for dag-draw tests -*- lexical-binding: t -*-

;; Copyright (C) 2024, 2025

;;; Commentary:

;; Test helper functions for dag-draw testing. These are utility functions
;; used by test code to verify algorithm behavior, not part of the main
;; algorithm implementation.

;;; Code:

(require 'dag-draw-core)

;;; Packcut Test Helpers

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
               (when (= (dag-draw-node-rank node) rank)
                 (push node-id nodes-in-rank)))
             (dag-draw-graph-nodes graph))
    
    ;; Sort by X coordinate
    (sort nodes-in-rank 
          (lambda (a b)
            (< (dag-draw-node-x-coord (dag-draw-get-node graph a))
               (dag-draw-node-x-coord (dag-draw-get-node graph b)))))))

(defun dag-draw--find-rank-compaction-opportunities (graph nodes-in-rank)
  "Find compaction opportunities within NODES-IN-RANK.
Returns list of compaction operations for this rank."
  (let ((opportunities '()))
    
    ;; Check each adjacent pair of nodes for excessive spacing
    (when (>= (length nodes-in-rank) 2)
      (dotimes (i (1- (length nodes-in-rank)))
        (let* ((left-node-id (nth i nodes-in-rank))
               (right-node-id (nth (1+ i) nodes-in-rank))
               (left-node (dag-draw-get-node graph left-node-id))
               (right-node (dag-draw-get-node graph right-node-id))
               (left-x (dag-draw-node-x-coord left-node))
               (right-x (dag-draw-node-x-coord right-node))
               (left-width (dag-draw-node-x-size left-node))
               (right-width (dag-draw-node-x-size right-node))
               (node-sep (dag-draw-graph-node-separation graph))
               (current-gap (- right-x left-x))
               (min-required-gap (+ (/ (+ left-width right-width) 2.0) node-sep))
               (excess-space (- current-gap min-required-gap)))
          
          ;; If there's significant excess space, record compaction opportunity
          (when (> excess-space 5.0)  ; Threshold for worthwhile compaction
            (push (list :can-compact t
                       :savings excess-space
                       :left-node left-node-id
                       :right-node right-node-id
                       :current-gap current-gap
                       :min-gap min-required-gap)
                  opportunities)))))
    
    opportunities))

(defun dag-draw--get-graph-ranks (graph)
  "Get list of all ranks present in GRAPH."
  (let ((ranks '()))
    (ht-each (lambda (_node-id node)
               (let ((rank (dag-draw-node-rank node)))
                 (unless (member rank ranks)
                   (push rank ranks))))
             (dag-draw-graph-nodes graph))
    (sort ranks '<)))

;;; Network Simplex Test Helpers

(defun dag-draw--contains-high-weight-edge-p (tree-edges from-node to-node)
  "Test helper: Check if tree edges contain a high-weight edge from FROM-NODE to TO-NODE.
Returns t if the spanning tree contains an edge between the specified nodes with weight > 2."
  (cl-some (lambda (edge)
             (and (dag-draw-tree-edge-p edge)
                  (eq (dag-draw-tree-edge-from-node edge) from-node)
                  (eq (dag-draw-tree-edge-to-node edge) to-node)
                  (> (dag-draw-tree-edge-weight edge) 2)))
           tree-edges))

(defun dag-draw--cost-reflects-weight-distance-product-p (graph spanning-tree network-cost)
  "Test helper: Check if network cost correctly reflects weight-distance product.
Verifies that the network cost is calculated using the GKNV formula: Σ(weight × length)."
  (let* ((tree-edges (dag-draw-spanning-tree-edges spanning-tree))
         (expected-cost 0))
    
    ;; Calculate expected cost using same formula as dag-draw--calculate-network-cost
    (dolist (edge tree-edges)
      (let* ((from-node (dag-draw-tree-edge-from-node edge))
             (to-node (dag-draw-tree-edge-to-node edge))
             (weight (dag-draw-tree-edge-weight edge))
             (original-edge (dag-draw--find-graph-edge graph from-node to-node))
             (edge-length (if original-edge
                              (dag-draw-edge-min-length original-edge)
                            1)))
        (setq expected-cost (+ expected-cost (* weight edge-length)))))
    
    ;; Network cost should match our expected calculation
    (= network-cost expected-cost)))

(defun dag-draw--uses-high-weight-edges-effectively-p (spanning-tree)
  "Test helper: Check if spanning tree uses high-weight edges effectively.
Returns t if the spanning tree avoids including high-weight edges where possible,
indicating effective network simplex optimization."
  (let ((tree-edges (dag-draw-spanning-tree-edges spanning-tree))
        (high-weight-count 0)
        (total-weight 0))
    ;; Count high-weight edges and total weight
    (dolist (edge tree-edges)
      (let ((weight (dag-draw-tree-edge-weight edge)))
        (setq total-weight (+ total-weight weight))
        (when (> weight 5)  ; Consider weight > 5 as high
          (cl-incf high-weight-count))))
    ;; Effective use means: few high-weight edges relative to total
    (and (< high-weight-count 3)  ; At most 2 high-weight edges
         (< total-weight 20))))   ; Keep total weight reasonable

(defun dag-draw--sampled-points-length (points)
  "Calculate approximate length from pre-sampled spline points.
Test helper for verifying spline generation quality."
  (let ((total-length 0.0))
    (when (> (length points) 1)
      (dotimes (i (1- (length points)))
        (let* ((p1 (nth i points))
               (p2 (nth (1+ i) points))
               (dx (- (dag-draw-point-x p2) (dag-draw-point-x p1)))
               (dy (- (dag-draw-point-y p2) (dag-draw-point-y p1))))
          (setq total-length (+ total-length (sqrt (+ (* dx dx) (* dy dy))))))))
    total-length))

(provide 'dag-draw-test-helpers)

;;; dag-draw-test-helpers.el ends here