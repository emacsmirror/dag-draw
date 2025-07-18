;;; dag-draw-test-helpers.el --- Helper functions for dag-draw tests -*- lexical-binding: t -*-

;; Copyright (C) 2024

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

(provide 'dag-draw-test-helpers)

;;; dag-draw-test-helpers.el ends here