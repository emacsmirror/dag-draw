;;; dag-draw-topological.el --- Topological rank assignment fallback -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Claude Code
;; Version: 1.0
;; Keywords: graphs, layout, topological, ranking

;;; Commentary:

;; This module implements topological sorting as a fallback rank assignment
;; algorithm for the GKNV layout system. This is used when the network simplex
;; algorithm fails or is not suitable.
;;
;; The topological approach processes nodes level by level based on their
;; in-degree, ensuring that all predecessor nodes are assigned ranks before
;; their successors. This guarantees a valid ranking for acyclic graphs.
;;
;; Functions:
;; - dag-draw--assign-ranks-topological: Main topological rank assignment

;;; Code:

(require 'ht)
(require 'cl-lib)
(require 'dag-draw-core)

;;; Topological Rank Assignment

(defun dag-draw--assign-ranks-topological (graph)
  "Assign ranks using a simple topological approach.
This is a fallback algorithm when network simplex optimization fails.
Uses Kahn's algorithm for topological sorting with level-by-level processing."
  (let ((in-degree (ht-create))
        (queue '())
        (current-rank 0))

    ;; Step 1: Calculate in-degrees for all nodes
    (dolist (node-id (dag-draw-get-node-ids graph))
      (ht-set! in-degree node-id 0))

    (dolist (edge (dag-draw-graph-edges graph))
      (let ((to-node (dag-draw-edge-to-node edge)))
        (ht-set! in-degree to-node (1+ (ht-get in-degree to-node 0)))))

    ;; Step 2: Find initial nodes with in-degree 0 (source nodes)
    (dolist (node-id (dag-draw-get-node-ids graph))
      (when (zerop (ht-get in-degree node-id))
        (push node-id queue)))

    ;; Step 3: Process nodes level by level using Kahn's algorithm
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

    ;; Step 4: Set maximum rank in graph for layout algorithms
    (setf (dag-draw-graph-max-rank graph) (1- current-rank))

    graph))

(provide 'dag-draw-topological)

;;; dag-draw-topological.el ends here