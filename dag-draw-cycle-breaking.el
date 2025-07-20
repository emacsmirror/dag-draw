;;; dag-draw-cycle-breaking.el --- Cycle detection and breaking for DAG layout -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Claude Code
;; Version: 1.0
;; Keywords: graphs, layout, cycles

;;; Commentary:

;; This module implements cycle detection and breaking algorithms for
;; directed graph layout. Based on GKNV paper requirements for creating
;; acyclic graphs as input to the ranking algorithm.
;;
;; Functions:
;; - dag-draw-simple-has-cycles: Detect if graph contains cycles
;; - dag-draw-simple-break-cycles: Remove edges to break all cycles
;; - dag-draw--simple-has-cycle: Helper for DFS cycle detection

;;; Code:

(require 'ht)
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

(provide 'dag-draw-cycle-breaking)

;;; dag-draw-cycle-breaking.el ends here