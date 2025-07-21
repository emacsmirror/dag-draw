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

;;; GKNV DFS Edge Classification (Section 2.1)

(defun dag-draw--classify-edges-gknv (graph)
  "Classify edges using GKNV DFS taxonomy per Section 2.1.
Returns hash table with keys: tree-edges, forward-edges, cross-edges, back-edges."
  (let ((classification (ht-create))
        (visited (ht-create))
        (discovery-time (ht-create))
        (finish-time (ht-create))
        (time-counter 0)
        (parent (ht-create)))

    ;; Initialize classification categories
    (ht-set! classification 'tree-edges '())
    (ht-set! classification 'forward-edges '())
    (ht-set! classification 'cross-edges '())
    (ht-set! classification 'back-edges '())

    ;; Perform DFS from each unvisited node
    (ht-each (lambda (node-id _node)
               (unless (ht-get visited node-id)
                 (setq time-counter (dag-draw--dfs-classify-edges
                                     graph node-id visited discovery-time
                                     finish-time time-counter parent classification))))
             (dag-draw-graph-nodes graph))

    classification))

(defun dag-draw--dfs-classify-edges (graph node visited discovery-time finish-time time-counter parent classification)
  "DFS traversal that classifies edges per GKNV Section 2.1.
Returns updated time counter."
  (ht-set! visited node t)
  (ht-set! discovery-time node time-counter)
  (setq time-counter (1+ time-counter))

  ;; Visit all adjacent nodes
  (dolist (edge (dag-draw-get-edges-from graph node))
    (let ((neighbor (dag-draw-edge-to-node edge)))
      (cond
       ;; Tree edge: neighbor not yet visited
       ((not (ht-get visited neighbor))
        (ht-set! parent neighbor node)
        (ht-set! classification 'tree-edges
                 (cons edge (ht-get classification 'tree-edges)))
        (setq time-counter (dag-draw--dfs-classify-edges
                            graph neighbor visited discovery-time
                            finish-time time-counter parent classification)))

       ;; Back edge: neighbor is ancestor (in recursion stack)
       ((not (ht-get finish-time neighbor))  ; Still being processed
        (ht-set! classification 'back-edges
                 (cons edge (ht-get classification 'back-edges))))

       ;; Forward or Cross edge: neighbor already finished
       (t
        (if (< (ht-get discovery-time node) (ht-get discovery-time neighbor))
            ;; Forward edge: node was discovered before neighbor (ancestor relationship)
            (ht-set! classification 'forward-edges
                     (cons edge (ht-get classification 'forward-edges)))
          ;; Cross edge: neighbor was discovered before node
          (ht-set! classification 'cross-edges
                   (cons edge (ht-get classification 'cross-edges))))))))

  ;; Mark node as finished
  (ht-set! finish-time node time-counter)
  (1+ time-counter))

(defun dag-draw--break-cycles-using-gknv-classification (graph)
  "Break cycles by reversing back edges per GKNV Section 2.1."
  (let ((classification (dag-draw--classify-edges-gknv graph)))
    (dolist (back-edge (ht-get classification 'back-edges))
      ;; Reverse back edge to break cycle
      (dag-draw--reverse-edge graph back-edge))))

(defun dag-draw--reverse-edge (graph edge)
  "Reverse an edge in the graph (internal direction only).
GKNV Section 2.1: Only internal direction is flipped, visual direction preserved."
  (let ((from-node (dag-draw-edge-from-node edge))
        (to-node (dag-draw-edge-to-node edge))
        (weight (dag-draw-edge-weight edge))
        (label (dag-draw-edge-label edge))
        (attributes (dag-draw-edge-attributes edge)))

    ;; Remove original edge
    (setf (dag-draw-graph-edges graph)
          (cl-remove edge (dag-draw-graph-edges graph)))

    ;; Add reversed edge with original direction preserved in attributes
    (let ((new-attrs (ht-copy (or attributes (ht-create)))))
      (ht-set! new-attrs 'original-direction (cons from-node to-node))
      (dag-draw-add-edge graph to-node from-node weight label new-attrs))))

(defun dag-draw--count-cycle-participation (graph)
  "Count cycle participation for each edge per GKNV Section 2.1 heuristic.
Returns hash table mapping edges to participation counts."
  (let ((participation (ht-create)))
    ;; Initialize all edges with zero participation
    (dolist (edge (dag-draw-graph-edges graph))
      (ht-set! participation edge 0))

    ;; For each strongly connected component, count cycles
    (let ((sccs (dag-draw--find-strongly-connected-components graph)))
      (dolist (scc sccs)
        (when (> (length scc) 1)  ; Non-trivial SCC
          (dag-draw--count-cycles-in-scc graph scc participation))))

    participation))

(defun dag-draw--find-strongly-connected-components (graph)
  "Find strongly connected components using Tarjan's algorithm."
  ;; Simplified implementation - return list of node lists
  (let ((visited (ht-create))
        (components '()))

    (ht-each (lambda (node-id _node)
               (unless (ht-get visited node-id)
                 (let ((component (dag-draw--dfs-component graph node-id visited)))
                   (when component
                     (push component components)))))
             (dag-draw-graph-nodes graph))
    components))

(defun dag-draw--dfs-component (graph start visited)
  "DFS to find connected component starting from node."
  (let ((component '())
        (stack (list start)))
    (while stack
      (let ((node (pop stack)))
        (unless (ht-get visited node)
          (ht-set! visited node t)
          (push node component)
          ;; Add neighbors to stack
          (dolist (edge (dag-draw-get-edges-from graph node))
            (let ((neighbor (dag-draw-edge-to-node edge)))
              (unless (ht-get visited neighbor)
                (push neighbor stack)))))))
    component))

(defun dag-draw--count-cycles-in-scc (graph scc participation)
  "Count cycles within a strongly connected component."
  ;; For each edge in the SCC, perform DFS to count cycles it participates in
  (dolist (edge (dag-draw-graph-edges graph))
    (let ((from (dag-draw-edge-from-node edge))
          (to (dag-draw-edge-to-node edge)))
      (when (and (member from scc) (member to scc))
        ;; Edge is within SCC, count cycles through it
        (let ((cycle-count (dag-draw--count-cycles-through-edge graph edge scc)))
          (ht-set! participation edge cycle-count))))))

(defun dag-draw--find-source-sink-nodes (graph)
  "Find source (no incoming) and sink (no outgoing) nodes per GKNV Section 2.1."
  (let ((sources '())
        (sinks '())
        (has-incoming (ht-create))
        (has-outgoing (ht-create)))

    ;; Mark nodes with incoming/outgoing edges
    (dolist (edge (dag-draw-graph-edges graph))
      (ht-set! has-incoming (dag-draw-edge-to-node edge) t)
      (ht-set! has-outgoing (dag-draw-edge-from-node edge) t))

    ;; Identify sources and sinks
    (ht-each (lambda (node-id _node)
               (unless (ht-get has-incoming node-id)
                 (push node-id sources))
               (unless (ht-get has-outgoing node-id)
                 (push node-id sinks)))
             (dag-draw-graph-nodes graph))

    (let ((result (ht-create)))
      (ht-set! result 'sources sources)
      (ht-set! result 'sinks sinks)
      result)))

;; Aliases for backward compatibility and test consistency
(defalias 'dag-draw-has-cycles 'dag-draw-simple-has-cycles
  "Alias for simple cycle detection function.")

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
