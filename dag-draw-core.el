;;; dag-draw-core.el --- Core utilities for dag-draw -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Core utility functions and data structure helpers for the dag-draw package.
;; This includes graph traversal, validation, and manipulation functions.

;;; Code:

(require 'dash)
(require 'ht)
(require 'dag-draw)

;;; Graph Traversal and Analysis

(defun dag-draw-get-node (graph node-id)
  "Get the node with NODE-ID from GRAPH, or nil if not found."
  (ht-get (dag-draw-graph-nodes graph) node-id))

(defun dag-draw-get-edges-from (graph node-id)
  "Get all edges originating from NODE-ID in GRAPH."
  (--filter (eq (dag-draw-edge-from-node it) node-id)
            (dag-draw-graph-edges graph)))

(defun dag-draw-get-edges-to (graph node-id)
  "Get all edges terminating at NODE-ID in GRAPH."
  (--filter (eq (dag-draw-edge-to-node it) node-id)
            (dag-draw-graph-edges graph)))

(defun dag-draw-get-successors (graph node-id)
  "Get list of successor node IDs for NODE-ID in GRAPH."
  (mapcar #'dag-draw-edge-to-node (dag-draw-get-edges-from graph node-id)))

(defun dag-draw-get-predecessors (graph node-id)
  "Get list of predecessor node IDs for NODE-ID in GRAPH."
  (mapcar #'dag-draw-edge-from-node (dag-draw-get-edges-to graph node-id)))

(defun dag-draw-get-node-ids (graph)
  "Get list of all node IDs in GRAPH."
  (ht-keys (dag-draw-graph-nodes graph)))

(defun dag-draw-node-count (graph)
  "Get the number of nodes in GRAPH."
  (ht-size (dag-draw-graph-nodes graph)))

(defun dag-draw-edge-count (graph)
  "Get the number of edges in GRAPH."
  (length (dag-draw-graph-edges graph)))

;;; Graph Properties

(defun dag-draw-get-source-nodes (graph)
  "Get list of source nodes (nodes with no incoming edges) in GRAPH."
  (let ((all-nodes (dag-draw-get-node-ids graph))
        (target-nodes (mapcar #'dag-draw-edge-to-node (dag-draw-graph-edges graph))))
    (--filter (not (member it target-nodes)) all-nodes)))

;;; Graph Modification

(defun dag-draw-remove-node (graph node-id)
  "Remove NODE-ID and all connected edges from GRAPH."
  (when (ht-get (dag-draw-graph-nodes graph) node-id)
    ;; Remove all edges connected to this node
    (setf (dag-draw-graph-edges graph)
          (--remove (or (eq (dag-draw-edge-from-node it) node-id)
                        (eq (dag-draw-edge-to-node it) node-id))
                    (dag-draw-graph-edges graph)))
    ;; Remove the node itself
    (ht-remove! (dag-draw-graph-nodes graph) node-id))
  graph)

(defun dag-draw-remove-edge (graph from-node to-node)
  "Remove the edge from FROM-NODE to TO-NODE in GRAPH."
  (setf (dag-draw-graph-edges graph)
        (--remove (and (eq (dag-draw-edge-from-node it) from-node)
                       (eq (dag-draw-edge-to-node it) to-node))
                  (dag-draw-graph-edges graph)))
  graph)

;;; Graph Copying and Cloning

(defun dag-draw-copy-graph (graph)
  "Create a deep copy of GRAPH."
  (let ((new-graph (dag-draw-graph-create
                    :node-separation (dag-draw-graph-node-separation graph)
                    :rank-separation (dag-draw-graph-rank-separation graph)
                    :attributes (ht-copy (dag-draw-graph-attributes graph)))))
    
    ;; Copy nodes
    (ht-each (lambda (node-id node)
               (let ((new-node (dag-draw-node-create
                               :id (dag-draw-node-id node)
                               :label (dag-draw-node-label node)
                               :x-size (dag-draw-node-x-size node)
                               :y-size (dag-draw-node-y-size node)
                               :x-coord (dag-draw-node-x-coord node)
                               :y-coord (dag-draw-node-y-coord node)
                               :rank (dag-draw-node-rank node)
                               :order (dag-draw-node-order node)
                               :attributes (if (dag-draw-node-attributes node) 
                                             (ht-copy (dag-draw-node-attributes node))
                                             (ht-create)))))
                 (ht-set! (dag-draw-graph-nodes new-graph) node-id new-node)))
             (dag-draw-graph-nodes graph))
    
    ;; Copy edges
    (dolist (edge (dag-draw-graph-edges graph))
      (let ((new-edge (dag-draw-edge-create
                      :from-node (dag-draw-edge-from-node edge)
                      :to-node (dag-draw-edge-to-node edge)
                      :weight (dag-draw-edge-weight edge)
                      :min-length (dag-draw-edge-δ edge)  ; GKNV δ(e) notation
                      :label (dag-draw-edge-label edge)
                      :spline-points (copy-sequence (dag-draw-edge-spline-points edge))
                      :attributes (if (dag-draw-edge-attributes edge)
                                    (ht-copy (dag-draw-edge-attributes edge))
                                    (ht-create)))))
        (push new-edge (dag-draw-graph-edges new-graph))))
    
    ;; Copy other graph properties
    (setf (dag-draw-graph-max-rank new-graph) (dag-draw-graph-max-rank graph))
    (setf (dag-draw-graph-rank-sets new-graph) (copy-tree (dag-draw-graph-rank-sets graph)))
    
    new-graph))

;;; Debugging and Inspection

(defun dag-draw-graph-summary (graph)
  "Return a human-readable summary string of GRAPH."
  (format "Graph: %d nodes, %d edges, max-rank: %s"
          (dag-draw-node-count graph)
          (dag-draw-edge-count graph)
          (or (dag-draw-graph-max-rank graph) "unset")))

(provide 'dag-draw-core)

;;; dag-draw-core.el ends here