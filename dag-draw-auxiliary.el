;;; dag-draw-auxiliary.el --- Auxiliary node management for GKNV algorithm -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Claude Code
;; Version: 1.0
;; Keywords: graphs, layout, auxiliary, gknv

;;; Commentary:

;; This module implements auxiliary node and edge management for the GKNV
;; layout algorithm. Auxiliary nodes (S_min and S_max) are used to ensure
;; a connected graph structure for the network simplex algorithm.
;;
;; Based on GKNV Section 2.1: "for all nodes v with no in-edge, we make a 
;; temporary edge (S_min,v) with δ = 0, and for all nodes v with no out-edge,
;; we make a temporary edge (v,S_max) with δ = 0."
;;
;; Functions:
;; - dag-draw--cleanup-auxiliary-elements: Remove auxiliary nodes and edges
;; - dag-draw--find-auxiliary-nodes: Find all auxiliary nodes in graph
;; - dag-draw--find-auxiliary-edges: Find all auxiliary edges in graph
;; - dag-draw--assign-basic-ranks-with-auxiliary: Assign ranks using auxiliary structure
;; - dag-draw--is-auxiliary-node-p: Check if node is auxiliary
;; - dag-draw--connect-auxiliary-source: Connect auxiliary source to sources
;; - dag-draw--connect-auxiliary-sink: Connect sinks to auxiliary sink

;;; Code:

(require 'ht)
(require 'cl-lib)
(require 'dag-draw-core)

;;; Auxiliary Node Constants

(defconst dag-draw--aux-source-id 'aux-source
  "Node ID for auxiliary source in network simplex graph.")

(defconst dag-draw--aux-sink-id 'aux-sink
  "Node ID for auxiliary sink in network simplex graph.")

;;; Auxiliary Node Management

(defun dag-draw--cleanup-auxiliary-elements (graph)
  "Remove auxiliary nodes and edges from GRAPH.
This must be called after ranking to clean up temporary GKNV auxiliary structure."
  ;; Find auxiliary elements before removal
  (let ((aux-nodes (dag-draw--find-auxiliary-nodes graph))
        (aux-edges (dag-draw--find-auxiliary-edges graph)))

    ;; Remove auxiliary edges first
    (dolist (edge aux-edges)
      (dag-draw-remove-edge graph 
                           (dag-draw-edge-from-node edge)
                           (dag-draw-edge-to-node edge)))

    ;; Remove auxiliary nodes
    (dolist (node-id aux-nodes)
      (dag-draw-remove-node graph node-id))

    ;; Verify cleanup
    (let ((remaining-aux-nodes (dag-draw--find-auxiliary-nodes graph)))
      (when remaining-aux-nodes
        (error "Failed to clean up auxiliary nodes: %s" remaining-aux-nodes)))))

(defun dag-draw--find-auxiliary-nodes (graph)
  "Find all auxiliary nodes in GRAPH."
  (let ((aux-nodes '()))
    (ht-each (lambda (node-id node)
               (when (dag-draw--is-auxiliary-node-p node-id)
                 (push node-id aux-nodes)))
             (dag-draw-graph-nodes graph))
    aux-nodes))

(defun dag-draw--find-auxiliary-edges (graph)
  "Find all auxiliary edges in GRAPH."
  (let ((aux-edges '()))
    (dolist (edge (dag-draw-graph-edges graph))
      (let ((from-node (dag-draw-edge-from-node edge))
            (to-node (dag-draw-edge-to-node edge)))
        (when (or (dag-draw--is-auxiliary-node-p from-node)
                  (dag-draw--is-auxiliary-node-p to-node))
          (push edge aux-edges))))
    aux-edges))

(defun dag-draw--is-auxiliary-node-p (node-id)
  "Check if NODE-ID is an auxiliary node (S_min or S_max)."
  (or (eq node-id 'dag-draw-s-min)  ; GKNV S_min auxiliary source
      (eq node-id 'dag-draw-s-max)  ; GKNV S_max auxiliary sink
      (eq node-id 'aux-source)  ; Legacy compatibility
      (eq node-id 'aux-sink)))

;;; Rank Assignment with Auxiliary Structure

(defun dag-draw--assign-basic-ranks-with-auxiliary (graph tree-info)
  "Assign basic ranks using auxiliary source and sink from TREE-INFO."
  ;; Get auxiliary nodes from tree construction
  (let ((aux-source (ht-get tree-info 'aux-source))
        (aux-sink (ht-get tree-info 'aux-sink)))

    ;; Set auxiliary source rank to 0
    (when aux-source
      (setf (dag-draw-node-rank (dag-draw-get-node graph aux-source)) 0))

    ;; BFS rank assignment from auxiliary source
    (let ((node-queue (list aux-source))
          (visited (ht-create)))

      ;; BFS from auxiliary source to assign ranks respecting edge weights
      (while node-queue
        (let ((current-node (pop node-queue)))
          (unless (ht-get visited current-node)
            (ht-set! visited current-node t)
            
            ;; Process all outgoing edges
            (dolist (edge (dag-draw-get-edges-from graph current-node))
              (let* ((target-node (dag-draw-edge-to-node edge))
                     (edge-weight (dag-draw-edge-weight edge))
                     (current-rank (dag-draw-node-rank (dag-draw-get-node graph current-node)))
                     (required-rank (+ current-rank edge-weight))
                     (target-node-obj (dag-draw-get-node graph target-node))
                     (existing-rank (dag-draw-node-rank target-node-obj)))
                
                ;; Set rank to maximum of existing and required
                (when (or (null existing-rank) (< existing-rank required-rank))
                  (setf (dag-draw-node-rank target-node-obj) required-rank))
                
                ;; Add to queue for further processing
                (push target-node node-queue)))))))

    ;; Set auxiliary sink rank based on maximum node rank
    (when aux-sink
      (let ((max-rank 0))
        ;; Find maximum rank among non-auxiliary nodes
        (ht-each (lambda (node-id node)
                   (unless (dag-draw--is-auxiliary-node-p node-id)
                     (when (dag-draw-node-rank node)
                       (setq max-rank (max max-rank (dag-draw-node-rank node))))))
                 (dag-draw-graph-nodes graph))
        
        ;; Set auxiliary sink to max + 1
        (setf (dag-draw-node-rank (dag-draw-get-node graph aux-sink)) (1+ max-rank))))))

;;; Auxiliary Graph Construction Helpers

(defun dag-draw--connect-auxiliary-source (aux-graph)
  "Connect auxiliary source to all source nodes in AUX-GRAPH."
  ;; Find all source nodes (nodes with no incoming edges)
  (dolist (node-id (dag-draw-get-node-ids aux-graph))
    (unless (eq node-id dag-draw--aux-source-id)
      (when (null (dag-draw-get-predecessors aux-graph node-id))
        (dag-draw-add-edge aux-graph dag-draw--aux-source-id node-id 1)))))

(defun dag-draw--connect-auxiliary-sink (aux-graph)
  "Connect all sink nodes to auxiliary sink in AUX-GRAPH."
  ;; Find all sink nodes (nodes with no outgoing edges)
  (dolist (node-id (dag-draw-get-node-ids aux-graph))
    (unless (eq node-id dag-draw--aux-sink-id)
      (when (null (dag-draw-get-successors aux-graph node-id))
        (dag-draw-add-edge aux-graph node-id dag-draw--aux-sink-id 1)))))

(provide 'dag-draw-auxiliary)

;;; dag-draw-auxiliary.el ends here