;;; dag-draw-algorithms.el --- Graph algorithms for dag-draw -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Core graph algorithms including cycle detection, DFS, and other
;; fundamental operations needed for the GKNV layout algorithm.

;;; Code:

(require 'dash)
(require 'ht)
(require 'dag-draw)
(require 'dag-draw-core)

;;; DFS and Cycle Detection

(defun dag-draw--dfs-visit (graph node-id visited pre-order post-order 
                                 current-time edge-classification)
  "Internal DFS visit function.
Updates VISITED, PRE-ORDER, POST-ORDER hash tables with visit times.
CURRENT-TIME is passed by reference (vector).
EDGE-CLASSIFICATION is a list container passed by reference."
  (ht-set! visited node-id 'gray)  ; Mark as being processed
  (ht-set! pre-order node-id (aref current-time 0))
  (aset current-time 0 (1+ (aref current-time 0)))
  
  ;; Visit all adjacent nodes
  (dolist (edge (dag-draw-get-edges-from graph node-id))
    (let ((target (dag-draw-edge-to-node edge)))
      (cond
       ;; Tree edge - target not yet visited
       ((eq (ht-get visited target 'white) 'white)
        (push (list edge 'tree) (car edge-classification))
        (dag-draw--dfs-visit graph target visited pre-order post-order
                            current-time edge-classification))
       
       ;; Back edge - target is being processed (creates cycle)
       ((eq (ht-get visited target) 'gray)
        (push (list edge 'back) (car edge-classification)))
       
       ;; Forward or cross edge - target already processed
       ((eq (ht-get visited target) 'black)
        (let ((pre-source (ht-get pre-order node-id))
              (pre-target (ht-get pre-order target)))
          (if (< pre-source pre-target)
              (push (list edge 'forward) (car edge-classification))
            (push (list edge 'cross) (car edge-classification))))))))
  
  ;; Mark as completely processed
  (ht-set! visited node-id 'black)
  (ht-set! post-order node-id (aref current-time 0))
  (aset current-time 0 (1+ (aref current-time 0))))

(defun dag-draw-dfs (graph &optional start-nodes)
  "Perform depth-first search on GRAPH starting from START-NODES.
If START-NODES is nil, starts from all source nodes.
Returns a plist with:
  :visited - hash table mapping node-id to color (white/gray/black)
  :pre-order - hash table mapping node-id to pre-order visit time
  :post-order - hash table mapping node-id to post-order visit time
  :edge-classification - list of (edge type) pairs where type is tree/back/forward/cross"
  (let ((visited (ht-create))
        (pre-order (ht-create))
        (post-order (ht-create))
        (edge-classification (list '()))
        (current-time (vector 0))
        (start-list (or start-nodes (dag-draw-get-source-nodes graph))))
    
    ;; Initialize all nodes as unvisited
    (dolist (node-id (dag-draw-get-node-ids graph))
      (ht-set! visited node-id 'white))
    
    ;; Start DFS from each unvisited start node
    (dolist (start-node start-list)
      (when (eq (ht-get visited start-node) 'white)
        (dag-draw--dfs-visit graph start-node visited pre-order post-order
                            current-time edge-classification)))
    
    ;; Handle any remaining unvisited nodes (disconnected components)
    (dolist (node-id (dag-draw-get-node-ids graph))
      (when (eq (ht-get visited node-id) 'white)
        (dag-draw--dfs-visit graph node-id visited pre-order post-order
                            current-time edge-classification)))
    
    (list :visited visited
          :pre-order pre-order
          :post-order post-order
          :edge-classification (nreverse (car edge-classification)))))

(defun dag-draw-detect-cycles (graph)
  "Detect cycles in GRAPH using DFS.
Returns list of back edges that create cycles."
  (let* ((dfs-result (dag-draw-dfs graph))
         (edge-classification (plist-get dfs-result :edge-classification)))
    (mapcar #'car
            (--filter (eq (cadr it) 'back) edge-classification))))

(defun dag-draw-has-cycles (graph)
  "Return t if GRAPH contains cycles, nil otherwise."
  (not (null (dag-draw-detect-cycles graph))))

(defun dag-draw-break-cycles (graph)
  "Break cycles in GRAPH by reversing back edges.
Returns a new graph with cycles broken. The original graph is not modified."
  (let ((copy (dag-draw-copy-graph graph))
        (back-edges (dag-draw-detect-cycles graph)))
    
    ;; Reverse each back edge to break cycles
    (dolist (back-edge back-edges)
      (let ((from-node (dag-draw-edge-from-node back-edge))
            (to-node (dag-draw-edge-to-node back-edge))
            (weight (dag-draw-edge-weight back-edge))
            (min-length (dag-draw-edge-min-length back-edge))
            (label (dag-draw-edge-label back-edge))
            (attributes (dag-draw-edge-attributes back-edge)))
        
        ;; Remove original edge
        (dag-draw-remove-edge copy from-node to-node)
        
        ;; Add reversed edge with special marking
        (let ((reversed-edge (dag-draw-edge-create
                             :from-node to-node
                             :to-node from-node
                             :weight weight
                             :min-length min-length
                             :label label
                             :attributes (ht-copy attributes))))
          ;; Mark as reversed for later rendering
          (ht-set! (dag-draw-edge-attributes reversed-edge) :reversed t)
          (push reversed-edge (dag-draw-graph-edges copy)))))
    
    copy))

;;; Topological Sorting

(defun dag-draw-topological-sort (graph)
  "Return topological ordering of nodes in GRAPH.
Returns nil if graph contains cycles."
  (when (dag-draw-has-cycles graph)
    (error "Cannot topologically sort graph with cycles"))
  
  (let* ((dfs-result (dag-draw-dfs graph))
         (post-order (plist-get dfs-result :post-order))
         (nodes-with-times '()))
    
    ;; Collect nodes with their post-order times
    (ht-each (lambda (node-id post-time)
               (push (cons node-id post-time) nodes-with-times))
             post-order)
    
    ;; Sort by decreasing post-order time
    (mapcar #'car 
            (sort nodes-with-times 
                  (lambda (a b) (> (cdr a) (cdr b)))))))

;;; Connected Components

(defun dag-draw-strongly-connected-components (graph)
  "Find strongly connected components using Kosaraju's algorithm.
Returns list of lists, where each inner list contains node IDs in one SCC."
  ;; First DFS on original graph
  (let* ((dfs1 (dag-draw-dfs graph))
         (post-order (plist-get dfs1 :post-order))
         (transpose (dag-draw-transpose-graph graph)))
    
    ;; Sort nodes by decreasing post-order time from first DFS
    (let* ((nodes-with-times '())
           (sorted-nodes (progn
                          (ht-each (lambda (node-id post-time)
                                     (push (cons node-id post-time) nodes-with-times))
                                   post-order)
                          (mapcar #'car
                                  (sort nodes-with-times
                                        (lambda (a b) (> (cdr a) (cdr b)))))))
           (visited (ht-create))
           (components '()))
      
      ;; Initialize visited tracking
      (dolist (node-id (dag-draw-get-node-ids graph))
        (ht-set! visited node-id nil))
      
      ;; Second DFS on transposed graph
      (dolist (node-id sorted-nodes)
        (unless (ht-get visited node-id)
          (let ((component '()))
            ;; DFS to find all nodes in this component
            (setq component (dag-draw--collect-component transpose node-id visited))
            (when component
              (push component components)))))
      
      (nreverse components))))

(defun dag-draw--collect-component (graph node-id visited)
  "Helper function to collect nodes in a strongly connected component."
  (let ((component '())
        (stack (list node-id)))
    
    (while stack
      (let ((current (pop stack)))
        (unless (ht-get visited current)
          (ht-set! visited current t)
          (push current component)
          
          ;; Add unvisited successors to stack
          (dolist (successor (dag-draw-get-successors graph current))
            (unless (ht-get visited successor)
              (push successor stack))))))
    
    component))

(defun dag-draw-transpose-graph (graph)
  "Create transpose (edge-reversed) version of GRAPH."
  (let ((transpose (dag-draw-graph-create
                   :node-separation (dag-draw-graph-node-separation graph)
                   :rank-separation (dag-draw-graph-rank-separation graph))))
    
    ;; Copy all nodes
    (ht-each (lambda (node-id node)
               (let ((new-node (dag-draw-node-create
                               :id (dag-draw-node-id node)
                               :label (dag-draw-node-label node)
                               :x-size (dag-draw-node-x-size node)
                               :y-size (dag-draw-node-y-size node)
                               :attributes (ht-copy (dag-draw-node-attributes node)))))
                 (ht-set! (dag-draw-graph-nodes transpose) node-id new-node)))
             (dag-draw-graph-nodes graph))
    
    ;; Reverse all edges
    (dolist (edge (dag-draw-graph-edges graph))
      (let ((reversed-edge (dag-draw-edge-create
                           :from-node (dag-draw-edge-to-node edge)
                           :to-node (dag-draw-edge-from-node edge)
                           :weight (dag-draw-edge-weight edge)
                           :min-length (dag-draw-edge-min-length edge)
                           :label (dag-draw-edge-label edge)
                           :attributes (ht-copy (dag-draw-edge-attributes edge)))))
        (push reversed-edge (dag-draw-graph-edges transpose))))
    
    transpose))

(provide 'dag-draw-algorithms)

;;; dag-draw-algorithms.el ends here