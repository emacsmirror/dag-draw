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



(provide 'dag-draw-algorithms)

;;; dag-draw-algorithms.el ends here