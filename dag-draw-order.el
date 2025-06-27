;;; dag-draw-order.el --- Vertex ordering for dag-draw -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Implementation of the vertex ordering pass of the GKNV algorithm.
;; This module orders vertices within each rank to minimize edge crossings
;; using the weighted median heuristic and local transposition.

;;; Code:

(require 'dash)
(require 'ht)
(require 'dag-draw)
(require 'dag-draw-core)

;;; Virtual Nodes for Long Edges

(defun dag-draw--create-virtual-nodes (graph)
  "Create virtual nodes for edges spanning multiple ranks.
Returns a new graph with virtual nodes inserted."
  (let ((new-graph (dag-draw-copy-graph graph))
        (virtual-counter 0))
    
    ;; Clear edges - we'll rebuild them with virtual nodes
    (setf (dag-draw-graph-edges new-graph) '())
    
    ;; Process each edge from original graph
    (dolist (edge (dag-draw-graph-edges graph))
      (let* ((from-node (dag-draw-get-node graph (dag-draw-edge-from-node edge)))
             (to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge)))
             (from-rank (dag-draw-node-rank from-node))
             (to-rank (dag-draw-node-rank to-node))
             (rank-span (- to-rank from-rank)))
        
        (if (<= rank-span 1)
            ;; Edge spans only one rank - add directly
            (push edge (dag-draw-graph-edges new-graph))
          
          ;; Edge spans multiple ranks - add virtual nodes
          (let ((prev-node-id (dag-draw-edge-from-node edge)))
            (dotimes (i (1- rank-span))
              (let* ((virtual-id (intern (format "virtual_%d" (incf virtual-counter))))
                     (virtual-rank (+ from-rank i 1))
                     (virtual-node (dag-draw-node-create
                                   :id virtual-id
                                   :label ""
                                   :x-size 1  ; Minimal size
                                   :y-size 1
                                   :rank virtual-rank)))
                
                ;; Add virtual node to graph
                (ht-set! (dag-draw-graph-nodes new-graph) virtual-id virtual-node)
                
                ;; Add edge from previous to virtual
                (let ((virtual-edge (dag-draw-edge-create
                                    :from-node prev-node-id
                                    :to-node virtual-id
                                    :weight (dag-draw-edge-weight edge))))
                  (push virtual-edge (dag-draw-graph-edges new-graph)))
                
                (setq prev-node-id virtual-id)))
            
            ;; Add final edge from last virtual to target
            (let ((final-edge (dag-draw-edge-create
                              :from-node prev-node-id
                              :to-node (dag-draw-edge-to-node edge)
                              :weight (dag-draw-edge-weight edge))))
              (push final-edge (dag-draw-graph-edges new-graph)))))))
    
    new-graph))

;;; Rank Organization

(defun dag-draw--organize-by-ranks (graph)
  "Organize nodes by rank into a list of lists.
Returns a vector where index i contains list of nodes at rank i."
  (let ((max-rank (or (dag-draw-graph-max-rank graph) 0))
        (ranks (make-vector (1+ (or (dag-draw-graph-max-rank graph) 0)) '())))
    
    ;; Group nodes by rank
    (ht-each (lambda (node-id node)
               (let ((rank (or (dag-draw-node-rank node) 0)))
                 (when (<= rank max-rank)
                   (push node-id (aref ranks rank)))))
             (dag-draw-graph-nodes graph))
    
    ;; Reverse each rank list to maintain insertion order
    (dotimes (i (length ranks))
      (setf (aref ranks i) (nreverse (aref ranks i))))
    
    ranks))

;;; Edge Crossing Calculation

(defun dag-draw--count-crossings-between-ranks (graph rank1-nodes rank2-nodes)
  "Count edge crossings between two adjacent ranks."
  (let ((crossings 0)
        (edges-between '()))
    
    ;; Collect edges between the two ranks
    (dolist (from-node rank1-nodes)
      (dolist (edge (dag-draw-get-edges-from graph from-node))
        (when (member (dag-draw-edge-to-node edge) rank2-nodes)
          (push edge edges-between))))
    
    ;; Count crossings between edge pairs
    (let ((edge-list edges-between))
      (while edge-list
        (let ((edge1 (car edge-list)))
          (dolist (edge2 (cdr edge-list))
            (when (dag-draw--edges-cross-p graph edge1 edge2 
                                          rank1-nodes rank2-nodes)
              (setq crossings (1+ crossings))))
          (setq edge-list (cdr edge-list)))))
    
    crossings))

(defun dag-draw--edges-cross-p (graph edge1 edge2 rank1-nodes rank2-nodes)
  "Check if two edges cross given the current node ordering."
  (let ((from1 (dag-draw-edge-from-node edge1))
        (to1 (dag-draw-edge-to-node edge1))
        (from2 (dag-draw-edge-from-node edge2))
        (to2 (dag-draw-edge-to-node edge2)))
    
    (let ((pos1-from (cl-position from1 rank1-nodes))
          (pos1-to (cl-position to1 rank2-nodes))
          (pos2-from (cl-position from2 rank1-nodes))
          (pos2-to (cl-position to2 rank2-nodes)))
      
      ;; Edges cross if relative order is different in the two ranks
      (and pos1-from pos1-to pos2-from pos2-to
           (not (eq (< pos1-from pos2-from) 
                    (< pos1-to pos2-to)))))))

;;; Weighted Median Heuristic

(defun dag-draw--calculate-median-position (graph node-id adjacent-rank-nodes)
  "Calculate median position for a node based on adjacent rank."
  (let ((adjacent-positions '()))
    
    ;; Collect positions of adjacent nodes
    (dolist (edge (dag-draw-get-edges-from graph node-id))
      (let ((target (dag-draw-edge-to-node edge))
            (pos (cl-position target adjacent-rank-nodes)))
        (when pos
          (push pos adjacent-positions))))
    
    (dolist (edge (dag-draw-get-edges-to graph node-id))
      (let ((source (dag-draw-edge-from-node edge))
            (pos (cl-position source adjacent-rank-nodes)))
        (when pos
          (push pos adjacent-positions))))
    
    (if (null adjacent-positions)
        -1.0  ; Special value for nodes with no adjacent connections
      (dag-draw--weighted-median adjacent-positions))))

(defun dag-draw--weighted-median (positions)
  "Calculate weighted median of positions.
This implements the biased median described in the GKNV paper."
  (when (null positions)
    (return-from dag-draw--weighted-median 0.0))
  
  (let ((sorted-pos (sort positions #'<))
        (len (length positions)))
    
    (cond
     ;; Single position
     ((= len 1)
      (float (car sorted-pos)))
     
     ;; Two positions - average
     ((= len 2)
      (/ (+ (car sorted-pos) (cadr sorted-pos)) 2.0))
     
     ;; Odd number - true median
     ((oddp len)
      (float (nth (/ len 2) sorted-pos)))
     
     ;; Even number - weighted average of two middle elements
     (t
      (let* ((mid (/ len 2))
             (left-pos (nth (1- mid) sorted-pos))
             (right-pos (nth mid sorted-pos))
             (left-extent (- left-pos (car sorted-pos)))
             (right-extent (- (car (last sorted-pos)) right-pos)))
        
        ;; Bias toward side with more spread
        (if (= (+ left-extent right-extent) 0)
            (/ (+ left-pos right-pos) 2.0)
          (/ (+ (* right-pos left-extent) (* left-pos right-extent))
             (+ left-extent right-extent))))))))

;;; Node Ordering Within Ranks

(defun dag-draw--order-rank-by-median (graph rank-nodes adjacent-rank-nodes direction)
  "Order nodes in a rank using weighted median heuristic.
DIRECTION is 'down or 'up indicating sweep direction."
  (let ((nodes-with-medians '()))
    
    ;; Calculate median for each node
    (dolist (node-id rank-nodes)
      (let ((median (dag-draw--calculate-median-position 
                     graph node-id adjacent-rank-nodes)))
        (push (cons node-id median) nodes-with-medians)))
    
    ;; Sort by median position
    (let ((sorted-pairs (sort nodes-with-medians 
                             (lambda (a b) (< (cdr a) (cdr b))))))
      
      ;; Handle nodes with no connections (median = -1)
      (let ((connected-nodes '())
            (isolated-nodes '()))
        (dolist (pair sorted-pairs)
          (if (< (cdr pair) 0)
              (push (car pair) isolated-nodes)
            (push (car pair) connected-nodes)))
        
        ;; Return connected nodes first, then isolated
        (append (nreverse connected-nodes) (nreverse isolated-nodes))))))

;;; Local Transposition Optimization

(defun dag-draw--transpose-adjacent (graph ranks rank-idx)
  "Try transposing adjacent nodes within a rank to reduce crossings."
  (let ((rank-nodes (aref ranks rank-idx))
        (improved t)
        (total-improvement 0))
    
    (while improved
      (setq improved nil)
      
      ;; Try swapping each adjacent pair
      (dotimes (i (1- (length rank-nodes)))
        (let ((node1 (nth i rank-nodes))
              (node2 (nth (1+ i) rank-nodes)))
          
          ;; Calculate crossings before swap
          (let ((before-crossings (dag-draw--calculate-local-crossings 
                                  graph ranks rank-idx i)))
            
            ;; Perform swap
            (setf (nth i rank-nodes) node2)
            (setf (nth (1+ i) rank-nodes) node1)
            
            ;; Calculate crossings after swap
            (let ((after-crossings (dag-draw--calculate-local-crossings 
                                   graph ranks rank-idx i)))
              
              (if (< after-crossings before-crossings)
                  ;; Keep the swap
                  (progn
                    (setq improved t)
                    (setq total-improvement (+ total-improvement 
                                              (- before-crossings after-crossings))))
                ;; Revert the swap
                (progn
                  (setf (nth i rank-nodes) node1)
                  (setf (nth (1+ i) rank-nodes) node2)))))))
      
      ;; Update the ranks array
      (setf (aref ranks rank-idx) rank-nodes))
    
    total-improvement))

(defun dag-draw--calculate-local-crossings (graph ranks rank-idx pos)
  "Calculate crossings involving edges from position POS and POS+1 in rank."
  (let ((crossings 0))
    
    ;; Check crossings with previous rank
    (when (> rank-idx 0)
      (setq crossings (+ crossings 
                        (dag-draw--count-crossings-between-ranks 
                         graph
                         (list (nth pos (aref ranks rank-idx))
                               (nth (1+ pos) (aref ranks rank-idx)))
                         (aref ranks (1- rank-idx))))))
    
    ;; Check crossings with next rank
    (when (< rank-idx (1- (length ranks)))
      (setq crossings (+ crossings 
                        (dag-draw--count-crossings-between-ranks 
                         graph
                         (list (nth pos (aref ranks rank-idx))
                               (nth (1+ pos) (aref ranks rank-idx)))
                         (aref ranks (1+ rank-idx))))))
    
    crossings))

;;; Main Ordering Algorithm

(defun dag-draw-order-vertices (graph)
  "Order vertices within ranks to minimize edge crossings.
This is the second pass of the GKNV algorithm."
  (let* ((graph-with-virtuals (dag-draw--create-virtual-nodes graph))
         (ranks (dag-draw--organize-by-ranks graph-with-virtuals))
         (max-iterations 8)
         (best-ranks (copy-sequence ranks))
         (best-crossings most-positive-fixnum))
    
    ;; Initial ordering by DFS or similar
    (dag-draw--initialize-ordering graph-with-virtuals ranks)
    
    ;; Iterative improvement
    (dotimes (iteration max-iterations)
      (let ((forward (= (mod iteration 2) 0)))
        
        ;; Sweep through ranks
        (if forward
            ;; Forward pass (top to bottom)
            (dotimes (r (1- (length ranks)))
              (when (> (length (aref ranks (1+ r))) 1)
                (setf (aref ranks (1+ r))
                      (dag-draw--order-rank-by-median
                       graph-with-virtuals
                       (aref ranks (1+ r))
                       (aref ranks r)
                       'down))))
          
          ;; Backward pass (bottom to top)
          (loop for r from (- (length ranks) 2) downto 0 do
                (when (> (length (aref ranks r)) 1)
                  (setf (aref ranks r)
                        (dag-draw--order-rank-by-median
                         graph-with-virtuals
                         (aref ranks r)
                         (aref ranks (1+ r))
                         'up)))))
        
        ;; Apply local transposition
        (dotimes (r (length ranks))
          (dag-draw--transpose-adjacent graph-with-virtuals ranks r))
        
        ;; Check if this is the best solution so far
        (let ((total-crossings (dag-draw--count-total-crossings graph-with-virtuals ranks)))
          (when (< total-crossings best-crossings)
            (setq best-crossings total-crossings)
            (setq best-ranks (copy-sequence ranks))))))
    
    ;; Apply best ordering back to original graph
    (dag-draw--apply-ordering-to-graph graph best-ranks)
    
    graph))

(defun dag-draw--initialize-ordering (graph ranks)
  "Initialize node ordering within ranks."
  ;; Simple initialization - keep existing order or randomize
  (dotimes (r (length ranks))
    (let ((rank-nodes (aref ranks r)))
      ;; For now, just keep the existing order
      ;; Could implement DFS-based initialization here
      (setf (aref ranks r) rank-nodes))))

(defun dag-draw--count-total-crossings (graph ranks)
  "Count total edge crossings in current ordering."
  (let ((total 0))
    (dotimes (r (1- (length ranks)))
      (setq total (+ total 
                    (dag-draw--count-crossings-between-ranks 
                     graph
                     (aref ranks r)
                     (aref ranks (1+ r))))))
    total))

(defun dag-draw--apply-ordering-to-graph (graph ranks)
  "Apply the computed ordering back to the original graph nodes."
  (dotimes (r (length ranks))
    (let ((rank-nodes (aref ranks r))
          (order 0))
      (dolist (node-id rank-nodes)
        (let ((node (dag-draw-get-node graph node-id)))
          ;; Only set order for non-virtual nodes
          (when (and node (not (string-match "^virtual_" (symbol-name node-id))))
            (setf (dag-draw-node-order node) order)
            (setq order (1+ order))))))))

(provide 'dag-draw-order)

;;; dag-draw-order.el ends here