;;; dag-draw-backtrack.el --- Backtracking mechanisms for GKNV algorithm -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Implementation of backtracking mechanisms for the GKNV algorithm.
;; This module provides Pass 3→Pass 2 backtracking for overlap resolution
;; and Pass 4→Pass 2/3 backtracking for spline quality improvement.
;; These mechanisms enable iterative refinement to achieve high-quality layouts.

;;; Code:

(require 'dash)
(require 'ht)
(require 'dag-draw-core)
(require 'dag-draw-quality)

;;; Backtracking State Management

(cl-defstruct (dag-draw-backtrack-state
               (:constructor dag-draw-backtrack-state-create)
               (:copier nil))
  "State information for backtracking operations."
  (iteration-count 0)           ; Current iteration number
  (max-iterations 10)           ; Maximum iterations before giving up
  (best-metrics nil)            ; Best quality metrics achieved so far
  (best-state nil)              ; Best layout state (node positions/orders)
  (previous-attempts nil)       ; History of previous attempts
  (convergence-threshold 0.1)   ; Quality improvement threshold
  (stagnation-count 0)          ; Number of iterations without improvement
  (max-stagnation 3))           ; Maximum stagnation before termination

;;; Pass 3 → Pass 2 Backtracking (Overlap Resolution)

(defun dag-draw-backtrack-pass3-overlaps (graph)
  "Detect overlaps after Pass 3 and trigger Pass 2 backtracking if needed.
Returns updated graph with resolved overlaps or nil if resolution failed."
  (let* ((initial-metrics (dag-draw-quality-assess-layout graph))
         (backtrack-state (dag-draw-backtrack-state-create
                          :max-iterations 5
                          :best-metrics initial-metrics)))
    
    ;; Check if backtracking is needed
    (if (dag-draw-quality-layout-needs-refinement-p 
         initial-metrics 
         '(:max-overlaps 0 :max-crossings 15))
        (prog1 (dag-draw-backtrack--resolve-overlaps graph backtrack-state)
          (message "BACKTRACK: Pass 3→Pass 2 overlap resolution completed"))
      ;; No backtracking needed
      (progn
        (message "BACKTRACK: Pass 3 layout quality acceptable, no backtracking needed")
        graph))))

(defun dag-draw-backtrack--resolve-overlaps (graph backtrack-state)
  "Resolve overlaps through iterative Pass 2 re-execution."
  (let ((current-graph (dag-draw-backtrack--save-graph-state graph))
        (improvement-found t))
    
    (while (and improvement-found
                (< (dag-draw-backtrack-state-iteration-count backtrack-state)
                   (dag-draw-backtrack-state-max-iterations backtrack-state)))
      
      (setf (dag-draw-backtrack-state-iteration-count backtrack-state)
            (1+ (dag-draw-backtrack-state-iteration-count backtrack-state)))
      
      (message "BACKTRACK: Pass 3→Pass 2 iteration %d" 
               (dag-draw-backtrack-state-iteration-count backtrack-state))
      
      ;; Try different vertex ordering strategies
      (let ((candidate-graph (dag-draw-backtrack--try-vertex-reordering 
                             current-graph backtrack-state)))
        
        (if candidate-graph
            ;; Re-run Pass 3 with new ordering
            (progn
              ;; Apply coordinate assignment with new ordering
              (dag-draw-position-nodes candidate-graph)
              
              ;; Evaluate the new layout
              (let ((new-metrics (dag-draw-quality-assess-layout candidate-graph)))
                (if (dag-draw-backtrack--is-improvement-p 
                     new-metrics 
                     (dag-draw-backtrack-state-best-metrics backtrack-state))
                    (progn
                      (message "BACKTRACK: Improvement found - node overlaps: %d→%d, crossings: %d→%d"
                               (dag-draw-quality-metrics-node-overlaps 
                                (dag-draw-backtrack-state-best-metrics backtrack-state))
                               (dag-draw-quality-metrics-node-overlaps new-metrics)
                               (dag-draw-quality-metrics-edge-crossings 
                                (dag-draw-backtrack-state-best-metrics backtrack-state))
                               (dag-draw-quality-metrics-edge-crossings new-metrics))
                      
                      ;; Update best state
                      (setf (dag-draw-backtrack-state-best-metrics backtrack-state) new-metrics)
                      (setf (dag-draw-backtrack-state-best-state backtrack-state) 
                            (dag-draw-backtrack--save-graph-state candidate-graph))
                      (setf (dag-draw-backtrack-state-stagnation-count backtrack-state) 0)
                      (setq current-graph candidate-graph))
                  
                  ;; No improvement - increment stagnation counter
                  (setf (dag-draw-backtrack-state-stagnation-count backtrack-state)
                        (1+ (dag-draw-backtrack-state-stagnation-count backtrack-state)))
                  
                  (when (>= (dag-draw-backtrack-state-stagnation-count backtrack-state)
                            (dag-draw-backtrack-state-max-stagnation backtrack-state))
                    (message "BACKTRACK: Stagnation detected, terminating backtracking")
                    (setq improvement-found nil)))))
          
          ;; No more vertex ordering strategies available
          (progn
            (message "BACKTRACK: No more vertex ordering strategies available")
            (setq improvement-found nil)))))
    
    ;; Return the best graph state found
    (or (dag-draw-backtrack-state-best-state backtrack-state) current-graph)))

(defun dag-draw-backtrack--try-vertex-reordering (graph backtrack-state)
  "Try different vertex ordering strategies for overlap resolution."
  (let* ((iteration (dag-draw-backtrack-state-iteration-count backtrack-state))
         (strategy (cond
                   ((= iteration 1) 'barycenter-reverse)
                   ((= iteration 2) 'median-heuristic)
                   ((= iteration 3) 'size-aware-ordering)
                   ((= iteration 4) 'random-permutation)
                   (t 'adaptive-spacing))))
    
    (message "BACKTRACK: Trying vertex ordering strategy: %s" strategy)
    
    (let ((candidate-graph (dag-draw-backtrack--save-graph-state graph)))
      ;; Apply the selected strategy
      (cond
       ((eq strategy 'barycenter-reverse)
        (dag-draw-backtrack--apply-reverse-barycenter candidate-graph))
       
       ((eq strategy 'median-heuristic)
        (dag-draw-backtrack--apply-median-heuristic candidate-graph))
       
       ((eq strategy 'size-aware-ordering)
        (dag-draw-backtrack--apply-size-aware-ordering candidate-graph))
       
       ((eq strategy 'random-permutation)
        (dag-draw-backtrack--apply-random-permutation candidate-graph))
       
       ((eq strategy 'adaptive-spacing)
        (dag-draw-backtrack--apply-adaptive-spacing candidate-graph))
       
       (t candidate-graph)))))

(defun dag-draw-backtrack--apply-reverse-barycenter (graph)
  "Apply reverse barycenter ordering to reduce overlaps."
  ;; Simple implementation: reverse the order within each rank
  (let ((ranks (ht-create)))
    ;; Group nodes by rank
    (ht-each (lambda (node-id node)
               (let ((rank (or (dag-draw-node-rank node) 0)))
                 (ht-set! ranks rank 
                         (cons node (ht-get ranks rank '())))))
             (dag-draw-graph-nodes graph))
    
    ;; Reverse order within each rank
    (ht-each (lambda (rank nodes)
               (let ((reversed-nodes (reverse nodes)))
                 (dotimes (i (length reversed-nodes))
                   (setf (dag-draw-node-order (nth i reversed-nodes)) i))))
             ranks))
  
  graph)

(defun dag-draw-backtrack--apply-median-heuristic (graph)
  "Apply median heuristic for vertex ordering."
  ;; Simplified implementation: sort by median neighbor position
  (let ((ranks (ht-create)))
    ;; Group nodes by rank
    (ht-each (lambda (node-id node)
               (let ((rank (or (dag-draw-node-rank node) 0)))
                 (ht-set! ranks rank 
                         (cons node (ht-get ranks rank '())))))
             (dag-draw-graph-nodes graph))
    
    ;; Sort each rank by median neighbor position
    (ht-each (lambda (rank nodes)
               (let ((sorted-nodes 
                      (sort nodes 
                            (lambda (a b)
                              (< (dag-draw-backtrack--calculate-median-neighbor-position graph a)
                                 (dag-draw-backtrack--calculate-median-neighbor-position graph b))))))
                 (dotimes (i (length sorted-nodes))
                   (setf (dag-draw-node-order (nth i sorted-nodes)) i))))
             ranks))
  
  graph)

(defun dag-draw-backtrack--calculate-median-neighbor-position (graph node)
  "Calculate median position of node's neighbors."
  (let ((neighbor-positions '())
        (node-id (dag-draw-node-id node)))
    
    ;; Collect positions of all neighbors
    (dolist (edge (dag-draw-graph-edges graph))
      (cond
       ((eq (dag-draw-edge-from-node edge) node-id)
        (let ((neighbor (dag-draw-get-node graph (dag-draw-edge-to-node edge))))
          (when neighbor
            (push (or (dag-draw-node-order neighbor) 0) neighbor-positions))))
       
       ((eq (dag-draw-edge-to-node edge) node-id)
        (let ((neighbor (dag-draw-get-node graph (dag-draw-edge-from-node edge))))
          (when neighbor
            (push (or (dag-draw-node-order neighbor) 0) neighbor-positions))))))
    
    ;; Return median position
    (if neighbor-positions
        (let ((sorted-positions (sort neighbor-positions #'<))
              (len (length neighbor-positions)))
          (if (= (% len 2) 0)
              (/ (+ (nth (/ len 2) sorted-positions)
                    (nth (1- (/ len 2)) sorted-positions)) 2.0)
            (nth (/ len 2) sorted-positions)))
      0)))

(defun dag-draw-backtrack--apply-size-aware-ordering (graph)
  "Apply size-aware vertex ordering to reduce overlaps."
  (let ((ranks (ht-create)))
    ;; Group nodes by rank
    (ht-each (lambda (node-id node)
               (let ((rank (or (dag-draw-node-rank node) 0)))
                 (ht-set! ranks rank 
                         (cons node (ht-get ranks rank '())))))
             (dag-draw-graph-nodes graph))
    
    ;; Sort each rank by node size (smaller nodes first)
    (ht-each (lambda (rank nodes)
               (let ((sorted-nodes 
                      (sort nodes 
                            (lambda (a b)
                              (< (* (dag-draw-node-x-size a) (dag-draw-node-y-size a))
                                 (* (dag-draw-node-x-size b) (dag-draw-node-y-size b)))))))
                 (dotimes (i (length sorted-nodes))
                   (setf (dag-draw-node-order (nth i sorted-nodes)) i))))
             ranks))
  
  graph)

(defun dag-draw-backtrack--apply-random-permutation (graph)
  "Apply random permutation to vertex ordering."
  (let ((ranks (ht-create)))
    ;; Group nodes by rank
    (ht-each (lambda (node-id node)
               (let ((rank (or (dag-draw-node-rank node) 0)))
                 (ht-set! ranks rank 
                         (cons node (ht-get ranks rank '())))))
             (dag-draw-graph-nodes graph))
    
    ;; Randomly shuffle nodes within each rank
    (ht-each (lambda (rank nodes)
               (let ((shuffled-nodes (dag-draw-backtrack--shuffle-list nodes)))
                 (dotimes (i (length shuffled-nodes))
                   (setf (dag-draw-node-order (nth i shuffled-nodes)) i))))
             ranks))
  
  graph)

(defun dag-draw-backtrack--shuffle-list (list)
  "Shuffle a list randomly."
  (let ((result (copy-sequence list)))
    (dotimes (i (length result))
      (let ((j (random (length result))))
        (cl-rotatef (nth i result) (nth j result))))
    result))

(defun dag-draw-backtrack--apply-adaptive-spacing (graph)
  "Apply adaptive spacing adjustments to reduce overlaps."
  ;; Simple implementation: increase node separation
  (setf (dag-draw-graph-node-separation graph) 
        (* (dag-draw-graph-node-separation graph) 1.2))
  
  graph)

;;; Pass 4 → Pass 2/3 Backtracking (Spline Quality)

(defun dag-draw-backtrack-pass4-quality (graph)
  "Assess spline quality after Pass 4 and trigger backtracking if needed."
  (let* ((initial-metrics (dag-draw-quality-assess-layout graph))
         (backtrack-state (dag-draw-backtrack-state-create
                          :max-iterations 3
                          :best-metrics initial-metrics)))
    
    ;; Check if spline quality is acceptable
    (if (dag-draw-quality-layout-needs-refinement-p 
         initial-metrics 
         '(:min-aesthetic-score 0.5 :max-crossings 20))
        (prog1 (dag-draw-backtrack--improve-spline-quality graph backtrack-state)
          (message "BACKTRACK: Pass 4→Pass 2/3 spline quality improvement completed"))
      ;; No backtracking needed
      (progn
        (message "BACKTRACK: Pass 4 spline quality acceptable, no backtracking needed")
        graph))))

(defun dag-draw-backtrack--improve-spline-quality (graph backtrack-state)
  "Improve spline quality through selective backtracking."
  (let ((current-graph (dag-draw-backtrack--save-graph-state graph))
        (improvement-found t))
    
    (while (and improvement-found
                (< (dag-draw-backtrack-state-iteration-count backtrack-state)
                   (dag-draw-backtrack-state-max-iterations backtrack-state)))
      
      (setf (dag-draw-backtrack-state-iteration-count backtrack-state)
            (1+ (dag-draw-backtrack-state-iteration-count backtrack-state)))
      
      (message "BACKTRACK: Pass 4→Pass 2/3 iteration %d" 
               (dag-draw-backtrack-state-iteration-count backtrack-state))
      
      ;; Try different improvement strategies
      (let ((candidate-graph (dag-draw-backtrack--try-spline-improvement 
                             current-graph backtrack-state)))
        
        (if candidate-graph
            (progn
              ;; Re-run Pass 3 and 4 with improvements
              (dag-draw-position-nodes candidate-graph)
              (dag-draw-generate-splines candidate-graph)
              
              ;; Evaluate the new layout
              (let ((new-metrics (dag-draw-quality-assess-layout candidate-graph)))
                (if (dag-draw-backtrack--is-improvement-p 
                     new-metrics 
                     (dag-draw-backtrack-state-best-metrics backtrack-state))
                    (progn
                      (message "BACKTRACK: Spline quality improvement found")
                      (setf (dag-draw-backtrack-state-best-metrics backtrack-state) new-metrics)
                      (setf (dag-draw-backtrack-state-best-state backtrack-state) 
                            (dag-draw-backtrack--save-graph-state candidate-graph))
                      (setq current-graph candidate-graph))
                  
                  ;; No improvement
                  (setq improvement-found nil))))
          
          ;; No more improvement strategies available
          (setq improvement-found nil))))
    
    ;; Return the best graph state found
    (or (dag-draw-backtrack-state-best-state backtrack-state) current-graph)))

(defun dag-draw-backtrack--try-spline-improvement (graph backtrack-state)
  "Try different spline improvement strategies."
  (let* ((iteration (dag-draw-backtrack-state-iteration-count backtrack-state))
         (strategy (cond
                   ((= iteration 1) 'increase-rank-separation)
                   ((= iteration 2) 'reorder-high-crossing-ranks)
                   (t 'adjust-node-positions))))
    
    (message "BACKTRACK: Trying spline improvement strategy: %s" strategy)
    
    (let ((candidate-graph (dag-draw-backtrack--save-graph-state graph)))
      ;; Apply the selected strategy
      (cond
       ((eq strategy 'increase-rank-separation)
        (setf (dag-draw-graph-rank-separation candidate-graph) 
              (* (dag-draw-graph-rank-separation candidate-graph) 1.3)))
       
       ((eq strategy 'reorder-high-crossing-ranks)
        (dag-draw-backtrack--reorder-high-crossing-ranks candidate-graph))
       
       ((eq strategy 'adjust-node-positions)
        (dag-draw-backtrack--adjust-node-positions candidate-graph)))
      
      candidate-graph)))

(defun dag-draw-backtrack--reorder-high-crossing-ranks (graph)
  "Reorder nodes in ranks with high crossing counts."
  ;; Simplified implementation: reverse order in ranks with crossings
  (let ((edge-crossings (dag-draw-quality-count-edge-crossings graph)))
    (when (> edge-crossings 0)
      (dag-draw-backtrack--apply-reverse-barycenter graph))))

(defun dag-draw-backtrack--adjust-node-positions (graph)
  "Make small adjustments to node positions to improve spline quality."
  ;; Simple implementation: add small random perturbations
  (ht-each (lambda (node-id node)
             (let ((current-x (or (dag-draw-node-x-coord node) 0))
                   (current-y (or (dag-draw-node-y-coord node) 0))
                   (perturbation-x (* (- (random 21) 10) 0.1))  ; ±1.0 range
                   (perturbation-y (* (- (random 21) 10) 0.1))) ; ±1.0 range
               (setf (dag-draw-node-x-coord node) (+ current-x perturbation-x))
               (setf (dag-draw-node-y-coord node) (+ current-y perturbation-y))))
           (dag-draw-graph-nodes graph)))

;;; Utility Functions

(defun dag-draw-backtrack--save-graph-state (graph)
  "Save the current state of the graph for backtracking."
  ;; Create a deep copy of the graph structure
  (let ((new-graph (dag-draw-create-graph)))
    ;; Copy graph-level properties
    (setf (dag-draw-graph-node-separation new-graph) 
          (dag-draw-graph-node-separation graph))
    (setf (dag-draw-graph-rank-separation new-graph) 
          (dag-draw-graph-rank-separation graph))
    (setf (dag-draw-graph-max-rank new-graph) 
          (dag-draw-graph-max-rank graph))
    
    ;; Copy nodes
    (ht-each (lambda (node-id node)
               (let ((new-node (dag-draw-node-create 
                               :id (dag-draw-node-id node)
                               :label (dag-draw-node-label node))))
                 (setf (dag-draw-node-rank new-node) (dag-draw-node-rank node))
                 (setf (dag-draw-node-order new-node) (dag-draw-node-order node))
                 (setf (dag-draw-node-x-coord new-node) (dag-draw-node-x-coord node))
                 (setf (dag-draw-node-y-coord new-node) (dag-draw-node-y-coord node))
                 (setf (dag-draw-node-x-size new-node) (dag-draw-node-x-size node))
                 (setf (dag-draw-node-y-size new-node) (dag-draw-node-y-size node))
                 (dag-draw-add-node new-graph (dag-draw-node-id new-node) 
                                   (dag-draw-node-label new-node))))
             (dag-draw-graph-nodes graph))
    
    ;; Copy edges
    (dolist (edge (dag-draw-graph-edges graph))
      (dag-draw-add-edge new-graph 
                        (dag-draw-edge-from-node edge) 
                        (dag-draw-edge-to-node edge)))
    
    new-graph))

(defun dag-draw-backtrack--is-improvement-p (new-metrics old-metrics)
  "Check if new metrics represent an improvement over old metrics."
  (when (and new-metrics old-metrics)
    (let ((new-score (dag-draw-quality--calculate-overall-score new-metrics))
          (old-score (dag-draw-quality--calculate-overall-score old-metrics)))
      (> new-score old-score))))

(provide 'dag-draw-backtrack)

;;; dag-draw-backtrack.el ends here