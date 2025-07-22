;;; dag-draw-aesthetic-principles.el --- GKNV Aesthetic Principles A1-A4 -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Claude Code
;; Version: 1.0
;; Keywords: graphs, layout, aesthetics

;;; Commentary:

;; Implementation of GKNV aesthetic principles A1-A4 per Section 1.1.
;; Provides validation and measurement functions for:
;; - A1: Expose hierarchical structure
;; - A2: Avoid visual anomalies 
;; - A3: Keep edges short
;; - A4: Favor symmetry and balance
;;
;; GKNV Reference: Section 1.1, lines 43-54
;; Ubiquitous Language: Aesthetic Principles - proper A1-A4 integration

;;; Code:

(require 'ht)
(require 'dag-draw-core)

;; Forward declarations for existing modules
(declare-function dag-draw--count-total-crossings "dag-draw-pass2-ordering")
(declare-function dag-draw--organize-by-ranks "dag-draw-pass2-ordering")
(declare-function dag-draw-balance-ranks "dag-draw-rank-balancing")

;;; A1: Expose hierarchical structure (Section 1.1, line 43)

(defun dag-draw--validate-hierarchical-structure (graph)
  "Validate A1 compliance: hierarchical structure exposure.
Returns score from 0-1 indicating hierarchical clarity."
  (let* ((nodes (dag-draw-get-node-ids graph))
         (total-edges (length (dag-draw-graph-edges graph)))
         (forward-edges 0))
    
    (if (zerop total-edges)
        1.0  ; Perfect hierarchy with no edges
      ;; Count edges that follow hierarchical flow (rank increase)
      (progn
        (dolist (edge (dag-draw-graph-edges graph))
          (let ((from-node (dag-draw-get-node graph (dag-draw-edge-from-node edge)))
                (to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge))))
            (when (and from-node to-node
                       (dag-draw-node-rank from-node)
                       (dag-draw-node-rank to-node)
                       (< (dag-draw-node-rank from-node)
                          (dag-draw-node-rank to-node)))
              (setq forward-edges (1+ forward-edges)))))
        
        ;; A1 score: proportion of edges following hierarchical flow
        (/ (float forward-edges) total-edges)))))

(defun dag-draw--measure-directional-consistency (graph)
  "Measure A1 directional consistency: edges in same general direction.
Returns score from 0-1 indicating consistency."
  (let* ((edges (dag-draw-graph-edges graph))
         (total-edges (length edges))
         (consistent-edges 0))
    
    (when (zerop total-edges)
      (return 1.0))
    
    ;; Determine primary direction (top-down in this case)
    (dolist (edge edges)
      (let ((from-node (dag-draw-get-node graph (dag-draw-edge-from-node edge)))
            (to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge))))
        (when (and from-node to-node
                   (dag-draw-node-rank from-node)
                   (dag-draw-node-rank to-node))
          ;; Edge is consistent if it goes from lower to higher rank (top-down)
          (when (<= (dag-draw-node-rank from-node)
                    (dag-draw-node-rank to-node))
            (setq consistent-edges (1+ consistent-edges))))))
    
    (/ (float consistent-edges) total-edges)))

(defun dag-draw--identify-source-sink-prominence (graph)
  "Identify source and sink nodes for A1 hierarchical prominence.
Returns hash table with source-count and sink-count."
  (let ((result (ht-create))
        (sources '())
        (sinks '())
        (has-incoming (ht-create))
        (has-outgoing (ht-create)))
    
    ;; Mark nodes with incoming/outgoing edges
    (dolist (edge (dag-draw-graph-edges graph))
      (ht-set! has-incoming (dag-draw-edge-to-node edge) t)
      (ht-set! has-outgoing (dag-draw-edge-from-node edge) t))
    
    ;; Identify sources (no incoming) and sinks (no outgoing)
    (dolist (node-id (dag-draw-get-node-ids graph))
      (unless (ht-get has-incoming node-id)
        (push node-id sources))
      (unless (ht-get has-outgoing node-id)
        (push node-id sinks)))
    
    (ht-set! result 'source-count (length sources))
    (ht-set! result 'sink-count (length sinks))
    (ht-set! result 'sources sources)
    (ht-set! result 'sinks sinks)
    result))

;;; A2: Avoid visual anomalies (Section 1.1, line 47)

(defun dag-draw--count-edge-crossings (graph)
  "Count edge crossings for A2 visual anomaly minimization.
Reuses existing crossing detection from Pass 2 ordering."
  (require 'dag-draw-pass2-ordering)
  
  ;; Use existing crossing count function if nodes have ranks/orders
  (let ((ranks (dag-draw--organize-by-ranks graph)))
    (if ranks
        (dag-draw--count-total-crossings graph ranks)
      ;; Fallback for graphs without ranking
      0)))


(defun dag-draw--analyze-edge-bends (graph)
  "Analyze edge bends for A2 sharp bend avoidance.
Returns hash table with bend metrics."
  (let ((result (ht-create))
        (max-bend-angle 0)
        (total-bends 0))
    
    ;; For each edge, analyze bending characteristics
    (dolist (edge (dag-draw-graph-edges graph))
      (let* ((from-node (dag-draw-get-node graph (dag-draw-edge-from-node edge)))
             (to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge)))
             (bend-angle (dag-draw--calculate-edge-bend graph from-node to-node)))
        
        (when bend-angle
          (setq max-bend-angle (max max-bend-angle bend-angle))
          (when (> bend-angle 30)  ; Consider angles > 30 degrees as bends
            (setq total-bends (1+ total-bends))))))
    
    (ht-set! result 'max-bend-angle max-bend-angle)
    (ht-set! result 'total-bends total-bends)
    result))

(defun dag-draw--calculate-edge-bend (graph from-node to-node)
  "Calculate bend angle for an edge between two nodes."
  (when (and from-node to-node
             (dag-draw-node-x-coord from-node) (dag-draw-node-y-coord from-node)
             (dag-draw-node-x-coord to-node) (dag-draw-node-y-coord to-node))
    
    (let* ((dx (- (dag-draw-node-x-coord to-node) (dag-draw-node-x-coord from-node)))
           (dy (- (dag-draw-node-y-coord to-node) (dag-draw-node-y-coord from-node)))
           (angle (atan dy dx))
           (degrees (* angle (/ 180.0 pi))))
      
      ;; Return absolute deviation from straight vertical (0 degrees)
      (abs degrees))))

;;; A3: Keep edges short (Section 1.1, line 50)

(defun dag-draw--measure-edge-lengths (graph)
  "Measure edge lengths for A3 short edge optimization.
Returns hash table with length metrics."
  (let ((result (ht-create))
        (total-length 0)
        (edge-count 0)
        (lengths '()))
    
    (dolist (edge (dag-draw-graph-edges graph))
      (let* ((from-node (dag-draw-get-node graph (dag-draw-edge-from-node edge)))
             (to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge)))
             (length (dag-draw--calculate-edge-length from-node to-node)))
        
        (when length
          (push length lengths)
          (setq total-length (+ total-length length))
          (setq edge-count (1+ edge-count)))))
    
    (ht-set! result 'average-length (if (zerop edge-count) 0 (/ total-length edge-count)))
    (ht-set! result 'total-length total-length)
    (ht-set! result 'edge-count edge-count)
    (ht-set! result 'max-length (if lengths (apply #'max lengths) 0))
    result))

(defun dag-draw--calculate-edge-length (from-node to-node)
  "Calculate Euclidean distance between two nodes."
  (when (and from-node to-node
             (dag-draw-node-x-coord from-node) (dag-draw-node-y-coord from-node)
             (dag-draw-node-x-coord to-node) (dag-draw-node-y-coord to-node))
    
    (let ((dx (- (dag-draw-node-x-coord to-node) (dag-draw-node-x-coord from-node)))
          (dy (- (dag-draw-node-y-coord to-node) (dag-draw-node-y-coord from-node))))
      (sqrt (+ (* dx dx) (* dy dy))))))

(defun dag-draw--measure-node-clustering (graph)
  "Measure node clustering quality for A3 related node proximity.
Returns clustering score from 0-1."
  (let* ((nodes (dag-draw-get-node-ids graph))
         (total-pairs 0)
         (close-pairs 0))
    
    ;; For each pair of connected nodes, check if they're close
    (dolist (edge (dag-draw-graph-edges graph))
      (let* ((from-node (dag-draw-get-node graph (dag-draw-edge-from-node edge)))
             (to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge)))
             (distance (dag-draw--calculate-edge-length from-node to-node)))
        
        (when distance
          (setq total-pairs (1+ total-pairs))
          ;; Consider "close" if distance is below threshold
          (when (< distance 100)  ; Arbitrary threshold
            (setq close-pairs (1+ close-pairs))))))
    
    (if (zerop total-pairs) 1.0
      (/ (float close-pairs) total-pairs))))

;;; A4: Favor symmetry and balance (Section 1.1, line 53)

(defun dag-draw--measure-layout-symmetry (graph)
  "Measure layout symmetry for A4 aesthetic balance.
Returns symmetry score from 0-1."
  (let* ((nodes (dag-draw-get-node-ids graph))
         (x-positions '())
         (symmetry-score 0))
    
    ;; Collect X positions
    (dolist (node-id nodes)
      (let ((node (dag-draw-get-node graph node-id)))
        (when (and node (dag-draw-node-x-coord node))
          (push (dag-draw-node-x-coord node) x-positions))))
    
    (when x-positions
      (let* ((min-x (apply #'min x-positions))
             (max-x (apply #'max x-positions))
             (center-x (/ (+ min-x max-x) 2.0))
             (balanced-pairs 0)
             (total-checks 0))
        
        ;; Check for symmetric node placement around center
        (dolist (pos x-positions)
          (let ((mirror-pos (+ center-x (- center-x pos))))
            (setq total-checks (1+ total-checks))
            ;; Check if there's a node near the mirror position
            (when (cl-some (lambda (other-pos)
                             (< (abs (- other-pos mirror-pos)) 10))  ; 10 unit tolerance
                           x-positions)
              (setq balanced-pairs (1+ balanced-pairs)))))
        
        (setq symmetry-score (if (zerop total-checks) 1.0
                               (/ (float balanced-pairs) total-checks)))))
    
    symmetry-score))

(defun dag-draw--measure-layout-balance (graph)
  "Measure layout balance for A4 distributed node arrangement.
Returns hash table with balance metrics."
  (let ((result (ht-create))
        (x-positions '())
        (y-positions '()))
    
    ;; Collect node positions
    (dolist (node-id (dag-draw-get-node-ids graph))
      (let ((node (dag-draw-get-node graph node-id)))
        (when node
          (when (dag-draw-node-x-coord node)
            (push (dag-draw-node-x-coord node) x-positions))
          (when (dag-draw-node-y-coord node)
            (push (dag-draw-node-y-coord node) y-positions)))))
    
    ;; Calculate horizontal balance (standard deviation of X positions)
    (let ((h-balance (if (< (length x-positions) 2) 1.0
                       (dag-draw--calculate-position-balance x-positions)))
          (v-balance (if (< (length y-positions) 2) 1.0
                       (dag-draw--calculate-position-balance y-positions))))
      
      (ht-set! result 'horizontal-balance h-balance)
      (ht-set! result 'vertical-balance v-balance)
      (ht-set! result 'overall-balance (/ (+ h-balance v-balance) 2.0)))
    
    result))

(defun dag-draw--calculate-position-balance (positions)
  "Calculate balance score from position distribution.
Lower standard deviation indicates better balance."
  (when (> (length positions) 1)
    (let* ((mean (/ (apply #'+ positions) (length positions)))
           (variance (/ (apply #'+ (mapcar (lambda (pos) 
                                             (expt (- pos mean) 2)) 
                                           positions))
                        (length positions)))
           (std-dev (sqrt variance)))
      
      ;; Convert std-dev to 0-1 score (lower std-dev = higher score)
      ;; Using arbitrary scaling factor
      (max 0.0 (min 1.0 (- 1.0 (/ std-dev 100.0)))))))

;;; Algorithm Integration Functions

(defun dag-draw--evaluate-ranking-aesthetics (graph)
  "Evaluate aesthetic principles in ranking decisions (Pass 1).
Focuses on A1 (hierarchy) and A3 (short edges)."
  (let ((a1-score (dag-draw--validate-hierarchical-structure graph))
        (a3-score (ht-get (dag-draw--measure-edge-lengths graph) 'average-length)))
    
    ;; Return plist with individual scores
    (list :hierarchical-score a1-score
          :edge-length-score (if a3-score (max 0 (min 1 (- 1 (/ a3-score 100)))) 0)
          :overall-score (+ (* 0.7 a1-score)  ; A1 is primary for ranking
                           (* 0.3 (if a3-score (max 0 (min 1 (- 1 (/ a3-score 100)))) 0))))))

(defun dag-draw--evaluate-ordering-aesthetics (graph)
  "Evaluate aesthetic principles in ordering decisions (Pass 2).
Focuses on A2 (crossing minimization)."
  (let ((crossings (or (dag-draw--count-edge-crossings graph) 0))
        (total-edges (length (dag-draw-graph-edges graph))))
    
    ;; Return plist with individual scores
    (list :crossing-count crossings
          :total-edges total-edges
          :crossing-score (if (zerop total-edges) 1.0
                           (max 0.0 (- 1.0 (/ crossings (* total-edges total-edges))))))))

(defun dag-draw--evaluate-positioning-aesthetics (graph)
  "Evaluate aesthetic principles in positioning decisions (Pass 3).
Focuses on A3 (edge length) and A4 (balance)."
  (let* ((a3-metrics (dag-draw--measure-edge-lengths graph))
         (a4-metrics (dag-draw--measure-layout-balance graph))
         (avg-edge-length (or (ht-get a3-metrics 'average-length) 0))
         (symmetry-score (or (ht-get a4-metrics 'symmetry-score) 1.0))
         (a3-score (if (> avg-edge-length 0)
                       (max 0 (min 1 (- 1 (/ avg-edge-length 100))))
                     1.0))
         (a4-score (or (ht-get a4-metrics 'overall-balance) 1.0)))
    
    ;; Return plist with individual scores
    (list :average-edge-length avg-edge-length
          :symmetry-score symmetry-score
          :edge-length-score a3-score
          :balance-score a4-score
          :overall-score (+ (* 0.6 a3-score)  ; A3 is primary for positioning
                           (* 0.4 a4-score)))))

(provide 'dag-draw-aesthetic-principles)

;;; dag-draw-aesthetic-principles.el ends here