;;; dag-draw-advanced-crossing-reduction-test.el --- TDD tests for advanced GKNV crossing reduction -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; TDD Implementation of advanced GKNV crossing reduction with weighted median heuristic.
;; This implements the full weighted median approach described in the research paper.

;;; Code:

(require 'buttercup)
(require 'dag-draw-order-simple)

(describe "Advanced GKNV crossing reduction with weighted median"
  (describe "weighted median calculation"
    (it "should calculate weighted median positions based on edge weights"
      ;; RED phase: This test will fail because weighted median doesn't exist yet
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")
        (dag-draw-add-node graph 'd "D")
        (dag-draw-add-edge graph 'a 'c 2)  ; Higher weight
        (dag-draw-add-edge graph 'b 'd 1)  ; Lower weight
        
        ;; Set up ranks
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'a)) 0)
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'b)) 0)
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'c)) 1)
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'd)) 1)
        
        ;; Set initial orders
        (setf (dag-draw-node-order (dag-draw-get-node graph 'a)) 0)
        (setf (dag-draw-node-order (dag-draw-get-node graph 'b)) 1)
        (setf (dag-draw-node-order (dag-draw-get-node graph 'c)) 0)
        (setf (dag-draw-node-order (dag-draw-get-node graph 'd)) 1)
        
        ;; Apply weighted median heuristic
        (let ((weighted-median (dag-draw--calculate-weighted-median graph 'c)))
          (expect (numberp weighted-median) :to-be t)
          (expect (>= weighted-median 0) :to-be t)))))
  
  (describe "iterative crossing reduction"
    (it "should reduce crossings through multiple iterations"
      ;; RED phase: This test will fail because iterative reduction doesn't exist yet
      (let ((graph (dag-draw-create-graph)))
        ;; Create a graph with potential crossings
        (dag-draw-add-node graph 'x1 "X1")
        (dag-draw-add-node graph 'x2 "X2")
        (dag-draw-add-node graph 'y1 "Y1")
        (dag-draw-add-node graph 'y2 "Y2")
        (dag-draw-add-edge graph 'x1 'y2)  ; These edges cross
        (dag-draw-add-edge graph 'x2 'y1)  ; if ordered badly
        
        ;; Set up ranks
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'x1)) 0)
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'x2)) 0)
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'y1)) 1)
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'y2)) 1)
        
        ;; Set bad initial ordering that creates crossings
        (setf (dag-draw-node-order (dag-draw-get-node graph 'x1)) 0)
        (setf (dag-draw-node-order (dag-draw-get-node graph 'x2)) 1)
        (setf (dag-draw-node-order (dag-draw-get-node graph 'y1)) 1)
        (setf (dag-draw-node-order (dag-draw-get-node graph 'y2)) 0)
        
        ;; Count initial crossings
        (let ((initial-crossings (dag-draw--count-crossings graph 0 1)))
          ;; Apply iterative crossing reduction
          (let ((result (dag-draw--iterative-crossing-reduction graph)))
            (expect (ht-get result 'converged) :to-be t)
            (expect (ht-get result 'final-crossings) :not :to-be nil)
            ;; Final crossings should be <= initial crossings
            (expect (<= (ht-get result 'final-crossings) initial-crossings) :to-be t)))))))
  
  (describe "two-layer crossing minimization"
    (it "should minimize crossings between two specific layers"
      ;; RED phase: This test will fail because two-layer optimization doesn't exist yet
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'top1 "Top1")
        (dag-draw-add-node graph 'top2 "Top2")
        (dag-draw-add-node graph 'bot1 "Bot1")
        (dag-draw-add-node graph 'bot2 "Bot2")
        (dag-draw-add-node graph 'bot3 "Bot3")
        (dag-draw-add-edge graph 'top1 'bot1)
        (dag-draw-add-edge graph 'top1 'bot3)
        (dag-draw-add-edge graph 'top2 'bot2)
        
        ;; Set up ranks
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'top1)) 0)
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'top2)) 0)
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'bot1)) 1)
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'bot2)) 1)
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'bot3)) 1)
        
        ;; Apply two-layer crossing minimization
        (let ((result (dag-draw--optimize-two-layer-crossings graph 0 1)))
          (expect (ht-get result 'optimization-applied) :to-be t)
          (expect (numberp (ht-get result 'final-crossings)) :to-be t)))))

(provide 'dag-draw-advanced-crossing-reduction-test)

;;; dag-draw-advanced-crossing-reduction-test.el ends here