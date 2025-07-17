;;; dag-draw-real-network-simplex-test.el --- TDD for real network simplex -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; TDD tests to implement actual network simplex optimization,
;; not just the topological fallback currently used.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-pass1-ranking)

(describe "Real Network Simplex Implementation"
  
  (it "should produce different results than topological sorting for weighted graphs"
    ;; RED: This test should fail because we're using topological sorting
    (let ((graph (dag-draw-create-graph)))
      ;; Create a graph where network simplex should produce different ranking
      ;; than topological sort due to edge weights
      (dag-draw-add-node graph 'a "A")
      (dag-draw-add-node graph 'b "B") 
      (dag-draw-add-node graph 'c "C")
      (dag-draw-add-node graph 'd "D")
      
      ;; Add edges with different weights
      ;; High weight path: a->b->d (should be favored by network simplex)
      (dag-draw-add-edge graph 'a 'b 10)  ; high weight
      (dag-draw-add-edge graph 'b 'd 10)  ; high weight
      
      ;; Low weight path: a->c->d (should be less favored)
      (dag-draw-add-edge graph 'a 'c 1)   ; low weight  
      (dag-draw-add-edge graph 'c 'd 1)   ; low weight
      
      ;; Get ranking with current "network simplex" (actually topological)
      (dag-draw--assign-ranks-network-simplex graph)
      
      ;; Get ranking with pure topological sort
      (let ((topo-graph (dag-draw-copy-graph graph)))
        (dag-draw--assign-ranks-topological topo-graph)
        
        ;; They should be DIFFERENT if we have real network simplex
        ;; But currently they're the same because we're using topological
        (let ((simplex-a-rank (dag-draw-node-rank (dag-draw-get-node graph 'a)))
              (simplex-b-rank (dag-draw-node-rank (dag-draw-get-node graph 'b)))
              (simplex-c-rank (dag-draw-node-rank (dag-draw-get-node graph 'c)))
              (simplex-d-rank (dag-draw-node-rank (dag-draw-get-node graph 'd)))
              (topo-a-rank (dag-draw-node-rank (dag-draw-get-node topo-graph 'a)))
              (topo-b-rank (dag-draw-node-rank (dag-draw-get-node topo-graph 'b)))
              (topo-c-rank (dag-draw-node-rank (dag-draw-get-node topo-graph 'c)))
              (topo-d-rank (dag-draw-node-rank (dag-draw-get-node topo-graph 'd))))
          
          ;; This should fail - ranks should be different with real network simplex
          ;; Network simplex should favor the high-weight path a->b->d over a->c->d
          (expect (or (not (= simplex-a-rank topo-a-rank))
                      (not (= simplex-b-rank topo-b-rank))
                      (not (= simplex-c-rank topo-c-rank))
                      (not (= simplex-d-rank topo-d-rank))) :to-be t))))))

(provide 'dag-draw-real-network-simplex-test)

;;; dag-draw-real-network-simplex-test.el ends here