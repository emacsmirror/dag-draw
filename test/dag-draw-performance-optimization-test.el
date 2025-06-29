;;; dag-draw-performance-optimization-test.el --- TDD tests for performance optimizations -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; TDD Implementation of performance optimizations and edge case handling.
;; This implements caching, memoization, and large graph handling optimizations.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-optimization)

(describe "Performance optimizations and edge case handling"
  (describe "graph size and complexity analysis"
    (it "should analyze graph complexity for optimization decisions"
      ;; RED phase: This test will fail because complexity analysis doesn't exist yet
      (let ((graph (dag-draw-create-graph)))
        ;; Create a moderately complex graph
        (dotimes (i 10)
          (dag-draw-add-node graph (intern (format "node%d" i)) (format "Node %d" i)))
        (dotimes (i 9)
          (dag-draw-add-edge graph (intern (format "node%d" i)) (intern (format "node%d" (1+ i)))))
        
        ;; Add some cross-connections
        (dag-draw-add-edge graph 'node0 'node5)
        (dag-draw-add-edge graph 'node2 'node7)
        
        ;; Analyze complexity
        (let ((analysis (dag-draw--analyze-graph-complexity graph)))
          (expect (ht-get analysis 'node-count) :to-be 10)
          (expect (ht-get analysis 'edge-count) :to-be 11)
          (expect (ht-get analysis 'complexity-level) :not :to-be nil)
          (expect (member (ht-get analysis 'complexity-level) '(low medium high)) :not :to-be nil)))))
  
  (describe "layout computation caching and memoization"
    (it "should cache expensive computations for repeated layout operations"
      ;; RED phase: This test will fail because caching doesn't exist yet
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)
        
        ;; First layout should populate cache
        (let ((start-time (current-time)))
          (dag-draw-layout-graph graph)
          (let ((first-time (time-subtract (current-time) start-time)))
            
            ;; Second layout should be faster due to caching
            (setq start-time (current-time))
            (dag-draw-layout-graph graph)
            (let ((second-time (time-subtract (current-time) start-time))
                  (cache-info (dag-draw--get-layout-cache-info graph)))
              
              (expect (ht-get cache-info 'cache-hits) :to-be-greater-than 0)
              (expect (ht-get cache-info 'cache-enabled) :to-be t)))))))
  
  (describe "large graph optimization strategies"
    (it "should apply different algorithms based on graph size"
      ;; RED phase: This test will fail because size-based optimization doesn't exist yet
      (let ((small-graph (dag-draw-create-graph))
            (large-graph (dag-draw-create-graph)))
        
        ;; Small graph (10 nodes)
        (dotimes (i 10)
          (dag-draw-add-node small-graph (intern (format "s%d" i)) (format "Small %d" i)))
        
        ;; Large graph (100 nodes)
        (dotimes (i 100)
          (dag-draw-add-node large-graph (intern (format "l%d" i)) (format "Large %d" i)))
        
        ;; Apply adaptive optimization
        (let ((small-strategy (dag-draw--select-optimization-strategy small-graph))
              (large-strategy (dag-draw--select-optimization-strategy large-graph)))
          
          (expect (eq small-strategy 'full-optimization) :to-be t)
          (expect (eq large-strategy 'performance-optimization) :to-be t)
          
          ;; Verify strategies produce valid results
          (expect (dag-draw--strategy-is-valid-p small-strategy) :to-be t)
          (expect (dag-draw--strategy-is-valid-p large-strategy) :to-be t))))))

(provide 'dag-draw-performance-optimization-test)

;;; dag-draw-performance-optimization-test.el ends here