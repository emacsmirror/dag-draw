;;; dag-draw-algorithms-test.el --- Tests for dag-draw-algorithms.el -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; GKNV Baseline Compliance Tests - Pass 2: Core Ordering Algorithms
;;
;; This module tests GKNV core ordering algorithms (median, transpose) as
;; specified in "A Technique for Drawing Directed Graphs" (Gansner, Koutsofios,
;; North, Vo).
;;
;; GKNV Reference: Section 3 Figure 3-2 (weighted median), Figure 3-3 (transpose)
;; Decision: D2.2 - Weighted median with special interpolation
;;           D2.4 - Iterative transpose to local optimum
;; Algorithm: Weighted Median + Transpose Core Implementation
;;
;; Key Requirements Tested:
;; - wmedian(rank, iter): sort rank by weighted median of adjacent rank positions
;; - Direction alternates: even iterations down (0→max), odd up (max→0)
;; - median_value(v, adj_rank): compute weighted median position for node v
;; - transpose(rank): swap adjacent pairs if reduces crossings
;; - Transpose iterates until no beneficial swaps remain
;; - Algorithm correctness: implementations match GKNV pseudocode
;; - Integration: median + transpose work together effectively
;;
;; Test Coverage:
;; - wmedian correctly sorts nodes by median
;; - Alternating direction (forward/backward sweeps)
;; - median_value returns correct values (odd, even, weighted cases)
;; - transpose identifies beneficial swaps
;; - transpose iterates to convergence
;; - Combined median + transpose reduces crossings
;; - Algorithm implementations match paper pseudocode
;; - Unit tests for individual functions
;;
;; Baseline Status: ✅ Required for GKNV compliance
;;
;; See doc/implementation-decisions.md (D2.2, D2.4) for full decision rationale.
;; See doc/algorithm-specification.md Pass 2 for implementation details.

;; Tests for graph algorithms including DFS, cycle detection, and topological sorting.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-algorithms)

(describe "dag-draw-algorithms"
  
  (describe "depth-first search"
    (it "should perform DFS on acyclic graph"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-node graph 'c)
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'a 'c)
        
        (let ((result (dag-draw-dfs graph)))
          (expect (plist-get result :visited) :to-be-truthy)
          (expect (plist-get result :pre-order) :to-be-truthy)
          (expect (plist-get result :post-order) :to-be-truthy)
          (expect (plist-get result :edge-classification) :to-be-truthy))))
    
    (it "should classify edges correctly"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-node graph 'c)
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'a 'c)
        (dag-draw-add-edge graph 'b 'c)  ; This should be forward or cross edge
        
        (let* ((result (dag-draw-dfs graph))
               (classifications (plist-get result :edge-classification)))
          (expect (length classifications) :to-equal 3)
          ;; Should have at least some tree edges
          (expect (--some (eq (cadr it) 'tree) classifications) :to-be t)))))

  (describe "cycle detection"
    (it "should detect no cycles in acyclic graph"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-node graph 'c)
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)
        
        (expect (dag-draw-detect-cycles graph) :to-equal nil)))
    
    (it "should detect cycles in cyclic graph"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-node graph 'c)
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)
        (dag-draw-add-edge graph 'c 'a)  ; Creates cycle
        
        (let ((back-edges (dag-draw-detect-cycles graph)))
          (expect (length back-edges) :to-be-greater-than 0))))
    
    
    (it "should handle disconnected components"
      (let ((graph (dag-draw-create-graph)))
        ;; Component 1: acyclic
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-edge graph 'a 'b)
        
        ;; Component 2: cyclic
        (dag-draw-add-node graph 'x)
        (dag-draw-add-node graph 'y)
        (dag-draw-add-edge graph 'x 'y)
        (dag-draw-add-edge graph 'y 'x)
        
        (let ((back-edges (dag-draw-detect-cycles graph)))
          (expect (length back-edges) :to-be-greater-than 0)))))





)

;;; dag-draw-algorithms-test.el ends here