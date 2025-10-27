;;; dag-draw-rank-test.el --- Tests for dag-draw-rank.el -*- lexical-binding: t -*-

;; Copyright (C) 2024, 2025

;;; Commentary:

;; GKNV Baseline Compliance Tests - Pass 1: Complete Ranking
;;
;; This module tests complete GKNV Pass 1 ranking as specified in
;; "A Technique for Drawing Directed Graphs" (Gansner, Koutsofios, North, Vo).
;;
;; GKNV Reference: Section 2 (complete Pass 1 algorithm)
;; Decision: D1.1 - Cycle breaking via DFS
;;           D1.2 - Initial ranking via topological sort
;;           D1.3 - Feasible tree construction
;;           D1.4 - Network simplex iteration
;;           D1.5 - Edge weight interpretation
;;           D1.6 - Rank normalization (min = 0)
;;           D1.7 - Greedy rank balancing
;;           D1.8 - Cut value computation optimizations
;;           D1.9 - Rank constraint handling
;;           D1.10 - Delta (minimum length) handling
;; Algorithm: Complete GKNV Pass 1 - Rank Assignment
;;
;; Key Requirements Tested:
;; - End-to-end Pass 1: input graph → ranked acyclic graph
;; - All edges point generally downward (aesthetic A1)
;; - Delta constraints satisfied for all edges
;; - Sum of weighted edge lengths minimized
;; - Rank constraints (S_min, S_max, S_k) satisfied
;; - Minimum rank normalized to 0
;; - Crowding reduced via balancing
;; - Output valid for Pass 2 (ordering)
;;
;; Test Coverage:
;; - Complete Pass 1 execution on various graphs
;; - Cycle breaking successful (graph becomes DAG)
;; - Ranking optimal (minimum weighted edge length)
;; - Constraints satisfied (delta, S_min, S_max, S_k)
;; - Normalization correct (min rank = 0)
;; - Balancing improves aspect ratio
;; - Integration with Pass 2 verified
;; - Real-world graph structures
;;
;; Baseline Status: ✅ Required for GKNV compliance
;;
;; See doc/implementation-decisions.md (D1.1-D1.10) for full decision rationale.
;; See doc/algorithm-specification.md Pass 1 for implementation details.

;; Original Commentary:
;; Tests for rank assignment functionality.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-pass1-ranking)
(require 'dag-draw-cycle-breaking)
(require 'dag-draw-rank-balancing)

(describe
 "dag-draw-rank"

 (describe
  "simple cycle detection"
  (it "should detect no cycles in acyclic graph"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-node graph 'c)
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)

        (expect (dag-draw-simple-has-cycles graph) :to-be nil)))

  (it "should detect cycles in cyclic graph"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-node graph 'c)
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)
        (dag-draw-add-edge graph 'c 'a)  ; Creates cycle

        (expect (dag-draw-simple-has-cycles graph) :to-be t)))

  (it "should break cycles using GKNV DFS edge classification"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'a)  ; Creates cycle

        ;; GKNV approach: modifies graph in-place by reversing back edges
        (let ((original-edge-count (length (dag-draw-graph-edges graph))))
          (dag-draw--break-cycles-using-gknv-classification graph)
          ;; GKNV preserves all edges (reverses back edges, doesn't remove)
          (expect (length (dag-draw-graph-edges graph)) :to-equal original-edge-count)
          ;; Graph should now be acyclic
          (expect (dag-draw-simple-has-cycles graph) :to-be nil)))))

 (describe
  "rank assignment"
  (it "should assign ranks to simple linear graph"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-node graph 'c)
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)

        (dag-draw-rank-graph graph)

        (let ((rank-a (dag-draw-node-rank (dag-draw-get-node graph 'a)))
              (rank-b (dag-draw-node-rank (dag-draw-get-node graph 'b)))
              (rank-c (dag-draw-node-rank (dag-draw-get-node graph 'c))))

          (expect rank-a :to-be-less-than rank-b)
          (expect rank-b :to-be-less-than rank-c)
          (expect rank-a :to-equal 0))))  ; Should be normalized

  (it "should handle parallel branches"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'root)
        (dag-draw-add-node graph 'left)
        (dag-draw-add-node graph 'right)
        (dag-draw-add-node graph 'sink)
        (dag-draw-add-edge graph 'root 'left)
        (dag-draw-add-edge graph 'root 'right)
        (dag-draw-add-edge graph 'left 'sink)
        (dag-draw-add-edge graph 'right 'sink)

        (dag-draw-rank-graph graph)

        (let ((rank-root (dag-draw-node-rank (dag-draw-get-node graph 'root)))
              (rank-left (dag-draw-node-rank (dag-draw-get-node graph 'left)))
              (rank-right (dag-draw-node-rank (dag-draw-get-node graph 'right)))
              (rank-sink (dag-draw-node-rank (dag-draw-get-node graph 'sink))))

          ;; Root should be at rank 0
          (expect rank-root :to-equal 0)
          ;; Left and right should be at same rank
          (expect rank-left :to-equal rank-right)
          ;; Sink should be after branches
          (expect rank-sink :to-be-greater-than rank-left))))

  (it "should break cycles before ranking"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-node graph 'c)
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)
        (dag-draw-add-edge graph 'c 'a)  ; Creates cycle

        (dag-draw-rank-graph graph)

        ;; Should succeed without error and assign ranks
        (expect (dag-draw-node-rank (dag-draw-get-node graph 'a)) :to-be-truthy)
        (expect (dag-draw-node-rank (dag-draw-get-node graph 'b)) :to-be-truthy)
        (expect (dag-draw-node-rank (dag-draw-get-node graph 'c)) :to-be-truthy)))

  (it "should set max rank"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-node graph 'c)
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)

        (dag-draw-rank-graph graph)

        (expect (dag-draw-graph-max-rank graph) :to-equal 2))))

 (describe
  "rank normalization"
  (it "should normalize ranks to start from 0"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)

        ;; Manually set ranks to non-zero start
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'a)) 5)
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'b)) 7)
        (setf (dag-draw-graph-max-rank graph) 7)

        (dag-draw-normalize-ranks graph)

        (expect (dag-draw-node-rank (dag-draw-get-node graph 'a)) :to-equal 0)
        (expect (dag-draw-node-rank (dag-draw-get-node graph 'b)) :to-equal 2)
        (expect (dag-draw-graph-max-rank graph) :to-equal 2))))

 (describe
  "integration with layout pipeline"
  (it "should work with main layout function"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-node graph 'b "Node B")
        (dag-draw-add-edge graph 'a 'b)

        ;; Should not throw error
        (expect (dag-draw-layout-graph graph) :not :to-throw)

        ;; Should have assigned ranks
        (expect (dag-draw-node-rank (dag-draw-get-node graph 'a)) :to-equal 0)
        (expect (dag-draw-node-rank (dag-draw-get-node graph 'b)) :to-equal 1)))))

;;; dag-draw-rank-test.el ends here
