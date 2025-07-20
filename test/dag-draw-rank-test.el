;;; dag-draw-rank-test.el --- Tests for dag-draw-rank.el -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Tests for rank assignment functionality.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-pass1-ranking)
(require 'dag-draw-cycle-breaking)

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

  (it "should break cycles"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'a)  ; Creates cycle

        (let ((acyclic (dag-draw-simple-break-cycles graph)))
          (expect (dag-draw-simple-has-cycles acyclic) :to-be nil)
          ;; Original should be unchanged
          (expect (dag-draw-simple-has-cycles graph) :to-be t)))))

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
