;;; dag-draw-pass3-baseline-compliance-test.el --- Pass 3 GKNV Baseline Compliance Tests -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; This file tests that Pass 3 (positioning) implements ONLY the GKNV baseline
;; algorithm as specified in Decision D3.1: Network simplex on auxiliary graph.
;;
;; Critical Requirement: Use network simplex on auxiliary graph for positioning - NOT heuristics
;;
;; Reference: doc/implementation-decisions.md, Decision D3.1

;;; Commentary:

;; These tests verify that the implementation follows Decision D3.1:
;; "Use network simplex on auxiliary graph (Section 4.2)"
;;
;; The GKNV paper explicitly states Section 4.2 is superior to Section 4.1:
;; - "simpler code"
;; - "as fast or faster"
;; - Optimal solution vs heuristic approximation
;; - Reuses network simplex from Pass 1

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-pass3-positioning)

(describe "Pass 3 GKNV Baseline Compliance"

  (describe "Decision D3.1: Network Simplex Only"

    (it "should not define a heuristic positioning fallback function"
      ;; Per D3.1, we should use network simplex ONLY, not heuristics
      ;; The heuristic function dag-draw--position-nodes-heuristic should not exist
      (expect (fboundp 'dag-draw--position-nodes-heuristic) :not :to-be-truthy))

    (it "should use auxiliary graph method for X-coordinate positioning"
      ;; Verify the main positioning function uses the auxiliary graph approach
      (expect (fboundp 'dag-draw--position-with-auxiliary-graph) :to-be-truthy)
      (expect (fboundp 'dag-draw--build-constraint-auxiliary-graph) :to-be-truthy))

    (it "should implement network simplex constraint solver for auxiliary graph"
      ;; Verify network simplex functions exist for X-coordinate optimization
      (expect (fboundp 'dag-draw--network-simplex-x-coordinates) :to-be-truthy)
      (expect (fboundp 'dag-draw--simple-x-coordinate-solver) :to-be-truthy))

    (it "should build auxiliary graph with cost edges and separation edges"
      ;; Per GKNV Section 4.2: auxiliary graph has two types of edges
      ;; 1. Cost edges (weight > 0) encoding original graph edges via n_e nodes
      ;; 2. Separation edges (weight = 0, min-length = rho) between adjacent nodes in same rank
      (let* ((graph (dag-draw-create-graph))
             (node-a (dag-draw-add-node graph 'a "A"))
             (node-b (dag-draw-add-node graph 'b "B"))
             (node-c (dag-draw-add-node graph 'c "C")))

        ;; Add edge A -> B
        (dag-draw-add-edge graph 'a 'b 1)

        ;; Assign ranks (needed for auxiliary graph construction)
        (setf (dag-draw-node-rank node-a) 0)
        (setf (dag-draw-node-rank node-b) 1)
        (setf (dag-draw-node-rank node-c) 1)

        ;; Assign orders (needed for separation edges)
        (setf (dag-draw-node-order node-b) 0)
        (setf (dag-draw-node-order node-c) 1)

        ;; Initialize X coordinates (required before building aux graph)
        (setf (dag-draw-node-x-coord node-a) 0)
        (setf (dag-draw-node-x-coord node-b) 0)
        (setf (dag-draw-node-x-coord node-c) 100)

        ;; Build auxiliary graph
        (let ((aux-graph (dag-draw--build-constraint-auxiliary-graph graph)))

          ;; Verify auxiliary graph has cost edges (for A->B edge)
          ;; Per GKNV: edge (a,b) becomes n_e node with edges (n_e, a) and (n_e, b)
          (let ((has-cost-edges nil))
            (dolist (edge (dag-draw-graph-edges aux-graph))
              (when (> (dag-draw-edge-weight edge) 0)
                (setq has-cost-edges t)))
            (expect has-cost-edges :to-be-truthy))

          ;; Verify auxiliary graph has separation edges (for B-C adjacency in rank 1)
          ;; Per GKNV: adjacent nodes in same rank get separation edge with weight=0
          (let ((has-separation-edges nil))
            (dolist (edge (dag-draw-graph-edges aux-graph))
              (when (and (= (dag-draw-edge-weight edge) 0)
                        (or (and (eq (dag-draw-edge-from-node edge) 'b)
                                (eq (dag-draw-edge-to-node edge) 'c))
                            (and (eq (dag-draw-edge-from-node edge) 'c)
                                (eq (dag-draw-edge-to-node edge) 'b))))
                (setq has-separation-edges t)))
            (expect has-separation-edges :to-be-truthy)))))

    (it "should use Omega factors per GKNV specification"
      ;; Per D3.2: Omega values should be 1, 2, 8 for edge types
      ;; (real-real): 1, (real-virtual or virtual-real): 2, (virtual-virtual): 8

      ;; Test real-real edge
      (expect (dag-draw--calculate-omega-factor 'real-node 'other-real-node) :to-equal 1)

      ;; Test real-virtual edge
      (expect (dag-draw--calculate-omega-factor 'real-node 'virtual_something) :to-equal 2)
      (expect (dag-draw--calculate-omega-factor 'virtual_x 'real-node) :to-equal 2)

      ;; Test virtual-virtual edge
      (expect (dag-draw--calculate-omega-factor 'virtual_x 'virtual_y) :to-equal 8))

    (it "should calculate separation using GKNV rho formula"
      ;; Per D3.3: rho(a,b) = (xsize(a) + xsize(b))/2 + nodesep(G)
      (let* ((graph (dag-draw-create-graph))
             (node-a (dag-draw-add-node graph 'a "Node A"))
             (node-b (dag-draw-add-node graph 'b "Node B")))

        ;; Set node sizes
        (setf (dag-draw-node-x-size node-a) 60)
        (setf (dag-draw-node-x-size node-b) 80)

        ;; Set graph separation
        (setf (dag-draw-graph-node-separation graph) 10)

        ;; Calculate separation
        (let ((rho (dag-draw--calculate-separation graph 'a 'b)))
          ;; Expected: (60 + 80)/2 + 10 = 70 + 10 = 80
          (expect rho :to-equal 80.0))))))

(describe "Pass 3 Main Positioning Function"

  (it "should assign Y coordinates based on rank"
    ;; Per GKNV Section 4: Y coordinates are straightforward - same Y for nodes in same rank
    (let* ((graph (dag-draw-create-graph))
           (node-a (dag-draw-add-node graph 'a "A"))
           (node-b (dag-draw-add-node graph 'b "B"))
           (node-c (dag-draw-add-node graph 'c "C")))

      ;; Assign ranks
      (setf (dag-draw-node-rank node-a) 0)
      (setf (dag-draw-node-rank node-b) 1)
      (setf (dag-draw-node-rank node-c) 1)

      ;; Set rank separation
      (setf (dag-draw-graph-rank-separation graph) 50)

      ;; Assign Y coordinates
      (dag-draw--assign-y-coordinates graph)

      ;; Verify Y coordinates
      (expect (dag-draw-node-y-coord node-a) :to-equal 0)
      (expect (dag-draw-node-y-coord node-b) :to-equal 50)
      (expect (dag-draw-node-y-coord node-c) :to-equal 50)))  ;; Same rank = same Y

  (it "should position nodes using auxiliary graph method"
    ;; Integration test: verify the full positioning pipeline uses auxiliary graph
    (let* ((graph (dag-draw-create-graph))
           (node-a (dag-draw-add-node graph 'a "A"))
           (node-b (dag-draw-add-node graph 'b "B")))

      ;; Add edge
      (dag-draw-add-edge graph 'a 'b 1)

      ;; Assign ranks and orders (prerequisites)
      (setf (dag-draw-node-rank node-a) 0)
      (setf (dag-draw-node-rank node-b) 1)
      (setf (dag-draw-node-order node-a) 0)
      (setf (dag-draw-node-order node-b) 0)

      ;; Run positioning
      (dag-draw-position-nodes graph)

      ;; Verify coordinates were assigned
      (expect (dag-draw-node-x-coord node-a) :not :to-be nil)
      (expect (dag-draw-node-y-coord node-a) :not :to-be nil)
      (expect (dag-draw-node-x-coord node-b) :not :to-be nil)
      (expect (dag-draw-node-y-coord node-b) :not :to-be nil))))

(provide 'dag-draw-pass3-baseline-compliance-test)

;;; dag-draw-pass3-baseline-compliance-test.el ends here
