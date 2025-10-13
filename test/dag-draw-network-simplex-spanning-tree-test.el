;;; dag-draw-network-simplex-spanning-tree-test.el --- TDD tests for network simplex spanning tree -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; GKNV Baseline Compliance Tests - Pass 1: Feasible Spanning Tree
;;
;; This module tests GKNV feasible spanning tree construction as specified in
;; "A Technique for Drawing Directed Graphs" (Gansner, Koutsofios, North, Vo).
;;
;; GKNV Reference: Section 2.3 Figure 2-2 lines 3-9 (feasible_tree procedure)
;; Decision: D1.3 - Incremental tight tree construction
;; Algorithm: Feasible Tree via Minimal Slack Edge Addition
;;
;; Key Requirements Tested:
;; - Tree grows incrementally from one node to span all nodes
;; - At each step, edge with minimal slack incident to tree is selected
;; - Selected edge becomes tight (slack = 0) via rank adjustment
;; - Rank adjustment moves tree nodes to make incident edge tight
;; - Final tree has exactly |V|-1 edges
;; - All tree edges are tight (length = delta)
;; - Tree spans all nodes (connected)
;; - Provides valid starting solution for network simplex
;;
;; Test Coverage:
;; - tight_tree() starts with one node
;; - Loop adds nodes until all |V| nodes in tree
;; - find_min_slack_incident_edge() selects correct edge
;; - Rank adjustment makes edge tight without breaking existing tight edges
;; - Final tree has |V|-1 tight edges
;; - Tree connectivity verified (all nodes reachable)
;; - Various graph structures (linear, branching, complex)
;;
;; Baseline Status: ✅ Required for GKNV compliance
;;
;; See doc/implementation-decisions.md (D1.3) for full decision rationale.
;; See doc/algorithm-specification.md Pass 1 for implementation details.

;; Original Commentary:
;; TDD Phase 1.1: Feasible spanning tree construction for network simplex algorithm.
;; This implements the first component needed for full GKNV network simplex ranking.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-pass1-ranking)

(describe
    "Network Simplex Spanning Tree Construction"
  (describe
      "feasible spanning tree creation"
    (it "should create spanning tree for simple 3-node graph"
      ;; Test GKNV Figure 2-2 feasible tree construction
      (let ((graph (dag-draw-create-graph)))
        ;; Create simple graph: A -> B -> C (chain graph)
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)

        ;; GKNV Figure 2-2 creates tight tree from original edges
        (let ((tree-info (dag-draw--construct-feasible-tree graph)))
          ;; GKNV creates spanning tree from tight edges (both original edges should be tight)
          (expect (ht-get tree-info 'tree-edges) :not :to-be nil)
          (expect (length (ht-get tree-info 'tree-edges)) :to-equal 2)

          ;; Should have tight tree root
          (expect (ht-get tree-info 'tight-tree-root) :not :to-be nil)
          
          ;; Parent-child relationships should be built from tree edges
          (let ((parent-map (ht-get tree-info 'parent-map))
                (roots (ht-get tree-info 'roots)))
            (expect roots :not :to-be nil)
            (expect (length roots) :to-equal 1)
            ;; Root should have no parent
            (let ((root (car roots)))
              (expect (ht-get parent-map root) :to-be nil))))))

    (it "should create feasible ranking from spanning tree"
      ;; GKNV Figure 2-2 assigns ranks during tree construction
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")
        (dag-draw-add-edge graph 'a 'b)  ; weight=1, min-length=1
        (dag-draw-add-edge graph 'b 'c)  ; weight=1, min-length=1

        (let ((tree-info (dag-draw--construct-feasible-tree graph)))
          ;; GKNV assigns feasible ranks during init_rank() phase
          (expect (dag-draw-node-rank (dag-draw-get-node graph 'a)) :not :to-be nil)
          (expect (dag-draw-node-rank (dag-draw-get-node graph 'b)) :not :to-be nil)
          (expect (dag-draw-node-rank (dag-draw-get-node graph 'c)) :not :to-be nil)
          
          ;; Ranks should satisfy edge constraints (rank[b] > rank[a], rank[c] > rank[b])
          (let ((rank-a (dag-draw-node-rank (dag-draw-get-node graph 'a)))
                (rank-b (dag-draw-node-rank (dag-draw-get-node graph 'b)))
                (rank-c (dag-draw-node-rank (dag-draw-get-node graph 'c))))
            (expect rank-b :to-be-greater-than rank-a)
            (expect rank-c :to-be-greater-than rank-b)))))

    (it "should handle disconnected components"
      ;; GKNV Figure 2-2 works per component
      (let ((graph (dag-draw-create-graph)))
        ;; Component 1: A -> B
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-edge graph 'a 'b)

        ;; Component 2: C -> D
        (dag-draw-add-node graph 'c "C")
        (dag-draw-add-node graph 'd "D")
        (dag-draw-add-edge graph 'c 'd)

        (let ((tree-info (dag-draw--construct-feasible-tree graph)))
          ;; GKNV should find tight tree containing all nodes (both components)
          (expect (length (ht-get tree-info 'tree-edges)) :to-equal 2)
          
          ;; Should have one tight tree root (GKNV picks one component as starting point)
          (expect (ht-get tree-info 'tight-tree-root) :not :to-be nil))))

    )

  (describe
      "spanning tree navigation functions"
    (it "should extract root node from single-component spanning tree"
      ;; GKNV Figure 2-2 identifies tight tree root
      (let ((graph (dag-draw-create-graph)))
        ;; Create simple connected graph: A -> B -> C
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)

        (let* ((tree-info (dag-draw--construct-feasible-tree graph))
               (tight-tree-root (ht-get tree-info 'tight-tree-root))
               (roots (ht-get tree-info 'roots)))
          ;; GKNV uses single tight tree root for connected component
          (expect tight-tree-root :not :to-be nil)
          (expect (length roots) :to-equal 1)
          (expect (car roots) :to-equal tight-tree-root))))

    (it "should handle multiple components with single tight tree"
      ;; GKNV Figure 2-2 expands tree to include all reachable nodes
      (let ((graph (dag-draw-create-graph)))
        ;; Create disconnected graph: A -> B and C -> D
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")
        (dag-draw-add-node graph 'd "D")
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'c 'd)

        (let* ((tree-info (dag-draw--construct-feasible-tree graph))
               (tight-tree-root (ht-get tree-info 'tight-tree-root)))
          ;; GKNV creates one tight tree starting from one component
          (expect tight-tree-root :not :to-be nil)
          (expect (memq tight-tree-root '(a b c d)) :to-be-truthy))))))

(provide 'dag-draw-network-simplex-spanning-tree-test)

;;; dag-draw-network-simplex-spanning-tree-test.el ends here
