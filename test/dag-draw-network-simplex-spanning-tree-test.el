;;; dag-draw-network-simplex-spanning-tree-test.el --- TDD tests for network simplex spanning tree -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

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
      ;; RED phase: This test will fail because feasible spanning tree doesn't exist yet
      (let ((graph (dag-draw-create-graph)))
        ;; Create simple graph: A -> B -> C
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)

        ;; The spanning tree should be created from the graph structure
        (let ((spanning-tree (dag-draw--create-feasible-spanning-tree graph)))
          ;; Tree should have exactly 2 edges for 3 nodes
          (expect (length (dag-draw-spanning-tree-edges spanning-tree)) :to-equal 2)

          ;; Tree should connect all nodes
          (expect (length (dag-draw-spanning-tree-nodes spanning-tree)) :to-equal 3)

          ;; Tree should have parent-child relationships
          (let ((roots (dag-draw-spanning-tree-roots spanning-tree)))
            (expect roots :to-be-truthy)
            (expect (> (length roots) 0) :to-be t)
            ;; Check that roots have no parents
            (let ((parent-map (dag-draw-spanning-tree-parent spanning-tree))
                  (root (car roots)))
              (expect (ht-get parent-map root) :to-be nil))))))

    (it "should create feasible ranking from spanning tree"
      ;; RED phase: This test will fail because ranking from spanning tree doesn't exist yet
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")
        (dag-draw-add-edge graph 'a 'b)  ; weight=1, min-length=1
        (dag-draw-add-edge graph 'b 'c)  ; weight=1, min-length=1

        (let ((spanning-tree (dag-draw--create-feasible-spanning-tree graph)))
          ;; Generate ranking from spanning tree
          (let ((ranking (dag-draw--spanning-tree-to-ranking graph spanning-tree)))
            ;; All nodes should have ranks
            (expect (ht-get ranking 'a) :to-be-truthy)
            (expect (ht-get ranking 'b) :to-be-truthy)
            (expect (ht-get ranking 'c) :to-be-truthy)

            ;; Ranking should respect edge constraints (rank(target) >= rank(source) + min-length)
            (expect (>= (ht-get ranking 'b) (+ (ht-get ranking 'a) 1)) :to-be t)
            (expect (>= (ht-get ranking 'c) (+ (ht-get ranking 'b) 1)) :to-be t)))))

    (it "should handle disconnected components"
      ;; RED phase: This test will fail because multi-component spanning tree doesn't exist yet
      (let ((graph (dag-draw-create-graph)))
        ;; Component 1: A -> B
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-edge graph 'a 'b)

        ;; Component 2: C -> D
        (dag-draw-add-node graph 'c "C")
        (dag-draw-add-node graph 'd "D")
        (dag-draw-add-edge graph 'c 'd)

        (let ((spanning-tree (dag-draw--create-feasible-spanning-tree graph)))
          ;; Should handle both components
          (expect (length (dag-draw-spanning-tree-nodes spanning-tree)) :to-equal 4)
          (expect (length (dag-draw-spanning-tree-edges spanning-tree)) :to-equal 2)

          ;; Should have multiple roots (one per component)
          (let ((roots (dag-draw-spanning-tree-roots spanning-tree)))
            (expect (length roots) :to-equal 2)))))

    )

  (describe
      "spanning tree navigation functions"
    (it "should extract root node from single-component spanning tree"
      ;; RED phase: This test will fail because dag-draw-spanning-tree-root doesn't exist yet
      (let ((graph (dag-draw-create-graph)))
        ;; Create simple connected graph: A -> B -> C
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)

        (let* ((spanning-tree (dag-draw--create-feasible-spanning-tree graph))
               (roots (dag-draw-spanning-tree-roots spanning-tree)))
          ;; Should have single root node for connected graph
          (expect (length roots) :to-equal 1)
          (let ((root (car roots)))
            (expect root :to-be-truthy)
            (expect (memq root '(a b c)) :to-be-truthy)))))

    (it "should handle multiple roots in disconnected components"
      ;; RED phase: Test for multiple component case
      (let ((graph (dag-draw-create-graph)))
        ;; Create disconnected graph: A -> B and C -> D
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")
        (dag-draw-add-node graph 'd "D")
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'c 'd)

        (let* ((spanning-tree (dag-draw--create-feasible-spanning-tree graph))
               (roots (dag-draw-spanning-tree-roots spanning-tree)))
          ;; Should have multiple roots for disconnected components
          (expect (listp roots) :to-be t)
          (expect (length roots) :to-equal 2))))))

(provide 'dag-draw-network-simplex-spanning-tree-test)

;;; dag-draw-network-simplex-spanning-tree-test.el ends here
