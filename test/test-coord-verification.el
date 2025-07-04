;;; test-coord-verification.el --- Test coordinate verification  -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-render)
(require 'dag-draw-ascii-grid)

(describe "Progressive Coordinate Verification - Simple"

  (it "Level 1: Single Node - Coordinate Baseline"
    ;; Test: Establish baseline coordinate behavior
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node1 "Node1")
      (dag-draw-layout-graph graph)

      (let* ((raw-bounds (dag-draw-get-graph-bounds graph))
             (ascii-context (dag-draw--create-ascii-coordinate-context graph))
             (ascii-bounds (dag-draw--ascii-get-bounds ascii-context)))

        ;; Debug output
        (message "\n=== LEVEL 1: SINGLE NODE ===")
        (message "Raw bounds: %s" raw-bounds)
        (message "ASCII bounds: %s" ascii-bounds)

        ;; Single node should have minimal bounds
        (expect ascii-bounds :to-be-truthy))))

  (it "Level 2: Two Nodes - Basic Edge Tracking"
    ;; Test: Track coordinates through basic edge case
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node1 "Node1")
      (dag-draw-add-node graph 'node2 "Node2")
      (dag-draw-add-edge graph 'node1 'node2)
      (dag-draw-layout-graph graph)

      (let* ((raw-bounds (dag-draw-get-graph-bounds graph))
             (ascii-context (dag-draw--create-ascii-coordinate-context graph))
             (ascii-bounds (dag-draw--ascii-get-bounds ascii-context)))

        ;; Debug output
        (message "\n=== LEVEL 2: TWO NODES ===")
        (message "Raw bounds: %s" raw-bounds)
        (message "ASCII bounds: %s" ascii-bounds)

        ;; Two nodes should still have reasonable bounds
        (expect ascii-bounds :to-be-truthy)))))

(provide 'test-coord-verification)

;;; test-coord-verification.el ends here
