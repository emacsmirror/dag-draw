;;; dag-draw-coordinate-mode-test.el --- Tests for coordinate-mode parameter -*- lexical-binding: t -*-

;; Copyright (C) 2024, 2025

;;; Commentary:

;; Tests for coordinate-mode parameter functionality.
;; Implements Phase 2 of ASCII-native GKNV implementation.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)

(describe "Coordinate-mode parameter"

  (describe "dag-draw-layout-graph with coordinate-mode"
    (it "should accept coordinate-mode parameter"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-node graph 'b "Node B")
        (dag-draw-add-edge graph 'a 'b)

        ;; This should not fail - coordinate-mode parameter should be accepted
        (expect (dag-draw-layout-graph graph :coordinate-mode 'ascii)
                :not :to-be nil)))

    (it "should operate in ASCII-native mode when coordinate-mode is 'ascii"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-node graph 'b "Node B")
        (dag-draw-add-edge graph 'a 'b)

        ;; Layout with ASCII coordinate mode
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)

        ;; In ASCII mode, coordinates should be integer-friendly for grid positioning
        (let ((node-a (dag-draw-get-node graph 'a))
              (node-b (dag-draw-get-node graph 'b)))
          ;; ASCII mode should produce coordinates that work well with grid conversion
          ;; Verify nodes have valid coordinates and proper Y ordering (B below A)
          (expect (dag-draw-node-x-coord node-a) :to-be-greater-than 0)
          (expect (dag-draw-node-y-coord node-a) :not :to-be nil) ; Node A has valid Y coordinate
          (expect (dag-draw-node-x-coord node-b) :to-be-greater-than 0)
          (expect (dag-draw-node-y-coord node-b) :to-be-greater-than (dag-draw-node-y-coord node-a)))))))  ; B should be below A

    ;; DELETED: High-res mode tests - obsolete in ASCII-first architecture
    ;; ASCII is now the only supported mode, high-res mode eliminated)

;;; dag-draw-coordinate-mode-test.el ends here
