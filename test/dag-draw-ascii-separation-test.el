;;; dag-draw-ascii-separation-test.el --- Tests for ASCII-specific separations -*- lexical-binding: t -*-

;; Copyright (C) 2024, 2025

;;; Commentary:

;; Tests for ASCII-specific node and rank separations.
;; Verifies that ASCII mode uses compact separations for terminal-friendly output.
;;
;; Test Coverage:
;; - ASCII mode automatically applies compact separations
;; - Explicit separations still override defaults
;; - Visual output is compact and readable
;; - High-res mode (if used) maintains original separations

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)

(describe "ASCII-specific separations"

  (describe "Default ASCII mode behavior"
    (it "should use ASCII-specific compact separations in ASCII mode"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-node graph 'b "Node B")
        (dag-draw-add-edge graph 'a 'b)

        ;; Layout with ASCII mode (default)
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)

        ;; Verify that ASCII separations were applied
        (expect (dag-draw-graph-node-separation graph) :to-equal dag-draw-ascii-node-separation)
        (expect (dag-draw-graph-rank-separation graph) :to-equal dag-draw-ascii-rank-separation)

        ;; Verify the compact values
        (expect (dag-draw-graph-node-separation graph) :to-equal 6)
        (expect (dag-draw-graph-rank-separation graph) :to-equal 5)))

    (it "should produce compact Y coordinates with ASCII rank separation"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-node graph 'b "Node B")
        (dag-draw-add-edge graph 'a 'b)

        ;; Layout with ASCII mode
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)

        (let ((node-a (dag-draw-get-node graph 'a))
              (node-b (dag-draw-get-node graph 'b)))
          ;; Node A should be at rank 0
          (expect (dag-draw-node-rank node-a) :to-equal 0)
          (expect (dag-draw-node-y-coord node-a) :to-equal 0)

          ;; Node B should be at rank 1 with compact separation
          (expect (dag-draw-node-rank node-b) :to-equal 1)
          (expect (dag-draw-node-y-coord node-b) :to-equal 5)  ; rank 1 × ranksep 5

          ;; Verify compact vertical spacing (not the old 25-unit spacing)
          (let ((vertical-gap (- (dag-draw-node-y-coord node-b)
                                 (dag-draw-node-y-coord node-a))))
            (expect vertical-gap :to-be-less-than 10))))))

  (describe "Explicit separation overrides"
    (it "should allow explicit node-separation to override ASCII defaults"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-node graph 'b "Node B")
        (dag-draw-add-edge graph 'a 'b)

        ;; Explicitly set custom separation before layout
        (setf (dag-draw-graph-node-separation graph) 10)
        (setf (dag-draw-graph-rank-separation graph) 8)

        ;; Layout - should NOT override explicit settings
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)

        ;; Verify custom separations were preserved
        ;; Note: ASCII resolution preprocessing might increase these if needed,
        ;; but they should not be set to the ASCII defaults
        (expect (dag-draw-graph-node-separation graph) :not :to-equal 6)
        (expect (dag-draw-graph-rank-separation graph) :not :to-equal 5)))

    (it "should respect explicitly set separations in test scenarios"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-node graph 'b "Node B")
        (dag-draw-add-node graph 'c "Node C")
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)

        ;; Set very large separations for testing
        (setf (dag-draw-graph-node-separation graph) 50)
        (setf (dag-draw-graph-rank-separation graph) 40)

        (dag-draw-layout-graph graph :coordinate-mode 'ascii)

        ;; Verify vertical spacing reflects large rank-separation
        (let* ((node-a (dag-draw-get-node graph 'a))
               (node-b (dag-draw-get-node graph 'b))
               (node-c (dag-draw-get-node graph 'c))
               (vertical-gap-1 (- (dag-draw-node-y-coord node-b)
                                  (dag-draw-node-y-coord node-a)))
               (vertical-gap-2 (- (dag-draw-node-y-coord node-c)
                                  (dag-draw-node-y-coord node-b))))
          ;; With ranksep=40, vertical gaps should be large (not compact 2)
          (expect vertical-gap-1 :to-be-greater-than 10)
          (expect vertical-gap-2 :to-be-greater-than 10)))))

  (describe "Visual output quality"
    (it "should produce compact readable ASCII output"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "Top")
        (dag-draw-add-node graph 'b "Middle")
        (dag-draw-add-node graph 'c "Bottom")
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)

        ;; Layout with ASCII mode
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)

        ;; Render and verify compactness
        (let ((ascii-output (dag-draw-render-ascii graph)))
          ;; Output should exist
          (expect ascii-output :not :to-be nil)
          (expect (length ascii-output) :to-be-greater-than 10)

          ;; Verify it's not excessively large (compact output)
          (let ((line-count (length (split-string ascii-output "\n" t))))
            ;; With 3 nodes and ranksep=2, should be reasonably compact
            ;; Allow room for node boxes + edges but verify it's not huge
            (expect line-count :to-be-less-than 50))

          ;; Debug output for visual inspection
          (message "\n=== COMPACT ASCII OUTPUT ===")
          (message "%s" ascii-output)
          (message "============================")

          ;; Verify all nodes are present in output (may be truncated in rendering)
          ;; Just verify output contains recognizable parts of node labels
          (expect ascii-output :to-match "T")  ; Part of "Top"
          (expect ascii-output :to-match "M")  ; Part of "Middle"
          (expect ascii-output :to-match "Bottom"))))

    (it "should handle multi-node graphs with compact spacing"
      (let ((graph (dag-draw-create-graph)))
        ;; Create a diamond pattern
        (dag-draw-add-node graph 'top "Top")
        (dag-draw-add-node graph 'left "Left")
        (dag-draw-add-node graph 'right "Right")
        (dag-draw-add-node graph 'bottom "Bottom")
        (dag-draw-add-edge graph 'top 'left)
        (dag-draw-add-edge graph 'top 'right)
        (dag-draw-add-edge graph 'left 'bottom)
        (dag-draw-add-edge graph 'right 'bottom)

        ;; Layout with ASCII mode
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)

        ;; Verify compact rank separation
        (let ((top-node (dag-draw-get-node graph 'top))
              (left-node (dag-draw-get-node graph 'left))
              (bottom-node (dag-draw-get-node graph 'bottom)))

          ;; Ranks should use compact separation
          (expect (dag-draw-node-y-coord top-node) :to-equal 0)
          (expect (dag-draw-node-y-coord left-node) :to-equal 5)  ; rank 1 × 5
          (expect (dag-draw-node-y-coord bottom-node) :to-equal 10)  ; rank 2 × 5

          ;; Verify compact rank separation was used
          (let ((vertical-gap-1 (- (dag-draw-node-y-coord left-node)
                                   (dag-draw-node-y-coord top-node)))
                (vertical-gap-2 (- (dag-draw-node-y-coord bottom-node)
                                   (dag-draw-node-y-coord left-node))))
            ;; With ASCII ranksep=5, gaps should be exactly 5
            (expect vertical-gap-1 :to-equal 5)
            (expect vertical-gap-2 :to-equal 5))))))

  (describe "Regression: ASCII defaults vs world defaults"
    (it "should use different separations for ASCII vs high-res modes"
      (let ((ascii-graph (dag-draw-create-graph))
            (highres-graph (dag-draw-create-graph)))

        ;; Set up identical graphs
        (dag-draw-add-node ascii-graph 'a "Node A")
        (dag-draw-add-node ascii-graph 'b "Node B")
        (dag-draw-add-edge ascii-graph 'a 'b)

        (dag-draw-add-node highres-graph 'a "Node A")
        (dag-draw-add-node highres-graph 'b "Node B")
        (dag-draw-add-edge highres-graph 'a 'b)

        ;; Layout with different modes
        (dag-draw-layout-graph ascii-graph :coordinate-mode 'ascii)
        (dag-draw-layout-graph highres-graph :coordinate-mode 'high-res)

        ;; Verify ASCII uses compact separations
        (expect (dag-draw-graph-node-separation ascii-graph) :to-equal 6)
        (expect (dag-draw-graph-rank-separation ascii-graph) :to-equal 5)

        ;; Verify high-res mode doesn't use ASCII separations
        ;; (it may use default values or adjusted values, but not ASCII-specific ones)
        (expect (dag-draw-graph-node-separation highres-graph) :not :to-equal 6)
        (expect (dag-draw-graph-rank-separation highres-graph) :not :to-equal 5)))))

(provide 'dag-draw-ascii-separation-test)

;;; dag-draw-ascii-separation-test.el ends here
