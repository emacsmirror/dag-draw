;;; dag-draw-svg-selection-test.el --- Tests for SVG node selection rendering -*- lexical-binding: t; -*-

;; Tests for node selection visualization in SVG rendering

(add-to-list 'load-path (expand-file-name "test/helpers" (locate-dominating-file default-directory "Eldev")))

(require 'buttercup)
(require 'dag-draw)

(describe "SVG Rendering - Selected Node Parameter"
  (it "accepts selected parameter without error"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node-a "A")
      (expect (dag-draw-render-svg graph 'node-a) :not :to-throw)))

  (it "accepts nil selection"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node-a "A")
      (expect (dag-draw-render-svg graph nil) :not :to-throw)))

  (it "renders without selected parameter (backward compatibility)"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node-a "A")
      (expect (dag-draw-render-svg graph) :not :to-throw))))

(describe "SVG Rendering - Filter Definition"
  (it "includes glow filter definition when node is selected"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node-a "A")
      (let ((output (dag-draw-render-svg graph 'node-a)))
        ;; Should contain filter definition
        (expect output :to-match "<filter")
        (expect output :to-match "id=\"selection-glow\"")
        (expect output :to-match "feGaussianBlur"))))

  (it "does not include filter when no selection"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node-a "A")
      (let ((output (dag-draw-render-svg graph nil)))
        ;; Should NOT contain selection filter
        (expect output :not :to-match "selection-glow")))))

(describe "SVG Rendering - Filter Application"
  (it "applies filter to selected node's rect element"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node-a "Selected")
      (dag-draw-add-node graph 'node-b "Normal")
      (dag-draw-add-edge graph 'node-a 'node-b)

      (let ((output (dag-draw-render-svg graph 'node-a)))
        ;; Check that output applies filter to a rect
        (expect output :to-match "filter=\"url(#selection-glow)\""))))

  (it "does not apply filter to unselected nodes"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node-a "Selected")
      (dag-draw-add-node graph 'node-b "Normal")

      (let ((output (dag-draw-render-svg graph 'node-a)))
        ;; Count filter applications - should be exactly 1
        (let ((filter-count (length (split-string output "filter=\"url(#selection-glow)\"" t))))
          (expect filter-count :to-be 2))))))  ; split creates n+1 elements for n matches

(describe "SVG Rendering - Selection Edge Cases"
  (it "handles invalid node ID gracefully"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node-a "A")
      (let ((output (dag-draw-render-svg graph 'nonexistent)))
        ;; Should render without error, no filter applied
        (expect output :not :to-match "filter=")
        (expect output :not :to-match "selection-glow"))))

  (it "handles selection with empty graph"
    (let ((graph (dag-draw-create-graph)))
      (let ((output (dag-draw-render-svg graph 'some-node)))
        ;; Should render SVG structure
        (expect output :to-match "<svg")
        (expect output :to-match "</svg>")))))

(provide 'dag-draw-svg-selection-test)
;;; dag-draw-svg-selection-test.el ends here
