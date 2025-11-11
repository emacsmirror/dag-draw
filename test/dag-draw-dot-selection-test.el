;;; dag-draw-dot-selection-test.el --- Tests for DOT node selection rendering -*- lexical-binding: t; -*-

;; Tests for node selection visualization in DOT rendering

(add-to-list 'load-path (expand-file-name "test/helpers" (locate-dominating-file default-directory "Eldev")))

(require 'buttercup)
(require 'dag-draw)

(describe "DOT Rendering - Selected Node Parameter"
  (it "accepts selected parameter without error"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node-a "A")
      (expect (dag-draw-render-dot graph 'node-a) :not :to-throw)))

  (it "accepts nil selection"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node-a "A")
      (expect (dag-draw-render-dot graph nil) :not :to-throw)))

  (it "renders without selected parameter (backward compatibility)"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node-a "A")
      (expect (dag-draw-render-dot graph) :not :to-throw))))

(describe "DOT Rendering - Style Bold Attribute"
  (it "adds style=bold to selected node"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node-a "Selected")
      (dag-draw-add-node graph 'node-b "Normal")
      (dag-draw-add-edge graph 'node-a 'node-b)

      (let ((output (dag-draw-render-dot graph 'node-a)))
        ;; Selected node should have style=bold
        (expect output :to-match "node-a.*style=bold")
        ;; Unselected node should NOT have style=bold
        (expect output :not :to-match "node-b.*style=bold"))))

  (it "preserves label attribute when adding style"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node-a "My Label")

      (let ((output (dag-draw-render-dot graph 'node-a)))
        ;; Should have both label and style
        (expect output :to-match "node-a.*label=\"My Label\"")
        (expect output :to-match "node-a.*style=bold"))))

  (it "does not add style=bold when no selection"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node-a "A")
      (dag-draw-add-node graph 'node-b "B")

      (let ((output (dag-draw-render-dot graph nil)))
        ;; No nodes should have style=bold
        (expect output :not :to-match "style=bold")))))

(describe "DOT Rendering - Selection Edge Cases"
  (it "handles invalid node ID gracefully"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node-a "A")

      (let ((output (dag-draw-render-dot graph 'nonexistent)))
        ;; Should render without error, no style=bold
        (expect output :not :to-match "style=bold"))))

  (it "handles selection with empty graph"
    (let ((graph (dag-draw-create-graph)))
      (let ((output (dag-draw-render-dot graph 'some-node)))
        ;; Should render valid DOT structure
        (expect output :to-match "digraph G")
        (expect output :to-match "}")))))

(describe "DOT Rendering - Multiple Nodes"
  (it "only applies style=bold to selected node in multi-node graph"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node-a "A")
      (dag-draw-add-node graph 'node-b "B")
      (dag-draw-add-node graph 'node-c "C")
      (dag-draw-add-edge graph 'node-a 'node-b)
      (dag-draw-add-edge graph 'node-b 'node-c)

      (let ((output (dag-draw-render-dot graph 'node-b)))
        ;; Only node-b should have style=bold
        (expect output :to-match "node-b.*style=bold")
        ;; Count occurrences - should be exactly 1
        (let ((count (length (split-string output "style=bold" t))))
          (expect count :to-be 2))))))  ; split creates n+1 elements for n matches

(provide 'dag-draw-dot-selection-test)
;;; dag-draw-dot-selection-test.el ends here
