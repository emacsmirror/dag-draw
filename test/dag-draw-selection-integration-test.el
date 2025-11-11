;;; dag-draw-selection-integration-test.el --- Integration tests for node selection across formats -*- lexical-binding: t; -*-

;; Integration tests verifying node selection works consistently across all rendering formats

(add-to-list 'load-path (expand-file-name "test/helpers" (locate-dominating-file default-directory "Eldev")))

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-test-harness)

(describe "Node Selection Integration - Cross-Format Consistency"
  (it "renders same graph with selection consistently across all formats"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node-a "A")
      (dag-draw-add-node graph 'node-b "B")
      (dag-draw-add-node graph 'node-c "C")
      (dag-draw-add-edge graph 'node-a 'node-b)
      (dag-draw-add-edge graph 'node-b 'node-c)

      ;; Render with selection in all formats
      (let ((ascii-out (dag-draw-render-ascii graph 'node-b))
            (svg-out (dag-draw-render-svg graph 'node-b))
            (dot-out (dag-draw-render-dot graph 'node-b)))

        ;; ASCII should have double-line for node-b
        (expect ascii-out :to-match "╔")
        (expect ascii-out :to-match "┌")  ; Also has single-line for other nodes

        ;; SVG should have filter definition and application
        (expect svg-out :to-match "selection-glow")
        (expect svg-out :to-match "filter=\"url")

        ;; DOT should have style=bold for node-b
        (expect dot-out :to-match "node-b.*style=bold")
        (expect dot-out :not :to-match "node-a.*style=bold")
        (expect dot-out :not :to-match "node-c.*style=bold"))))

  (it "handles nil selection consistently across all formats"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node-a "A")
      (dag-draw-add-node graph 'node-b "B")

      (let ((ascii-out (dag-draw-render-ascii graph nil))
            (svg-out (dag-draw-render-svg graph nil))
            (dot-out (dag-draw-render-dot graph nil)))

        ;; ASCII should only have single-line boxes
        (expect ascii-out :to-match "┌")
        (expect ascii-out :not :to-match "╔")

        ;; SVG should not have selection filter
        (expect svg-out :not :to-match "selection-glow")

        ;; DOT should not have style=bold
        (expect dot-out :not :to-match "style=bold"))))

  (it "handles invalid node ID consistently across all formats"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node-a "A")

      (let ((ascii-out (dag-draw-render-ascii graph 'nonexistent))
            (svg-out (dag-draw-render-svg graph 'nonexistent))
            (dot-out (dag-draw-render-dot graph 'nonexistent)))

        ;; All formats should render normally without selection
        (expect ascii-out :not :to-match "╔")
        (expect svg-out :not :to-match "selection-glow")
        (expect dot-out :not :to-match "style=bold")))))

(describe "Node Selection Integration - Complex Graphs"
  (it "handles selection in complex multi-level DAG"
    (let ((graph (dag-draw-create-graph)))
      ;; Create a more complex graph
      (dag-draw-add-node graph 'root "Root")
      (dag-draw-add-node graph 'left "Left")
      (dag-draw-add-node graph 'right "Right")
      (dag-draw-add-node graph 'merge "Merge")
      (dag-draw-add-edge graph 'root 'left)
      (dag-draw-add-edge graph 'root 'right)
      (dag-draw-add-edge graph 'left 'merge)
      (dag-draw-add-edge graph 'right 'merge)

      ;; Select the merge node
      (let ((ascii-out (dag-draw-render-ascii graph 'merge))
            (svg-out (dag-draw-render-svg graph 'merge))
            (dot-out (dag-draw-render-dot graph 'merge)))

        ;; All formats should show merge as selected
        (expect ascii-out :to-match "╔")
        (expect svg-out :to-match "selection-glow")
        (expect dot-out :to-match "merge.*style=bold")

        ;; ASCII: Verify edges and junctions still work
        (let ((validation (dag-draw-test--validate-edge-connectivity ascii-out graph)))
          (expect (plist-get validation :all-connected) :to-be t)))))

  (it "maintains graph structure integrity with selection"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'a "A")
      (dag-draw-add-node graph 'b "B")
      (dag-draw-add-node graph 'c "C")
      (dag-draw-add-node graph 'd "D")
      (dag-draw-add-edge graph 'a 'b)
      (dag-draw-add-edge graph 'a 'c)
      (dag-draw-add-edge graph 'b 'd)
      (dag-draw-add-edge graph 'c 'd)

      (let ((ascii-out (dag-draw-render-ascii graph 'b)))
        ;; Verify all 4 nodes are present
        (let ((nodes (dag-draw-test--find-nodes-using-graph-data ascii-out graph)))
          (expect (length nodes) :to-be 4))

        ;; Verify selected node has correct style
        (let* ((grid (dag-draw-test--parse-ascii-grid ascii-out))
               (nodes (dag-draw-test--find-nodes-using-graph-data ascii-out graph))
               (node-b (cl-find-if (lambda (n) (eq (plist-get n :id) 'b)) nodes)))
          (when node-b
            (let* ((text-x (plist-get node-b :x))
                   (text-y (plist-get node-b :y))
                   (box-x (1- text-x))
                   (box-y (1- text-y))
                   (style (dag-draw-test--get-node-box-style grid box-x box-y)))
              (expect style :to-equal 'double-line))))))))

(describe "Node Selection Integration - End-to-End Workflow"
  (it "supports complete workflow from graph creation to rendering with selection"
    (let ((graph (dag-draw-create-graph)))
      ;; Build graph
      (dag-draw-add-node graph 'start "Start")
      (dag-draw-add-node graph 'process "Process")
      (dag-draw-add-node graph 'end "End")
      (dag-draw-add-edge graph 'start 'process)
      (dag-draw-add-edge graph 'process 'end)

      ;; Render in all formats with selection
      (expect (dag-draw-render-ascii graph 'process) :not :to-throw)
      (expect (dag-draw-render-svg graph 'process) :not :to-throw)
      (expect (dag-draw-render-dot graph 'process) :not :to-throw)

      ;; Verify outputs are non-empty
      (expect (length (dag-draw-render-ascii graph 'process)) :to-be-greater-than 0)
      (expect (length (dag-draw-render-svg graph 'process)) :to-be-greater-than 0)
      (expect (length (dag-draw-render-dot graph 'process)) :to-be-greater-than 0))))

(provide 'dag-draw-selection-integration-test)
;;; dag-draw-selection-integration-test.el ends here
