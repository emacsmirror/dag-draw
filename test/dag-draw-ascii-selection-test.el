;;; dag-draw-ascii-selection-test.el --- Tests for ASCII node selection rendering -*- lexical-binding: t; -*-

;; Tests for node selection visualization in ASCII rendering

(add-to-list 'load-path (expand-file-name "test/helpers" (locate-dominating-file default-directory "Eldev")))

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-test-harness)

(describe "ASCII Rendering - Selected Node Parameter"
  (it "accepts selected parameter without error"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node-a "A")
      (expect (dag-draw-render-ascii graph 'node-a) :not :to-throw)))

  (it "accepts nil selection"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node-a "A")
      (expect (dag-draw-render-ascii graph nil) :not :to-throw)))

  (it "renders without selected parameter (backward compatibility)"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node-a "A")
      (expect (dag-draw-render-ascii graph) :not :to-throw))))

(describe "ASCII Rendering - Single Node Selection"
  (it "renders selected node with double-line box characters"
    (let* ((graph (dag-draw-create-graph))
           (node-id 'node-a))
      (dag-draw-add-node graph node-id "Selected")

      (let ((output (dag-draw-render-ascii graph node-id)))
        ;; Check that output contains double-line box characters
        (expect output :to-match "╔")
        (expect output :to-match "╗")
        (expect output :to-match "╚")
        (expect output :to-match "╝")
        (expect output :to-match "═")
        (expect output :to-match "║")

        ;; Use test harness to verify node style
        ;; Note: find-nodes-using-graph-data returns text position, not box corner
        ;; Box corner is 1 position left and 1 position up from text
        (let* ((grid (dag-draw-test--parse-ascii-grid output))
               (nodes (dag-draw-test--find-nodes-using-graph-data output graph))
               (node (car nodes)))
          (expect (length nodes) :to-be 1)
          (let* ((text-x (plist-get node :x))
                 (text-y (plist-get node :y))
                 (box-x (1- text-x))  ; Box corner is 1 left of text
                 (box-y (1- text-y))  ; Box corner is 1 up from text
                 (style (dag-draw-test--get-node-box-style grid box-x box-y)))
            (expect style :to-equal 'double-line))))))

  (it "renders unselected node with single-line box characters"
    (let* ((graph (dag-draw-create-graph))
           (node-id 'node-a))
      (dag-draw-add-node graph node-id "Normal")

      ;; Render without selection (or with nil)
      (let ((output (dag-draw-render-ascii graph nil)))
        ;; Check that output contains single-line box characters
        (expect output :to-match "┌")
        (expect output :to-match "┐")
        (expect output :to-match "└")
        (expect output :to-match "┘")
        (expect output :to-match "─")
        (expect output :to-match "│")

        ;; Should NOT contain double-line characters
        (expect output :not :to-match "╔")
        (expect output :not :to-match "╗")
        (expect output :not :to-match "╚")
        (expect output :not :to-match "╝")

        ;; Use test harness to verify node style
        ;; Note: find-nodes-using-graph-data returns text position, not box corner
        ;; Box corner is 1 position left and 1 position up from text
        (let* ((grid (dag-draw-test--parse-ascii-grid output))
               (nodes (dag-draw-test--find-nodes-using-graph-data output graph))
               (node (car nodes)))
          (expect (length nodes) :to-be 1)
          (let* ((text-x (plist-get node :x))
                 (text-y (plist-get node :y))
                 (box-x (1- text-x))  ; Box corner is 1 left of text
                 (box-y (1- text-y))  ; Box corner is 1 up from text
                 (style (dag-draw-test--get-node-box-style grid box-x box-y)))
            (expect style :to-equal 'single-line)))))))

(describe "ASCII Rendering - Multiple Nodes with Mixed Selection"
  (it "renders only selected node with double-line style in multi-node graph"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node-a "A")
      (dag-draw-add-node graph 'node-b "B")
      (dag-draw-add-node graph 'node-c "C")
      (dag-draw-add-edge graph 'node-a 'node-b)
      (dag-draw-add-edge graph 'node-b 'node-c)

      (let ((output (dag-draw-render-ascii graph 'node-b)))
        ;; Output should contain both single and double-line characters
        (expect output :to-match "╔")  ; Double-line from selected node
        (expect output :to-match "┌")  ; Single-line from unselected nodes

        ;; Use validation helper to check all nodes
        (let* ((grid (dag-draw-test--parse-ascii-grid output))
               (nodes (dag-draw-test--find-nodes-using-graph-data output graph))
               (node-styles (mapcar (lambda (n)
                                     (let* ((text-x (plist-get n :x))
                                            (text-y (plist-get n :y))
                                            (box-x (1- text-x))
                                            (box-y (1- text-y)))
                                       (cons (plist-get n :id)
                                             (dag-draw-test--get-node-box-style grid box-x box-y))))
                                   nodes)))
          ;; Check that we found all 3 nodes
          (expect (length nodes) :to-be 3)

          ;; node-b should be double-line
          (expect (cdr (assq 'node-b node-styles)) :to-equal 'double-line)

          ;; node-a and node-c should be single-line
          (expect (cdr (assq 'node-a node-styles)) :to-equal 'single-line)
          (expect (cdr (assq 'node-c node-styles)) :to-equal 'single-line)))))

  (it "renders all nodes with single-line when no selection in multi-node graph"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node-a "A")
      (dag-draw-add-node graph 'node-b "B")
      (dag-draw-add-node graph 'node-c "C")
      (dag-draw-add-edge graph 'node-a 'node-b)
      (dag-draw-add-edge graph 'node-b 'node-c)

      (let ((output (dag-draw-render-ascii graph nil)))
        ;; Should only have single-line characters
        (expect output :to-match "┌")
        (expect output :not :to-match "╔")

        ;; Verify all nodes are single-line using validation helper
        (let* ((grid (dag-draw-test--parse-ascii-grid output))
               (nodes (dag-draw-test--find-nodes-using-graph-data output graph))
               (all-single-line-p
                (cl-every (lambda (n)
                           (let* ((text-x (plist-get n :x))
                                  (text-y (plist-get n :y))
                                  (box-x (1- text-x))
                                  (box-y (1- text-y))
                                  (style (dag-draw-test--get-node-box-style grid box-x box-y)))
                             (eq style 'single-line)))
                         nodes)))
          (expect (length nodes) :to-be 3)
          (expect all-single-line-p :to-be t))))))

(describe "ASCII Rendering - Selection Edge Cases"
  (it "handles invalid node ID gracefully"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node-a "A")

      ;; Select a node that doesn't exist
      (let ((output (dag-draw-render-ascii graph 'nonexistent-node)))
        ;; Should render without error, all nodes single-line
        (expect output :to-match "┌")
        (expect output :not :to-match "╔")

        (let* ((grid (dag-draw-test--parse-ascii-grid output))
               (nodes (dag-draw-test--find-nodes-using-graph-data output graph))
               (node (car nodes)))
          (let* ((text-x (plist-get node :x))
                 (text-y (plist-get node :y))
                 (box-x (1- text-x))
                 (box-y (1- text-y))
                 (style (dag-draw-test--get-node-box-style grid box-x box-y)))
            (expect style :to-equal 'single-line))))))

  (it "handles selection with empty graph"
    (let ((graph (dag-draw-create-graph)))
      ;; Try to select a node in an empty graph
      (let ((output (dag-draw-render-ascii graph 'some-node)))
        ;; Should return empty graph message
        (expect output :to-match "Empty Graph"))))

  (it "handles nil selection explicitly"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node-a "A")
      (dag-draw-add-node graph 'node-b "B")

      ;; Explicitly pass nil as selection
      (let ((output (dag-draw-render-ascii graph nil)))
        ;; All nodes should be single-line
        (expect output :to-match "┌")
        (expect output :not :to-match "╔")

        (let* ((grid (dag-draw-test--parse-ascii-grid output))
               (nodes (dag-draw-test--find-nodes-using-graph-data output graph))
               (all-single (cl-every
                           (lambda (n)
                             (let* ((text-x (plist-get n :x))
                                    (text-y (plist-get n :y))
                                    (box-x (1- text-x))
                                    (box-y (1- text-y)))
                               (eq (dag-draw-test--get-node-box-style grid box-x box-y)
                                   'single-line)))
                           nodes)))
          (expect all-single :to-be t))))))

(describe "ASCII Rendering - Junction Characters with Selection"
  (it "maintains correct junction characters at selected node boundaries"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node-a "A")
      (dag-draw-add-node graph 'node-b "B")
      (dag-draw-add-edge graph 'node-a 'node-b)

      (let ((output (dag-draw-render-ascii graph 'node-a)))
        ;; Selected node should have double-line box
        (expect output :to-match "╔")

        ;; Junction validation should pass
        (let* ((grid (dag-draw-test--parse-ascii-grid output))
               (validation (dag-draw-test--validate-junction-connectivity grid)))
          (expect (plist-get validation :all-valid) :to-be t)
          (when (not (plist-get validation :all-valid))
            (message "Junction validation issues: %S"
                     (plist-get validation :invalid-junctions)))))))

  (it "maintains correct edge connectivity with selected nodes"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node-a "A")
      (dag-draw-add-node graph 'node-b "B")
      (dag-draw-add-node graph 'node-c "C")
      (dag-draw-add-edge graph 'node-a 'node-b)
      (dag-draw-add-edge graph 'node-b 'node-c)

      (let ((output (dag-draw-render-ascii graph 'node-b)))
        ;; Validate edge connectivity
        (let ((validation (dag-draw-test--validate-edge-connectivity output graph)))
          (expect (plist-get validation :all-connected) :to-be t)
          (when (not (plist-get validation :all-connected))
            (message "Edge connectivity issues: %S"
                     (plist-get validation :disconnected-edges)))))))

  (it "maintains correct arrow placement with selected nodes"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'node-a "A")
      (dag-draw-add-node graph 'node-b "B")
      (dag-draw-add-edge graph 'node-a 'node-b)

      (let ((output (dag-draw-render-ascii graph 'node-b)))
        ;; Validate arrow placement
        (let ((validation (dag-draw-test--validate-arrows output)))
          (expect (plist-get validation :invalid-arrows) :to-equal 0)
          (when (> (plist-get validation :invalid-arrows) 0)
            (message "Arrow validation issues: %S"
                     (plist-get validation :invalid-contexts))))))))

(provide 'dag-draw-ascii-selection-test)
;;; dag-draw-ascii-selection-test.el ends here
