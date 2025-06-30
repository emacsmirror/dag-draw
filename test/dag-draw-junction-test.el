;;; dag-draw-junction-test.el --- Test junction character issues -*- lexical-binding: t -*-

(require 'buttercup)
(require 'dag-draw-render)

(describe "Junction Character Issues"
  (it "should not create floating junction characters"
    ;; Test the specific issue: ┼───────────◀┼ and ─┼     ──────┼
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'source "Source")
      (dag-draw-add-node graph 'target-a "Target A")  
      (dag-draw-add-node graph 'target-b "Target B")
      (dag-draw-add-edge graph 'source 'target-a)
      (dag-draw-add-edge graph 'source 'target-b)
      (dag-draw-layout-graph graph)
      
      (let ((ascii-output (dag-draw-render-ascii graph)))
        (message "=== JUNCTION TEST ===")
        (message "%s" ascii-output)
        
        ;; Should NOT have floating junction characters
        (expect ascii-output :not :to-match "┼───────────◀┼")
        (expect ascii-output :not :to-match "─┼     ──────┼")
        (expect ascii-output :not :to-match "┼│")
        
        ;; Should have all nodes visible
        (expect ascii-output :to-match "Source")
        (expect ascii-output :to-match "Target A")
        (expect ascii-output :to-match "Target B")
        
        (message "==================")))))

(provide 'dag-draw-junction-test)